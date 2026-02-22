-module(gp_store_client).

-export([
    append_event/3,
    get_event/2,
    list_events/2,
    enqueue_job/1,
    claim_jobs/2,
    ack_job/1,
    nack_job/2,
    put_dlq/1,
    list_dlq/1,
    requeue_dlq/1,
    delete_dlq/1,
    append_attempt/1,
    list_attempts/1,
    put_endpoint/1,
    get_endpoint/1,
    list_endpoints/1,
    delete_endpoint/1,
    put_subscription/1,
    list_subscriptions/1,
    delete_subscription/1,
    compact/1,
    queue_stats/1,
    append_audit/3,
    list_audit/2
]).

call(Cmd, Args) ->
    Frame = gp_store_protocol:encode(Cmd, Args),
    case gp_store_pool:call(Frame) of
        #{<<"ok">> := true}  = Resp -> {ok, Resp};
        #{<<"ok">> := false} = Resp ->
            Err = maps:get(<<"error">>, Resp, <<"unknown">>),
            logger:warning(#{event => store_error, cmd => Cmd, error => Err}),
            gp_delivery_metrics:inc_store_op(Cmd, <<"error">>),
            {error, Err};
        {error, _} = E ->
            logger:error(#{event => store_error, cmd => Cmd, reason => E}),
            gp_delivery_metrics:inc_store_op(Cmd, <<"error">>),
            E
    end.

append_event(EventId, TenantId, Payload) ->
    call(<<"store.append_event">>, #{
        <<"event_id">>  => EventId,
        <<"tenant_id">> => TenantId,
        <<"payload">>   => Payload
    }).

get_event(TenantId, EventId) ->
    call(<<"store.get_event">>, #{
        <<"tenant_id">> => TenantId,
        <<"event_id">>  => EventId
    }).

%% Opts: #{tenant_id, after_id, cursor, limit, from_ms, to_ms}
list_events(TenantId, Opts) when is_map(Opts) ->
    call(<<"store.list_events">>, Opts#{<<"tenant_id">> => TenantId}).

enqueue_job(Job) when is_map(Job) ->
    call(<<"store.enqueue_job">>, Job).

claim_jobs(MaxCount, LeaseSecs) ->
    call(<<"store.claim_jobs">>, #{
        <<"max_count">>  => MaxCount,
        <<"lease_secs">> => LeaseSecs
    }).

ack_job(JobId) ->
    call(<<"store.ack_job">>, #{<<"job_id">> => JobId}).

nack_job(JobId, DelaySecs) ->
    call(<<"store.nack_job">>, #{
        <<"job_id">>     => JobId,
        <<"delay_secs">> => DelaySecs
    }).

put_dlq(Entry) when is_map(Entry) ->
    call(<<"store.put_dlq">>, Entry).

list_dlq(TenantId) ->
    call(<<"store.list_dlq">>, #{<<"tenant_id">> => TenantId}).

requeue_dlq(JobId) ->
    call(<<"store.requeue_dlq">>, #{<<"job_id">> => JobId}).

delete_dlq(JobId) ->
    call(<<"store.delete_dlq">>, #{<<"job_id">> => JobId}).

append_attempt(Attempt) when is_map(Attempt) ->
    call(<<"store.append_attempt">>, Attempt).

%% Opts: #{job_id, event_id, endpoint_id, limit}
list_attempts(Opts) when is_map(Opts) ->
    call(<<"store.list_attempts">>, Opts).

put_endpoint(Endpoint) when is_map(Endpoint) ->
    call(<<"store.put_endpoint">>, Endpoint).

get_endpoint(EndpointId) ->
    call(<<"store.get_endpoint">>, #{<<"endpoint_id">> => EndpointId}).

list_endpoints(TenantId) ->
    call(<<"store.list_endpoints">>, #{<<"tenant_id">> => TenantId}).

delete_endpoint(EndpointId) ->
    call(<<"store.delete_endpoint">>, #{<<"endpoint_id">> => EndpointId}).

put_subscription(Sub) when is_map(Sub) ->
    call(<<"store.put_subscription">>, Sub).

list_subscriptions(TenantId) ->
    call(<<"store.list_subscriptions">>, #{<<"tenant_id">> => TenantId}).

delete_subscription(SubscriptionId) ->
    call(<<"store.delete_subscription">>, #{<<"subscription_id">> => SubscriptionId}).

compact(RetentionSecs) when is_integer(RetentionSecs) ->
    call(<<"store.compact">>, #{<<"retention_secs">> => RetentionSecs}).

queue_stats(TenantId) ->
    call(<<"store.queue_stats">>, #{<<"tenant_id">> => TenantId}).

append_audit(TenantId, Action, Metadata) ->
    call(<<"store.append_audit">>, #{
        <<"tenant_id">> => TenantId,
        <<"action">>    => Action,
        <<"metadata">>  => Metadata,
        <<"ts">>        => erlang:system_time(millisecond)
    }).

list_audit(TenantId, Opts) when is_map(Opts) ->
    call(<<"store.list_audit">>, Opts#{<<"tenant_id">> => TenantId}).
