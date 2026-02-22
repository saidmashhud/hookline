-module(gp_delivery_worker).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(GUN_TIMEOUT, 30000).
%% HTTP status codes that are non-retryable (go straight to DLQ)
-define(NON_RETRYABLE, [400, 401, 403, 404, 405, 410, 422]).

-record(state, {job :: map() | undefined}).

start_link() ->
    gen_server:start_link(?MODULE, [undefined], []).

start_link(Job) when is_map(Job) ->
    gen_server:start_link(?MODULE, [Job], []).

init([undefined]) -> {ok, #state{}};
init([Job]) ->
    self() ! deliver,
    {ok, #state{job = Job}}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(deliver, #state{job = Job} = State) when Job =/= undefined ->
    deliver(Job),
    {stop, normal, State};
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ── Main delivery pipeline ─────────────────────────────────────────────────

deliver(Job) ->
    JobId      = maps:get(<<"job_id">>,      Job),
    EventId    = maps:get(<<"event_id">>,    Job),
    EndpointId = maps:get(<<"endpoint_id">>, Job),
    TenantId   = maps:get(<<"tenant_id">>,   Job),
    AttemptN   = maps:get(<<"attempt_count">>, Job, 1),
    MaxAttempts = maps:get(<<"max_attempts">>, Job, gp_core_retry:max_attempts()),

    logger:info(#{event => delivery_attempt_started,
                  job_id => JobId, event_id => EventId,
                  endpoint_id => EndpointId, tenant_id => TenantId,
                  attempt_n => AttemptN}),

    case fetch_event_and_endpoint(TenantId, EventId, EndpointId) of
        {ok, Event, Endpoint} ->
            Enabled     = maps:get(<<"enabled">>,        Endpoint, true),
            URL         = maps:get(<<"url">>,             Endpoint),
            Secret      = maps:get(<<"secret">>,          Endpoint, <<>>),
            MaxInFlight = maps:get(<<"max_in_flight">>,   Endpoint, 10),
            RateRPS     = maps:get(<<"rate_limit_rps">>,  Endpoint, 100),
            TimeoutMs   = maps:get(<<"timeout_ms">>,      Endpoint, ?GUN_TIMEOUT),
            EpHeaders   = endpoint_headers(maps:get(<<"headers">>, Endpoint, #{})),

            case Enabled of
                false ->
                    logger:info(#{event => delivery_endpoint_disabled,
                                  job_id => JobId, endpoint_id => EndpointId}),
                    gp_store_client:nack_job(JobId, 300);
                _ ->
            Topic     = maps:get(<<"topic">>, Event, <<>>),
            AttemptId = gp_core_uuid:generate_str(),
            StartMs   = erlang:system_time(millisecond),

            %% Check rate limit (uses per-endpoint RPS from config)
            case gp_delivery_rate:check(EndpointId, RateRPS) of
                {error, rate_limited} ->
                    logger:warning(#{event => delivery_rate_limited,
                                     job_id => JobId, endpoint_id => EndpointId}),
                    gp_delivery_metrics:inc_rate_limited(EndpointId),
                    gp_store_client:nack_job(JobId, 5);

                ok ->
                    %% Acquire semaphore (max_in_flight)
                    case gp_delivery_sem:acquire(EndpointId, MaxInFlight) of
                        {error, at_capacity} ->
                            logger:info(#{event => delivery_at_capacity,
                                          job_id => JobId, endpoint_id => EndpointId}),
                            gp_store_client:nack_job(JobId, 1);

                        ok ->
                            Transform  = maps:get(<<"transform">>, Job, null),
                            FinalEvent = gp_core_transform:apply(Transform, Event),
                            PayloadBin = build_payload(FinalEvent, Job, AttemptId),
                            Result = try
                                do_http_post(URL, Secret, PayloadBin,
                                             EventId, Topic, AttemptId, TenantId,
                                             TimeoutMs, EpHeaders)
                            catch
                                _:Err -> {error, Err}
                            end,
                            gp_delivery_sem:release(EndpointId),
                            Duration = erlang:system_time(millisecond) - StartMs,
                            record_attempt(AttemptId, JobId, EventId, EndpointId,
                                           TenantId, AttemptN, StartMs, Duration, Result),
                            publish_delivery_status(TenantId, AttemptId, JobId,
                                                    EventId, EndpointId, AttemptN,
                                                    Duration, Result),
                            handle_result(JobId, EventId, EndpointId, TenantId,
                                          AttemptN, MaxAttempts, Result)
                    end
            end
            end;

        {error, Reason} ->
            logger:error(#{event => delivery_fetch_failed,
                           job_id => JobId, event_id => EventId,
                           endpoint_id => EndpointId, reason => Reason}),
            dlq_job(JobId, EventId, EndpointId, TenantId, AttemptN,
                    <<"failed to fetch event or endpoint">>)
    end.

fetch_event_and_endpoint(TenantId, EventId, EndpointId) ->
    case gp_store_client:get_event(TenantId, EventId) of
        {ok, #{<<"event">> := EventRaw}} ->
            case gp_store_client:get_endpoint(EndpointId) of
                {ok, #{<<"endpoint">> := EpRaw}} ->
                    Event    = decode_json(EventRaw),
                    Endpoint = decode_json(EpRaw),
                    {ok, Event, Endpoint};
                Err -> Err
            end;
        Err -> Err
    end.

decode_json(V) when is_binary(V) -> jsx:decode(V, [return_maps]);
decode_json(V) when is_map(V)    -> V.

build_payload(Event, Job, AttemptId) ->
    jsx:encode(#{
        <<"gatepulse_version">> => <<"1">>,
        <<"job_id">>            => maps:get(<<"job_id">>,     Job),
        <<"event_id">>          => maps:get(<<"event_id">>,   Job),
        <<"attempt_id">>        => AttemptId,
        <<"attempt">>           => maps:get(<<"attempt_count">>, Job, 1),
        <<"event">>             => Event
    }).

do_http_post(URL, Secret, PayloadBin, EventId, Topic, AttemptId, _TenantId,
             TimeoutMs, EpHeaders) ->
    Ts = erlang:system_time(millisecond),
    TsBin = integer_to_binary(Ts),

    SigHeader = case Secret of
        <<>> -> undefined;
        S    -> gp_core_signature:header_value(S, Ts, PayloadBin)
    end,

    Headers = [
        {<<"content-type">>,     <<"application/json">>},
        {<<"user-agent">>,       <<"GatePulse/0.1.0">>},
        {<<"x-gp-event-id">>,    EventId},
        {<<"x-gp-topic">>,       Topic},
        {<<"x-gp-delivery-id">>, AttemptId},
        {<<"x-gp-timestamp">>,   TsBin}
        | case SigHeader of
              undefined -> [];
              Sig -> [{<<"x-gp-signature">>, Sig}]
          end
    ] ++ EpHeaders,
    http_post(URL, PayloadBin, Headers, TimeoutMs).

http_post(URL, Body, Headers, TimeoutMs) ->
    URLStr = binary_to_list(URL),
    {Scheme, Host, Port, Path} = parse_url(URLStr),
    ConnOpts = case Scheme of
        https -> #{transport => tls};
        _     -> #{}
    end,
    ConnTimeout = min(TimeoutMs, 10000),
    case gun:open(Host, Port, ConnOpts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, ConnTimeout) of
                {ok, _} ->
                    StreamRef = gun:post(ConnPid, Path, Headers, Body),
                    Resp = case gun:await(ConnPid, StreamRef, TimeoutMs) of
                        {response, fin, Status, _RH} ->
                            {ok, Status, <<>>};
                        {response, nofin, Status, _RH} ->
                            {ok, RB} = gun:await_body(ConnPid, StreamRef, TimeoutMs),
                            {ok, Status, RB};
                        {error, R} ->
                            {error, R}
                    end,
                    gun:close(ConnPid),
                    Resp;
                {error, R} ->
                    gun:close(ConnPid),
                    {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

%% Convert endpoint headers map #{<<"Name">> => <<"Value">>} to gun header list
endpoint_headers(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{K, V} | Acc]
    end, [], Map);
endpoint_headers(_) -> [].

parse_url(URL) ->
    URLStr = case URL of
        B when is_binary(B) -> binary_to_list(B);
        L -> L
    end,
    Map    = uri_string:parse(URLStr),
    Scheme = list_to_atom(maps:get(scheme, Map, "http")),
    Host   = maps:get(host, Map, "localhost"),
    Port   = case maps:get(port, Map, 0) of
        0 -> case Scheme of https -> 443; _ -> 80 end;
        P -> P
    end,
    Path   = case maps:get(path, Map, "") of
        ""  -> "/";
        P2  -> P2
    end,
    {Scheme, Host, Port, Path}.

record_attempt(AttemptId, JobId, EventId, EndpointId, TenantId, N,
               StartMs, Duration, Result) ->
    {Status, RespCode, Err} = case Result of
        {ok, Code, _} -> {<<"success">>, Code, null};
        {error, R}    ->
            Msg = list_to_binary(io_lib:format("~p", [R])),
            {<<"failure">>, 0, Msg}
    end,
    logger:info(#{event => delivery_attempt_finished,
                  attempt_id => AttemptId, job_id => JobId,
                  event_id => EventId, endpoint_id => EndpointId,
                  tenant_id => TenantId, attempt_n => N,
                  status => Status, http_status => RespCode,
                  duration_ms => Duration, error => Err}),
    gp_delivery_metrics:observe_latency(EndpointId, Duration),
    gp_store_client:append_attempt(#{
        <<"attempt_id">>   => AttemptId,
        <<"job_id">>       => JobId,
        <<"event_id">>     => EventId,
        <<"endpoint_id">>  => EndpointId,
        <<"tenant_id">>    => TenantId,
        <<"attempt_n">>    => N,
        <<"status">>       => Status,
        <<"http_status">>  => RespCode,
        <<"error">>        => Err,
        <<"started_at">>   => StartMs,
        <<"duration_ms">>  => Duration
    }).

publish_delivery_status(TenantId, AttemptId, JobId, EventId, EndpointId,
                        AttemptN, Duration, Result) ->
    {Status, HttpStatus} = case Result of
        {ok, Code, _} when Code >= 200, Code < 300 -> {<<"success">>, Code};
        {ok, Code, _}                               -> {<<"failed">>, Code};
        {error, _}                                  -> {<<"failed">>, 0}
    end,
    gp_stream_pubsub:publish(TenantId, #{
        <<"topic">>       => <<"deliveries.status">>,
        <<"attempt_id">>  => AttemptId,
        <<"job_id">>      => JobId,
        <<"event_id">>    => EventId,
        <<"endpoint_id">> => EndpointId,
        <<"attempt_n">>   => AttemptN,
        <<"status">>      => Status,
        <<"http_status">> => HttpStatus,
        <<"latency_ms">>  => Duration,
        <<"ts">>          => erlang:system_time(millisecond)
    }).

handle_result(JobId, EventId, EndpointId, TenantId, AttemptN, MaxAttempts, Result) ->
    case Result of
        {ok, Status, _} when Status >= 200, Status < 300 ->
            gp_store_client:ack_job(JobId),
            gp_delivery_metrics:inc_delivered(TenantId, EndpointId);

        {ok, Status, _} when Status =:= 408; Status =:= 429 ->
            retry_job(JobId, EventId, EndpointId, TenantId, AttemptN, MaxAttempts);

        {ok, Status, _} ->
            case lists:member(Status, ?NON_RETRYABLE) of
                true ->
                    logger:warning(#{event => delivery_dlq,
                                     job_id => JobId, http_status => Status,
                                     endpoint_id => EndpointId}),
                    dlq_job(JobId, EventId, EndpointId, TenantId, AttemptN,
                            iolist_to_binary(["http_", integer_to_binary(Status)]));
                false ->
                    retry_job(JobId, EventId, EndpointId, TenantId, AttemptN, MaxAttempts)
            end;

        {error, Reason} ->
            logger:warning(#{event => delivery_network_error,
                             job_id => JobId, reason => Reason,
                             endpoint_id => EndpointId}),
            retry_job(JobId, EventId, EndpointId, TenantId, AttemptN, MaxAttempts)
    end.

retry_job(JobId, EventId, EndpointId, TenantId, AttemptN, MaxAttempts) ->
    case AttemptN >= MaxAttempts of
        true ->
            logger:warning(#{event => delivery_max_attempts_exceeded,
                             job_id => JobId, attempt_n => AttemptN,
                             max_attempts => MaxAttempts, endpoint_id => EndpointId}),
            dlq_job(JobId, EventId, EndpointId, TenantId, AttemptN, <<"max_attempts_exceeded">>);
        false ->
            Delay = gp_core_retry:backoff_secs(AttemptN, #{}),
            logger:info(#{event => delivery_retry_scheduled,
                          job_id => JobId, attempt_n => AttemptN, delay_secs => Delay}),
            gp_store_client:nack_job(JobId, Delay),
            gp_delivery_metrics:inc_retried(TenantId, EndpointId)
    end.

dlq_job(JobId, EventId, EndpointId, TenantId, AttemptN, Reason) ->
    gp_store_client:put_dlq(#{
        <<"job_id">>        => JobId,
        <<"event_id">>      => EventId,
        <<"endpoint_id">>   => EndpointId,
        <<"tenant_id">>     => TenantId,
        <<"reason">>        => Reason,
        <<"attempt_count">> => AttemptN
    }),
    gp_store_client:ack_job(JobId),
    gp_delivery_metrics:inc_dlq(TenantId, EndpointId).
