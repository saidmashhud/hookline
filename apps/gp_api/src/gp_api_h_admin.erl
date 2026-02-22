-module(gp_api_h_admin).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),
    handle(Method, Path, Req0, Opts).

%% GET /v1/admin/stats
handle(<<"GET">>, <<"/v1/admin/stats">>, Req0, Opts) ->
    TenantId = get_tenant(Req0),
    QueueDepth = case gp_store_client:queue_stats(TenantId) of
        {ok, #{<<"pending">> := P, <<"inflight">> := I}} ->
            #{<<"pending">> => P, <<"inflight">> => I};
        _ ->
            #{<<"pending">> => 0, <<"inflight">> => 0}
    end,
    CompactionStats = case catch gp_compaction:stats() of
        #{last_run_at := T, segments_deleted := S, duration_ms := D} ->
            #{<<"last_run_at">> => T,
              <<"segments_deleted">> => S,
              <<"duration_ms">> => D};
        _ ->
            #{<<"last_run_at">> => 0,
              <<"segments_deleted">> => 0,
              <<"duration_ms">> => 0}
    end,
    Body = #{
        <<"queue">>      => QueueDepth,
        <<"compaction">> => CompactionStats
    },
    reply_json(200, Body, Req0, Opts);

%% POST /v1/admin/compact
handle(<<"POST">>, <<"/v1/admin/compact">>, Req0, Opts) ->
    gp_compaction:run(),
    reply_json(202, #{<<"ok">> => true, <<"message">> => <<"compaction triggered">>},
               Req0, Opts);

%% POST /v1/admin/store/pause-claims
handle(<<"POST">>, <<"/v1/admin/store/pause-claims">>, Req0, Opts) ->
    gp_delivery_poller:pause(),
    reply_json(200, #{<<"ok">> => true, <<"paused">> => true}, Req0, Opts);

%% POST /v1/admin/store/resume-claims
handle(<<"POST">>, <<"/v1/admin/store/resume-claims">>, Req0, Opts) ->
    gp_delivery_poller:resume(),
    reply_json(200, #{<<"ok">> => true, <<"paused">> => false}, Req0, Opts);

%% POST /v1/admin/rotate-secrets
handle(<<"POST">>, <<"/v1/admin/rotate-secrets">>, Req0, Opts) ->
    gp_core_crypto:rotate_key(),
    reply_json(200, #{<<"ok">> => true, <<"message">> => <<"key rotation complete">>},
               Req0, Opts);

%% GET /v1/admin/audit
handle(<<"GET">>, <<"/v1/admin/audit">>, Req0, Opts) ->
    TenantId = get_tenant(Req0),
    QS    = cowboy_req:parse_qs(Req0),
    Limit = qs_int(QS, <<"limit">>, 100),
    case gp_store_client:list_audit(TenantId, #{<<"limit">> => Limit}) of
        {ok, #{<<"items">> := Items}} ->
            reply_json(200, #{<<"items">> => Items,
                              <<"count">> => length(Items)}, Req0, Opts);
        {error, Reason} ->
            gp_api_error:reply(Req0, 500, store_error,
                list_to_binary(io_lib:format("~p", [Reason]))),
            {ok, Req0, Opts}
    end;

handle(_, _, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(gp_config:get_str("GP_TENANT_ID", "default"))).

qs_int(QS, Key, Default) ->
    case proplists:get_value(Key, QS) of
        undefined -> Default;
        V -> try binary_to_integer(V) catch _:_ -> Default end
    end.

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
