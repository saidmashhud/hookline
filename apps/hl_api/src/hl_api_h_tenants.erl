%% Tenant management handler. All operations require admin scope.
%%
%% Routes:
%%   POST   /v1/tenants          — create tenant
%%   GET    /v1/tenants/:id      — get tenant
%%   DELETE /v1/tenants/:id      — delete tenant + purge all data
%%   GET    /v1/tenants/:id/stats— delivery stats for tenant
-module(hl_api_h_tenants).
-export([init/2]).

init(Req0, Opts) ->
    case hl_api_auth:require_scope(Req0, <<"admin">>) of
        ok ->
            Method   = cowboy_req:method(Req0),
            TenantId = cowboy_req:binding(id, Req0),
            Action   = maps:get(action, Opts, undefined),
            handle(Method, TenantId, Action, Req0, Opts);
        {stop, _} ->
            {ok, Req0, Opts}
    end.

%%--------------------------------------------------------------------
%% POST /v1/tenants — create (idempotent via Idempotency-Key header)
%%--------------------------------------------------------------------
handle(<<"POST">>, undefined, undefined, Req0, Opts) ->
    IdempKey = cowboy_req:header(<<"idempotency-key">>, Req0, <<>>),
    %% Fast path: replay a previous identical request.
    case check_idempotency(IdempKey) of
        {cached, TenantId} ->
            case hl_tenant_store:get(TenantId) of
                {ok, Tenant} -> reply_json(200, Tenant, Req0, Opts);
                _             -> do_create_tenant(IdempKey, Req0, Opts)
            end;
        miss ->
            do_create_tenant(IdempKey, Req0, Opts)
    end;

%%--------------------------------------------------------------------
%% GET /v1/tenants — list all active tenants
%%--------------------------------------------------------------------
handle(<<"GET">>, undefined, undefined, Req0, Opts) ->
    Tenants = hl_tenant_store:list(),
    reply_json(200, #{<<"tenants">> => Tenants,
                      <<"total">>   => length(Tenants)}, Req0, Opts);

%%--------------------------------------------------------------------
%% GET /v1/tenants/:id — get
%%--------------------------------------------------------------------
handle(<<"GET">>, TenantId, undefined, Req0, Opts) when TenantId =/= undefined ->
    case hl_tenant_store:get(TenantId) of
        {ok, Tenant} ->
            reply_json(200, Tenant, Req0, Opts);
        {error, not_found} ->
            hl_api_error:reply(Req0, 404, not_found, <<"Tenant not found">>),
            {ok, Req0, Opts}
    end;

%%--------------------------------------------------------------------
%% GET /v1/tenants/:id/stats — stats
%%--------------------------------------------------------------------
handle(<<"GET">>, TenantId, stats, Req0, Opts) when TenantId =/= undefined ->
    case hl_tenant_store:exists(TenantId) of
        false ->
            hl_api_error:reply(Req0, 404, not_found, <<"Tenant not found">>),
            {ok, Req0, Opts};
        true ->
            Stats = collect_stats(TenantId),
            reply_json(200, Stats, Req0, Opts)
    end;

%%--------------------------------------------------------------------
%% DELETE /v1/tenants/:id — delete + purge
%%--------------------------------------------------------------------
handle(<<"DELETE">>, TenantId, undefined, Req0, Opts) when TenantId =/= undefined ->
    case hl_tenant_store:get(TenantId) of
        {error, not_found} ->
            hl_api_error:reply(Req0, 404, not_found, <<"Tenant not found">>),
            {ok, Req0, Opts};
        {ok, _} ->
            purge_tenant(TenantId),
            Req = cowboy_req:reply(204, #{}, <<>>, Req0),
            {ok, Req, Opts}
    end;

handle(_, _, _, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

%%--------------------------------------------------------------------
%% Purge: stop actors, remove keys, remove endpoints + subscriptions
%%        from C store. Events/attempts stay (compaction handles them).
%%--------------------------------------------------------------------
purge_tenant(TenantId) ->
    %% 1. Stop all endpoint actors
    case hl_store_client:list_endpoints(TenantId) of
        {ok, #{<<"endpoints">> := Endpoints}} ->
            lists:foreach(fun(Ep) ->
                EpId = maps:get(<<"endpoint_id">>, Ep, undefined),
                EpId =/= undefined andalso
                    catch hl_tenant_manager:stop_actor(EpId)
            end, Endpoints);
        _ -> ok
    end,
    %% 2. Remove subscriptions from C store + subscription cache
    case hl_store_client:list_subscriptions(TenantId) of
        {ok, #{<<"subscriptions">> := Subs}} ->
            lists:foreach(fun(Sub) ->
                SubId = maps:get(<<"subscription_id">>, Sub, undefined),
                SubId =/= undefined andalso begin
                    catch hl_subscription_cache:remove(TenantId, SubId),
                    catch hl_store_client:delete_subscription(TenantId, SubId)
                end
            end, Subs);
        _ -> ok
    end,
    %% 3. Remove endpoints from C store
    case hl_store_client:list_endpoints(TenantId) of
        {ok, #{<<"endpoints">> := Eps2}} ->
            lists:foreach(fun(Ep) ->
                EpId = maps:get(<<"endpoint_id">>, Ep, undefined),
                EpId =/= undefined andalso
                    catch hl_store_client:delete_endpoint(TenantId, EpId)
            end, Eps2);
        _ -> ok
    end,
    %% 4. Remove all API keys for this tenant
    catch hl_api_key_store:delete_tenant(TenantId),
    %% 5. Mark tenant as deleted
    hl_tenant_store:delete(TenantId),
    logger:info(#{event => tenant_purged, tenant_id => TenantId}).

%%--------------------------------------------------------------------
%% Stats
%%--------------------------------------------------------------------
collect_stats(TenantId) ->
    QueueStats = case hl_store_client:queue_stats(TenantId) of
        {ok, S} -> S;
        _       -> #{}
    end,
    DlqCount = case hl_store_client:list_dlq(TenantId) of
        {ok, #{<<"entries">> := Entries}} -> length(Entries);
        _ -> 0
    end,
    KeyCount  = length(hl_api_key_store:list(TenantId)),
    #{<<"tenant_id">>   => TenantId,
      <<"queue_depth">> => maps:get(<<"queue_depth">>, QueueStats, 0),
      <<"in_flight">>   => maps:get(<<"in_flight">>,   QueueStats, 0),
      <<"dlq_count">>   => DlqCount,
      <<"api_keys">>    => KeyCount}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

check_idempotency(<<>>) -> miss;
check_idempotency(Key) ->
    case hl_core_idempotency:check(<<"admin">>, Key) of
        {exists, TenantId} -> {cached, TenantId};
        not_found          -> miss
    end.

do_create_tenant(IdempKey, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Params   = decode_body(Body),
    Name     = maps:get(<<"name">>, Params, <<"unnamed">>),
    TenantId = case maps:get(<<"id">>, Params, undefined) of
        undefined -> list_to_binary(hl_core_uuid:generate_str());
        Id        -> Id
    end,
    case hl_tenant_store:create(TenantId, Name) of
        {ok, TId} ->
            IdempKey =/= <<>> andalso
                hl_core_idempotency:store(<<"admin">>, IdempKey, TId),
            %% Warm caches for the new tenant so it can receive events immediately.
            hl_subscription_cache:load_tenant(TId),
            hl_tenant_manager:load_tenant(TId),
            {ok, Tenant} = hl_tenant_store:get(TId),
            reply_json(201, Tenant, Req1, Opts);
        {error, already_exists} ->
            hl_api_error:reply(Req0, 409, conflict,
                               <<"Tenant already exists">>),
            {ok, Req0, Opts}
    end.

decode_body(<<>>) -> #{};
decode_body(B) ->
    case catch jsx:decode(B, [return_maps]) of
        M when is_map(M) -> M;
        _                -> #{}
    end.

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
