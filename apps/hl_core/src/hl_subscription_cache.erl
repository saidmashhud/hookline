%% ETS-backed subscription cache.
%% Keeps {TenantId, SubId} -> subscription map in memory so that
%% hl_core:route_event does zero C store round-trips on the hot path.
-module(hl_subscription_cache).
-behaviour(gen_server).

-export([start_link/0, match/2, add/1, remove/2, load_tenant/1, load_tenant_sync/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3]).

-define(TABLE, ?MODULE).
-define(RETRY_MS, 200).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ETS read — no C store round-trip on event publish.
%% Returns all subscriptions whose topic_pattern matches Topic.
match(TenantId, Topic) ->
    All = ets:match(?TABLE, {{TenantId, '_'}, '$1'}),
    [Sub || [Sub] <- All,
            hl_core_topic:matches(
                maps:get(<<"topic_pattern">>, Sub, <<"#">>), Topic)].

add(Sub) ->
    ets:insert(?TABLE, {{maps:get(<<"tenant_id">>, Sub),
                         maps:get(<<"subscription_id">>, Sub)}, Sub}).

remove(TenantId, SubId) ->
    ets:delete(?TABLE, {TenantId, SubId}).

%% Load (or reload) all subscriptions for a single tenant into the cache.
%% Called after dynamic tenant creation in service_token mode.
load_tenant(TenantId) ->
    gen_server:cast(?MODULE, {load_tenant, TenantId}).

%% Synchronous variant used when a tenant is auto-materialized at request time.
%% Ensures cache is warm before continuing request processing.
load_tenant_sync(TenantId) ->
    gen_server:call(?MODULE, {load_tenant_sync, TenantId}).

%% ── gen_server ───────────────────────────────────────────────────────────────

init([]) ->
    ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]),
    {ok, #{}, {continue, load}}.

handle_continue(load, State) ->
    maybe_warm_cache(),
    {noreply, State}.

handle_call({load_tenant_sync, TenantId}, _From, State) ->
    load_tenant_subs(TenantId),
    {reply, ok, State};
handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast({load_tenant, TenantId}, State) ->
    load_tenant_subs(TenantId),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(retry_load, State) ->
    maybe_warm_cache(),
    {noreply, State};
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

maybe_warm_cache() ->
    case warm_cache() of
        ok ->
            ok;
        retry ->
            erlang:send_after(?RETRY_MS, self(), retry_load)
    end.

warm_cache() ->
    case hl_config:get_str("HL_AUTH_MODE", "api_key") of
        "service_token" ->
            %% Multi-tenant embedded mode: warm cache for ALL active tenants.
            case tenant_list() of
                {ok, Tenants} ->
                    lists:foreach(fun(#{<<"tenant_id">> := TId}) ->
                        load_tenant_subs(TId)
                    end, Tenants),
                    ok;
                retry ->
                    retry
            end;
        _ ->
            %% Single-tenant / api_key mode: load the configured default tenant.
            TenantId = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
            load_tenant_subs(TenantId),
            ok
    end.

tenant_list() ->
    try
        {ok, hl_tenant_store:list()}
    catch
        %% hl_tenant_store ETS table may not be ready yet during startup.
        error:badarg ->
            retry;
        error:undef ->
            retry
    end.

load_tenant_subs(TenantId) ->
    case hl_store_client:list_subscriptions(TenantId) of
        {ok, #{<<"items">> := Subs}} ->
            lists:foreach(fun(Sub) -> add(decode_sub(Sub)) end, Subs);
        _ ->
            ok
    end.

decode_sub(Sub) when is_map(Sub) -> Sub;
decode_sub(B) when is_binary(B)  -> jsx:decode(B, [return_maps]).
