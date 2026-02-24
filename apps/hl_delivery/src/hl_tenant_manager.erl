%% Manages the lifecycle of per-endpoint delivery actors.
%% On startup loads all endpoints from C store and boots actors.
%% HTTP handlers call start_actor/stop_actor/update_actor for live changes.
-module(hl_tenant_manager).
-behaviour(gen_server).

-export([start_link/0, start_actor/1, stop_actor/1, update_actor/1,
         load_tenant/1, load_tenant_sync/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3]).

-define(RETRY_MS, 200).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_actor(Ep) ->
    supervisor:start_child(hl_delivery_actor_sup, [Ep]).

stop_actor(EpId) ->
    case hl_endpoint_registry:lookup(EpId) of
        {ok, Pid} -> supervisor:terminate_child(hl_delivery_actor_sup, Pid);
        not_found -> ok
    end.

update_actor(Ep) ->
    EpId = maps:get(<<"endpoint_id">>, Ep),
    case hl_endpoint_registry:lookup(EpId) of
        {ok, Pid} -> gen_server:cast(Pid, {update_config, Ep});
        not_found -> start_actor(Ep)
    end.

%% Boot delivery actors for a single tenant (called after dynamic tenant creation).
load_tenant(TenantId) ->
    gen_server:cast(?MODULE, {load_tenant, TenantId}).

%% Synchronous tenant boot used when tenant is auto-materialized at request time.
load_tenant_sync(TenantId) ->
    gen_server:call(?MODULE, {load_tenant_sync, TenantId}).

%% ── gen_server ───────────────────────────────────────────────────────────────

init([]) ->
    {ok, #{}, {continue, load}}.

handle_continue(load, State) ->
    maybe_boot_actors(),
    {noreply, State}.

handle_call({load_tenant_sync, TenantId}, _From, State) ->
    load_tenant_eps(TenantId),
    {reply, ok, State};
handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast({load_tenant, TenantId}, State) ->
    load_tenant_eps(TenantId),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(retry_load, State) ->
    maybe_boot_actors(),
    {noreply, State};
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

maybe_boot_actors() ->
    case boot_actors() of
        ok ->
            ok;
        retry ->
            erlang:send_after(?RETRY_MS, self(), retry_load)
    end.

boot_actors() ->
    case hl_config:get_str("HL_AUTH_MODE", "api_key") of
        "service_token" ->
            %% Multi-tenant embedded mode: boot actors for ALL active tenants.
            case tenant_list() of
                {ok, Tenants} ->
                    lists:foreach(fun(#{<<"tenant_id">> := TId}) ->
                        load_tenant_eps(TId)
                    end, Tenants),
                    ok;
                retry ->
                    retry
            end;
        _ ->
            TenantId = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
            load_tenant_eps(TenantId),
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

load_tenant_eps(TenantId) ->
    case hl_store_client:list_endpoints(TenantId) of
        {ok, #{<<"items">> := Eps}} ->
            lists:foreach(fun(Ep) ->
                Dec = decode_ep(Ep),
                case maps:get(<<"enabled">>, Dec, true) of
                    true -> update_actor(Dec);
                    _    -> ok
                end
            end, Eps);
        _ ->
            ok
    end.

decode_ep(Ep) when is_map(Ep) -> Ep;
decode_ep(B) when is_binary(B)  -> jsx:decode(B, [return_maps]).
