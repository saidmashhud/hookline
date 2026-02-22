%% ETS-based idempotency store.
%% Deduplicates event publishing within a 24h TTL window per tenant.
%% In-memory only — acceptable for at-least-once semantics.
-module(gp_core_idempotency).
-behaviour(gen_server).

-export([start_link/0, check/2, store/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE,   gp_idempotency_ets).
-define(TTL_MS,  86_400_000).          %% 24 hours
-define(GC_MS,   300_000).             %% GC every 5 minutes

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
    schedule_gc(),
    {ok, #{}}.

%% Returns {exists, EventId} | not_found
check(TenantId, IdempotencyKey) when is_binary(TenantId), is_binary(IdempotencyKey) ->
    Key = {TenantId, IdempotencyKey},
    Now = erlang:system_time(millisecond),
    case ets:lookup(?TABLE, Key) of
        [{_, EventId, ExpiresAt}] when ExpiresAt > Now -> {exists, EventId};
        _ -> not_found
    end.

store(TenantId, IdempotencyKey, EventId) ->
    Key       = {TenantId, IdempotencyKey},
    ExpiresAt = erlang:system_time(millisecond) + ?TTL_MS,
    ets:insert(?TABLE, {Key, EventId, ExpiresAt}).

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(gc, State) ->
    Now = erlang:system_time(millisecond),
    ets:select_delete(?TABLE, [{{'_', '_', '$1'}, [{'<', '$1', Now}], [true]}]),
    schedule_gc(),
    {noreply, State};
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

schedule_gc() -> erlang:send_after(?GC_MS, self(), gc).
