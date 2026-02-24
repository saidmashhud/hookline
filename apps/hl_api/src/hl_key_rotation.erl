%% ETS-based overlap-window store for rotated API keys.
%%
%% After key rotation the old key's hash is inserted here with an expiry
%% timestamp.  Auth middleware checks this store when the main key store
%% returns not_found, allowing in-flight requests that still carry the old
%% token to succeed during the overlap window.
%%
%% Schema:  {Hash :: binary(), TenantId :: binary(), ExpiresAt :: integer()}
%%   Hash      — sha256(OldToken), same scheme as hl_api_key_store
%%   ExpiresAt — unix milliseconds
-module(hl_key_rotation).
-behaviour(gen_server).

-export([start_link/0, put_expiring/3, lookup_expiring/1, delete_expiring/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(ETS,   hl_key_rotation_ets).
-define(GC_MS, 60_000).   %% GC every 60 seconds

%%--------------------------------------------------------------------
%% Public API  (ETS direct reads — no gen_server round-trip)
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register an old key hash in the overlap store.
%% Hash must already be the sha256 of the raw token (to avoid
%% storing the plaintext token twice).
put_expiring(TenantId, Hash, ExpiresAt) when is_binary(TenantId),
                                              is_binary(Hash),
                                              is_integer(ExpiresAt) ->
    ets:insert(?ETS, {Hash, TenantId, ExpiresAt}).

%% Validate a raw token against the expiring store.
%% Returns {ok, TenantId} | not_found.
lookup_expiring(Token) when is_binary(Token) ->
    Hash = crypto:hash(sha256, Token),
    Now  = erlang:system_time(millisecond),
    case ets:lookup(?ETS, Hash) of
        [{_, TenantId, ExpiresAt}] when ExpiresAt > Now ->
            {ok, TenantId};
        _ ->
            not_found
    end.

%% Remove an entry early (e.g. forced revocation).
delete_expiring(Hash) when is_binary(Hash) ->
    ets:delete(?ETS, Hash).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    ets:new(?ETS, [named_table, public, set, {read_concurrency, true}]),
    schedule_gc(),
    {ok, #{}}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)        -> {noreply, State}.

handle_info(gc, State) ->
    Now = erlang:system_time(millisecond),
    Expired = ets:foldl(fun
        ({Hash, _, ExpiresAt}, Acc) when ExpiresAt =< Now -> [Hash | Acc];
        (_, Acc) -> Acc
    end, [], ?ETS),
    lists:foreach(fun(H) -> ets:delete(?ETS, H) end, Expired),
    logger:debug(#{event => key_rotation_gc, expired => length(Expired)}),
    schedule_gc(),
    {noreply, State};
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

schedule_gc() -> erlang:send_after(?GC_MS, self(), gc).
