%% Persistent API key store.
%%
%% ETS is the hot-path (O(1) lookup by token hash).
%% DETS backs ETS to disk so keys survive Erlang node restarts.
%%
%% Record schema (7-tuple):
%%   {KeyId, TenantId, Hash, Name, Scopes, CreatedBy, CreatedAt}
%%     KeyId     :: binary()  — UUID
%%     TenantId  :: binary()
%%     Hash      :: binary()  — sha256(Token), never store raw token
%%     Name      :: binary()
%%     Scopes    :: [binary()]
%%     CreatedBy :: binary()  — "admin" | key_id of creator
%%     CreatedAt :: integer() — unix ms
-module(hl_api_key_store).
-behaviour(gen_server).

-export([start_link/0]).
-export([put/4, lookup_by_token/1, lookup_hash_by_id/1, lookup_scopes/1,
         list/1, delete/2, delete_tenant/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(ETS,  hl_apikeys_ets).
-define(DETS, hl_apikeys_dets).

%%--------------------------------------------------------------------
%% Public API (all ETS reads — no gen_server round-trip)
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Create a new API key for TenantId. Returns {ok, Token, KeyId}.
put(TenantId, Name, Scopes, CreatedBy) ->
    gen_server:call(?MODULE, {put, TenantId, Name, Scopes, CreatedBy}).

%% Returns {ok, #{tenant_id, scopes}} | {error, not_found}.
%% Direct ETS read — no gen_server round-trip.
lookup_by_token(Token) ->
    Hash = crypto:hash(sha256, Token),
    case ets:match(?ETS, {'_', '$1', Hash, '_', '$2', '_', '_'}) of
        [[TenantId, Scopes] | _] ->
            {ok, #{tenant_id => TenantId, scopes => Scopes}};
        [] ->
            {error, not_found}
    end.

%% Returns {ok, Hash} | {error, not_found} — used by rotation handler.
lookup_hash_by_id(KeyId) ->
    case ets:lookup(?ETS, KeyId) of
        [{KeyId, _T, Hash, _, _, _, _}] -> {ok, Hash};
        []                              -> {error, not_found}
    end.

%% Returns list of scope binaries, or [] if not found.
lookup_scopes(Token) ->
    Hash = crypto:hash(sha256, Token),
    case ets:match(?ETS, {'_', '_', Hash, '_', '$1', '_', '_'}) of
        [[Scopes] | _] -> Scopes;
        []             -> []
    end.

%% List key metadata for a tenant (no token values returned).
list(TenantId) ->
    ets:foldl(fun
        ({KeyId, T, _Hash, Name, Scopes, CreatedBy, CreatedAt}, Acc)
                when T =:= TenantId ->
            [#{<<"key_id">>     => KeyId,
               <<"name">>       => Name,
               <<"scopes">>     => Scopes,
               <<"created_by">> => CreatedBy,
               <<"created_at">> => CreatedAt} | Acc];
        (_, Acc) ->
            Acc
    end, [], ?ETS).

%% Delete a key. Checks tenant ownership.
%% Returns ok | {error, not_found} | {error, forbidden}.
delete(TenantId, KeyId) ->
    gen_server:call(?MODULE, {delete, TenantId, KeyId}).

%% Delete ALL keys for a tenant (used during tenant purge).
delete_tenant(TenantId) ->
    gen_server:call(?MODULE, {delete_tenant, TenantId}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    DataDir  = hl_config:get_str("HL_DATA_DIR", "/tmp/hl_data"),
    DetsFile = filename:join(DataDir, "api_keys.dets"),
    ok = filelib:ensure_dir(DetsFile),
    {ok, _}  = dets:open_file(?DETS, [{file, DetsFile}, {type, set}]),
    ets:new(?ETS, [named_table, public, set, {read_concurrency, true}]),
    dets:foldl(fun(Record, ok) -> ets:insert(?ETS, Record), ok end, ok, ?DETS),
    Count = ets:info(?ETS, size),
    logger:info(#{event => api_key_store_loaded, keys => Count}),
    {ok, #{}}.

handle_call({put, TenantId, Name, Scopes, CreatedBy}, _From, State) ->
    KeyId  = list_to_binary(hl_core_uuid:generate_str()),
    Token  = gen_token(),
    Hash   = crypto:hash(sha256, Token),
    Now    = erlang:system_time(millisecond),
    Record = {KeyId, TenantId, Hash, Name, Scopes, CreatedBy, Now},
    ets:insert(?ETS, Record),
    ok = dets:insert(?DETS, Record),
    logger:info(#{event => api_key_created, key_id => KeyId,
                  tenant_id => TenantId, name => Name}),
    {reply, {ok, Token, KeyId}, State};

handle_call({delete, TenantId, KeyId}, _From, State) ->
    case ets:lookup(?ETS, KeyId) of
        [{KeyId, TenantId, _, _, _, _, _}] ->
            ets:delete(?ETS, KeyId),
            dets:delete(?DETS, KeyId),
            logger:info(#{event => api_key_deleted, key_id => KeyId,
                          tenant_id => TenantId}),
            {reply, ok, State};
        [{KeyId, _Other, _, _, _, _, _}] ->
            {reply, {error, forbidden}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_tenant, TenantId}, _From, State) ->
    %% Collect all key IDs belonging to this tenant, then delete
    KeyIds = ets:foldl(fun
        ({KeyId, T, _, _, _, _, _}, Acc) when T =:= TenantId -> [KeyId | Acc];
        (_, Acc) -> Acc
    end, [], ?ETS),
    lists:foreach(fun(KeyId) ->
        ets:delete(?ETS, KeyId),
        dets:delete(?DETS, KeyId)
    end, KeyIds),
    logger:info(#{event => api_keys_purged, tenant_id => TenantId,
                  count => length(KeyIds)}),
    {reply, {ok, length(KeyIds)}, State};

handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    catch dets:close(?DETS),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

gen_token() ->
    Rand = crypto:strong_rand_bytes(24),
    Hex  = binary:encode_hex(Rand),
    <<"sk_live_", Hex/binary>>.
