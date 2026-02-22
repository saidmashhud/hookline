%% API key management handler.
%% Keys are stored in ETS and survive only for the process lifetime (in-memory).
%% For production persistence, keys should be stored via gp_store.
-module(gp_api_h_apikeys).
-export([init/2, init_table/0, lookup_by_token/1, lookup_scopes/1]).

-define(TABLE, gp_apikeys_ets).

%% Call once at app startup
init_table() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end.

%% Returns {ok, TenantId} | {error, not_found}
%% Table schema: {KeyId, TenantId, Hash, Name, Scopes, CreatedAt}
lookup_by_token(Token) ->
    Hash = crypto:hash(sha256, Token),
    case ets:match(?TABLE, {'_', '$1', Hash, '_', '_', '_'}) of
        [[TenantId] | _] -> {ok, TenantId};
        []               -> {error, not_found}
    end.

%% Returns list of scope binaries for a token, or [<<"*">>] if not found
lookup_scopes(Token) ->
    Hash = crypto:hash(sha256, Token),
    case ets:match(?TABLE, {'_', '_', Hash, '_', '$1', '_'}) of
        [[Scopes] | _] -> Scopes;
        []             -> [<<"*">>]
    end.

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle(Method, Req0, Opts).

%% POST /v1/apikeys  — create a new API key
handle(<<"POST">>, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    TId = get_tenant(Req1),
    Params = case Body of
        <<>> -> #{};
        B    -> catch jsx:decode(B, [return_maps])
    end,
    Name = case Params of
        P when is_map(P) -> maps:get(<<"name">>, P, <<"default">>);
        _                -> <<"default">>
    end,

    Scopes = case Params of
        PS when is_map(PS) ->
            case maps:get(<<"scopes">>, PS, undefined) of
                S when is_list(S) -> S;
                _                 -> [<<"*">>]
            end;
        _ -> [<<"*">>]
    end,
    KeyId  = gp_core_uuid:generate_str(),
    Token  = gen_token(),
    Hash   = crypto:hash(sha256, Token),
    Now    = erlang:system_time(millisecond),

    ets:insert(?TABLE, {KeyId, TId, Hash, Name, Scopes, Now}),

    logger:info(#{event => apikey_created, key_id => KeyId, tenant_id => TId}),

    reply_json(201, #{
        <<"id">>        => KeyId,
        <<"key_id">>    => KeyId,
        <<"key">>       => Token,
        <<"name">>      => Name,
        <<"tenant_id">> => TId,
        <<"created_at">> => Now,
        <<"note">>      => <<"Store the key value — it will not be shown again.">>
    }, Req1, Opts);

%% GET /v1/apikeys  — list key metadata (no actual token values)
handle(<<"GET">>, Req0, Opts) ->
    TId = get_tenant(Req0),
    Items = ets:foldl(fun
        ({KeyId, T, _Hash, Name, Scopes, CreatedAt}, Acc) when T =:= TId ->
            [#{<<"key_id">>    => KeyId,
               <<"name">>      => Name,
               <<"scopes">>    => Scopes,
               <<"tenant_id">> => T,
               <<"created_at">> => CreatedAt} | Acc];
        (_, Acc) -> Acc
    end, [], ?TABLE),
    reply_json(200, #{<<"items">> => Items}, Req0, Opts);

%% DELETE /v1/apikeys/:id  — revoke a key
handle(<<"DELETE">>, Req0, Opts) ->
    case cowboy_req:binding(id, Req0) of
        undefined ->
            gp_api_error:reply(Req0, 400, validation_error, <<"key_id required">>),
            {ok, Req0, Opts};
        KeyId ->
            TId2 = get_tenant(Req0),
            case ets:lookup(?TABLE, KeyId) of
                [] ->
                    gp_api_error:reply(Req0, 404, not_found, <<"Key not found">>),
                    {ok, Req0, Opts};
                [{KeyId, TId2, _, _, _, _}] ->
                    ets:delete(?TABLE, KeyId),
                    logger:info(#{event => apikey_revoked, key_id => KeyId}),
                    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
                    {ok, Req, Opts};
                _ ->
                    gp_api_error:reply(Req0, 404, not_found, <<"Key not found">>),
                    {ok, Req0, Opts}
            end
    end;

handle(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

gen_token() ->
    Rand = crypto:strong_rand_bytes(24),
    Hex  = binary:encode_hex(Rand),
    <<"gp_", Hex/binary>>.

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(gp_config:get_str("GP_TENANT_ID", "default"))).

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
