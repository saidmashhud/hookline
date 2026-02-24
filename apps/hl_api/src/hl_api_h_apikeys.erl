%% API key management for the current tenant.
%%
%% Routes (scoped to the authenticated tenant):
%%   GET    /v1/apikeys      — list own keys
%%   POST   /v1/apikeys      — create a new key
%%   DELETE /v1/apikeys/:id  — revoke a key
%%
%% For admin cross-tenant key management see hl_api_h_tenant_apikeys.
-module(hl_api_h_apikeys).
-export([init/2]).

%% Kept for backward compat — hl_api_key_store owns the ETS table now.
-export([lookup_by_token/1, lookup_scopes/1, init_table/0]).

lookup_by_token(Token) ->
    case hl_api_key_store:lookup_by_token(Token) of
        {ok, #{tenant_id := TId}} -> {ok, TId};
        {error, _} = E            -> E
    end.

lookup_scopes(Token) ->
    hl_api_key_store:lookup_scopes(Token).

%% No-op: ETS table is now managed by hl_api_key_store.
init_table() -> ok.

init(Req0, Opts) ->
    case hl_api_auth:require_scope(Req0, <<"admin">>) of
        ok ->
            Method = cowboy_req:method(Req0),
            handle(Method, Req0, Opts);
        {stop, _Code} ->
            {ok, Req0, Opts}
    end.

%% POST /v1/apikeys  — create a key for the current tenant
handle(<<"POST">>, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    TenantId = get_tenant(Req1),
    Params   = decode_body(Body),
    Name     = maps:get(<<"name">>,   Params, <<"default">>),
    Scopes   = valid_scopes(maps:get(<<"scopes">>, Params, [<<"*">>])),
    case hl_api_key_store:put(TenantId, Name, Scopes, <<"self">>) of
        {ok, Token, KeyId} ->
            Now = erlang:system_time(millisecond),
            reply_json(201, #{
                <<"id">>         => KeyId,
                <<"key">>        => Token,
                <<"name">>       => Name,
                <<"tenant_id">>  => TenantId,
                <<"scopes">>     => Scopes,
                <<"created_at">> => Now,
                <<"note">>       => <<"Store the key value — it will not be shown again.">>
            }, Req1, Opts)
    end;

%% GET /v1/apikeys  — list key metadata for current tenant
handle(<<"GET">>, Req0, Opts) ->
    TenantId = get_tenant(Req0),
    Items    = hl_api_key_store:list(TenantId),
    reply_json(200, #{<<"items">> => Items}, Req0, Opts);

%% DELETE /v1/apikeys/:id  — revoke a key
handle(<<"DELETE">>, Req0, Opts) ->
    case cowboy_req:binding(id, Req0) of
        undefined ->
            hl_api_error:reply(Req0, 400, validation_error, <<"key_id required">>),
            {ok, Req0, Opts};
        KeyId ->
            TenantId = get_tenant(Req0),
            case hl_api_key_store:delete(TenantId, KeyId) of
                ok ->
                    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
                    {ok, Req, Opts};
                {error, not_found} ->
                    hl_api_error:reply(Req0, 404, not_found, <<"Key not found">>),
                    {ok, Req0, Opts};
                {error, forbidden} ->
                    hl_api_error:reply(Req0, 403, forbidden,
                                       <<"Key belongs to a different tenant">>),
                    {ok, Req0, Opts}
            end
    end;

handle(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(hl_config:get_str("HL_TENANT_ID", "default"))).

decode_body(<<>>) -> #{};
decode_body(B) ->
    case catch jsx:decode(B, [return_maps]) of
        M when is_map(M) -> M;
        _                -> #{}
    end.

valid_scopes(S) when is_list(S) -> S;
valid_scopes(_)                  -> [<<"*">>].

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
