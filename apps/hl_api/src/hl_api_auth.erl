-module(hl_api_auth).
-behaviour(cowboy_middleware).

-export([execute/2, require_scope/2]).

%% Public paths that skip auth
-define(PUBLIC_PATHS, [
    <<"/healthz">>,
    <<"/readyz">>,
    <<"/metrics">>,
    <<"/openapi.yaml">>
]).

execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    IsPublic = lists:member(Path, ?PUBLIC_PATHS) orelse
               binary:match(Path, <<"/v1/dev/inbox/receive/">>) =/= nomatch orelse
               binary:match(Path, <<"/console">>) =/= nomatch,
    case IsPublic of
        true ->
            {ok, Req, Env};
        false ->
            case extract_token(Req) of
                {ok, Token} ->
                    case verify_token(Token) of
                        {ok, TenantId} ->
                            Scopes = get_scopes(Token),
                            Req2 = cowboy_req:set_resp_header(
                                       <<"x-tenant-id">>, TenantId, Req),
                            Req3 = Req2#{tenant_id => TenantId, scopes => Scopes},
                            Env2 = Env#{tenant_id => TenantId},
                            {ok, Req3, Env2};
                        {error, _} ->
                            reply_unauthorized(Req, Env)
                    end;
                {error, _} ->
                    reply_unauthorized(Req, Env)
            end
    end.

extract_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> -> {ok, Token};
        undefined -> {error, missing};
        _         -> {error, invalid_format}
    end.

verify_token(Token) ->
    AuthMode      = hl_config:get_str("HL_AUTH_MODE", "api_key"),
    case AuthMode of
        "service_token" ->
            %% Single shared secret for machine-to-machine calls (e.g. mashgate-webhook-delivery).
            %% HL_SERVICE_TOKEN must be set; if blank, fall through to api_key mode.
            ServiceToken  = list_to_binary(hl_config:get_str("HL_SERVICE_TOKEN", "")),
            DefaultTenant = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
            if
                ServiceToken =/= <<>> andalso Token =:= ServiceToken ->
                    {ok, DefaultTenant};
                true ->
                    {error, unauthorized}
            end;
        _ ->
            %% Default api_key mode: HL_ADMIN_KEY > HL_API_KEY > dynamic key store
            verify_token_api_key(Token)
    end.

verify_token_api_key(Token) ->
    AdminKey      = list_to_binary(hl_config:get_str("HL_ADMIN_KEY", "")),
    EnvKey        = list_to_binary(hl_config:get_str("HL_API_KEY", "dev-secret")),
    DefaultTenant = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
    if
        AdminKey =/= <<>> andalso Token =:= AdminKey ->
            {ok, DefaultTenant};
        Token =:= EnvKey ->
            {ok, DefaultTenant};
        true ->
            case hl_api_key_store:lookup_by_token(Token) of
                {ok, #{tenant_id := TId}} -> {ok, TId};
                {error, not_found}        -> {error, unauthorized}
            end
    end.

%% Return 403 if Token doesn't have the required scope.
require_scope(Req, Scope) ->
    Scopes = maps:get(scopes, Req, [<<"*">>]),
    case lists:member(<<"*">>, Scopes) orelse lists:member(Scope, Scopes) of
        true  -> ok;
        false ->
            Body = jsx:encode(#{<<"error">> => <<"forbidden">>,
                                <<"code">>  => <<"INSUFFICIENT_SCOPE">>,
                                <<"required_scope">> => Scope}),
            cowboy_req:reply(403,
                #{<<"content-type">> => <<"application/json">>},
                Body, Req),
            {stop, 403}
    end.

%% Get scopes for a token.
%%   HL_AUTH_MODE=service_token + HL_SERVICE_TOKEN  → [<<"*">>]  (trusted machine caller)
%%   HL_ADMIN_KEY  → [<<"admin">>, <<"*">>]          (all scopes + admin-only ops)
%%   HL_API_KEY    → [<<"*">>]                       (all scopes, backward compat)
%%   dynamic key   → whatever was set at creation
get_scopes(Token) ->
    AuthMode = hl_config:get_str("HL_AUTH_MODE", "api_key"),
    case AuthMode of
        "service_token" ->
            ServiceToken = list_to_binary(hl_config:get_str("HL_SERVICE_TOKEN", "")),
            if
                ServiceToken =/= <<>> andalso Token =:= ServiceToken -> [<<"*">>];
                true -> []
            end;
        _ ->
            AdminKey = list_to_binary(hl_config:get_str("HL_ADMIN_KEY", "")),
            EnvKey   = list_to_binary(hl_config:get_str("HL_API_KEY", "dev-secret")),
            if
                AdminKey =/= <<>> andalso Token =:= AdminKey ->
                    [<<"admin">>, <<"*">>];
                Token =:= EnvKey ->
                    [<<"*">>];
                true ->
                    case hl_api_key_store:lookup_scopes(Token) of
                        []     -> [];
                        Scopes -> Scopes
                    end
            end
    end.

reply_unauthorized(Req, Env) ->
    logger:warning(#{event => auth_failed,
                     path  => cowboy_req:path(Req),
                     peer  => cowboy_req:peer(Req)}),
    Body = jsx:encode(#{<<"error">> => <<"unauthorized">>,
                        <<"code">>  => <<"AUTH_REQUIRED">>}),
    Req2 = cowboy_req:reply(401,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req),
    {stop, Req2, Env}.
