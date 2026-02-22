-module(gp_api_auth).
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
    SingleTenantMode = gp_config:get_bool(<<"GP_SINGLE_TENANT">>, true),
    if
        SingleTenantMode ->
            ApiKey = list_to_binary(gp_config:get_str(<<"GP_API_KEY">>, "dev-secret")),
            if
                Token =:= ApiKey ->
                    TenantId = list_to_binary(
                        gp_config:get_str(<<"GP_TENANT_ID">>, "default")),
                    {ok, TenantId};
                true ->
                    %% Also check dynamically created API keys
                    gp_api_h_apikeys:lookup_by_token(Token)
            end;
        true ->
            %% Multi-tenant: check ETS-backed key store first
            case gp_api_h_apikeys:lookup_by_token(Token) of
                {ok, _} = R -> R;
                {error, _}  -> gp_api_tenants:verify(Token)
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

%% Get scopes for a token (default: all scopes for legacy env-var key)
get_scopes(Token) ->
    ApiKey = list_to_binary(gp_config:get_str(<<"GP_API_KEY">>, "dev-secret")),
    case Token =:= ApiKey of
        true  -> [<<"*">>];
        false ->
            case catch gp_api_h_apikeys:lookup_scopes(Token) of
                Scopes when is_list(Scopes) -> Scopes;
                _                           -> [<<"*">>]
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
