-module(hl_api_auth).
-behaviour(cowboy_middleware).

-export([execute/2, require_scope/2]).

%% Scopes granted in embedded mode (HL_EMBEDDED_MODE=true).
%% Restricts service_token callers to data-plane operations only;
%% admin/tenant management endpoints remain blocked.
-define(EMBEDDED_SCOPES, [
    <<"events.publish">>, <<"events.read">>,
    <<"endpoints.read">>, <<"endpoints.write">>,
    <<"subscriptions.read">>, <<"subscriptions.write">>,
    <<"deliveries.read">>, <<"deliveries.retry">>,
    <<"dlq.read">>, <<"dlq.replay">>, <<"stream.subscribe">>
]).

%% Public paths that skip auth
-define(PUBLIC_PATHS, [
    <<"/healthz">>,
    <<"/readyz">>,
    <<"/v1/health/embedded">>,
    <<"/metrics">>,
    <<"/openapi.yaml">>
]).

execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    IsEmbedded = hl_config:get_str("HL_EMBEDDED_MODE", "false") =:= "true",
    IsPublic = lists:member(Path, ?PUBLIC_PATHS) orelse
               binary:match(Path, <<"/v1/dev/inbox/receive/">>) =/= nomatch orelse
               %% /console is public only in standalone mode;
               %% embedded mode blocks it so Mashgate callers cannot access the admin UI
               (not IsEmbedded andalso binary:match(Path, <<"/console">>) =/= nomatch),
    case IsPublic of
        true ->
            {ok, Req, Env};
        false ->
            AuthMode = hl_config:get_str("HL_AUTH_MODE", "api_key"),
            case extract_token(Req) of
                {ok, Token} ->
                    case authenticate(AuthMode, Token, Req, Env) of
                        {ok, Req1, Env1} ->
                            enforce_scope(Req1, Env1);
                        Other ->
                            Other
                    end;
                {error, _} ->
                    reply_unauthorized(Req, Env)
            end
    end.

%% service_token mode: shared secret proves caller identity;
%% tenant is resolved from the mandatory X-Tenant-Id request header.
%% Missing header → 400 (not 401) so callers get a clear error.
authenticate("service_token", Token, Req, Env) ->
    ServiceToken = list_to_binary(hl_config:get_str("HL_SERVICE_TOKEN", "")),
    if
        ServiceToken =:= <<>> orelse Token =/= ServiceToken ->
            reply_unauthorized(Req, Env);
        true ->
            case cowboy_req:header(<<"x-tenant-id">>, Req, undefined) of
                undefined ->
                    reply_missing_tenant(Req, Env);
                TenantId ->
                    case ensure_service_tenant(TenantId) of
                        ok ->
                            Scopes = case hl_config:get_str("HL_EMBEDDED_MODE", "false") of
                                "true" -> ?EMBEDDED_SCOPES;
                                _      -> [<<"*">>]
                            end,
                            Req2 = cowboy_req:set_resp_header(<<"x-tenant-id">>, TenantId, Req),
                            Req3 = Req2#{tenant_id => TenantId, scopes => Scopes},
                            Env2 = Env#{tenant_id => TenantId},
                            {ok, Req3, Env2};
                        {error, Reason} ->
                            reply_tenant_bootstrap_failed(Req, Env, TenantId, Reason)
                    end
            end
    end;

%% api_key mode (default): tenant is embedded in the token.
authenticate(_Mode, Token, Req, Env) ->
    case verify_token(Token) of
        {ok, TenantId} ->
            Scopes = get_scopes(Token),
            Req2 = cowboy_req:set_resp_header(<<"x-tenant-id">>, TenantId, Req),
            Req3 = Req2#{tenant_id => TenantId, scopes => Scopes},
            Env2 = Env#{tenant_id => TenantId},
            {ok, Req3, Env2};
        {error, _} ->
            reply_unauthorized(Req, Env)
    end.

extract_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> -> {ok, Token};
        undefined -> {error, missing};
        _         -> {error, invalid_format}
    end.

%% Used by api_key mode only. HL_ADMIN_KEY > HL_API_KEY > dynamic key store.
verify_token(Token) ->
    AdminKey      = list_to_binary(hl_config:get_str("HL_ADMIN_KEY", "")),
    EnvKey        = list_to_binary(hl_config:get_str("HL_API_KEY", "")),
    DefaultTenant = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
    if
        AdminKey =/= <<>> andalso Token =:= AdminKey ->
            {ok, DefaultTenant};
        EnvKey =/= <<>> andalso Token =:= EnvKey ->
            {ok, DefaultTenant};
        true ->
            case hl_api_key_store:lookup_by_token(Token) of
                {ok, #{tenant_id := TId}} -> {ok, TId};
                {error, not_found}        -> {error, unauthorized}
            end
    end.

%% Return 403 if Token doesn't have the required scope.
require_scope(Req, Scope) ->
    case has_scope(Req, Scope) of
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

enforce_scope(Req, Env) ->
    Method = cowboy_req:method(Req),
    Path   = cowboy_req:path(Req),
    case required_scope(Method, Path) of
        undefined ->
            {ok, Req, Env};
        Scope ->
            case has_scope(Req, Scope) of
                true ->
                    {ok, Req, Env};
                false ->
                    logger:warning(#{event => scope_denied,
                                     method => Method,
                                     path => Path,
                                     required_scope => Scope}),
                    Body = jsx:encode(#{<<"error">> => <<"forbidden">>,
                                        <<"code">>  => <<"INSUFFICIENT_SCOPE">>,
                                        <<"required_scope">> => Scope}),
                    Req2 = cowboy_req:reply(403,
                        #{<<"content-type">> => <<"application/json">>},
                        Body, Req),
                    {stop, Req2, Env}
            end
    end.

required_scope(Method, Path) ->
    case has_prefix(Path, <<"/v1/admin">>) orelse
         has_prefix(Path, <<"/v1/tenants">>) orelse
         has_prefix(Path, <<"/v1/apikeys">>) of
        true ->
            <<"admin">>;
        false ->
            case has_prefix(Path, <<"/v1/events">>) of
                true ->
                    case Method of
                        <<"POST">> -> <<"events.publish">>;
                        <<"GET">>  -> <<"events.read">>;
                        _          -> undefined
                    end;
                false ->
                    required_scope_non_events(Method, Path)
            end
    end.

required_scope_non_events(Method, Path) ->
    case has_prefix(Path, <<"/v1/endpoints">>) of
        true ->
            case Method of
                <<"GET">> -> <<"endpoints.read">>;
                <<"POST">> -> <<"endpoints.write">>;
                <<"PATCH">> -> <<"endpoints.write">>;
                <<"DELETE">> -> <<"endpoints.write">>;
                _ -> undefined
            end;
        false ->
            required_scope_non_endpoints(Method, Path)
    end.

required_scope_non_endpoints(Method, Path) ->
    case has_prefix(Path, <<"/v1/subscriptions">>) of
        true ->
            case Method of
                <<"GET">> -> <<"subscriptions.read">>;
                <<"POST">> -> <<"subscriptions.write">>;
                <<"DELETE">> -> <<"subscriptions.write">>;
                _ -> undefined
            end;
        false ->
            required_scope_non_subscriptions(Method, Path)
    end.

required_scope_non_subscriptions(Method, Path) ->
    case has_prefix(Path, <<"/v1/deliveries">>) of
        true ->
            case Method of
                <<"GET">> -> <<"deliveries.read">>;
                _ -> undefined
            end;
        false ->
            required_scope_non_deliveries(Method, Path)
    end.

required_scope_non_deliveries(Method, Path) ->
    case has_prefix(Path, <<"/v1/replay">>) of
        true ->
            case Method of
                <<"POST">> -> <<"deliveries.retry">>;
                <<"GET">>  -> <<"deliveries.read">>;
                _          -> undefined
            end;
        false ->
            required_scope_non_replay(Method, Path)
    end.

required_scope_non_replay(Method, Path) ->
    case has_prefix(Path, <<"/v1/dlq">>) of
        true ->
            case Method of
                <<"GET">>    -> <<"dlq.read">>;
                <<"POST">>   -> <<"dlq.replay">>;
                <<"DELETE">> -> <<"dlq.replay">>;
                _            -> undefined
            end;
        false ->
            case has_prefix(Path, <<"/v1/stream">>) of
                true  -> <<"stream.subscribe">>;
                false -> undefined
            end
    end.

has_scope(Req, Scope) ->
    Scopes0 = maps:get(scopes, Req, [<<"*">>]),
    Scopes  = [normalize_scope(S) || S <- Scopes0, is_binary(S)],
    Required = normalize_scope(Scope),
    lists:any(fun(S) -> scope_matches(Required, S) end, Scopes).

normalize_scope(Scope) when is_binary(Scope) ->
    binary:replace(Scope, <<":">>, <<".">>, [global]).

scope_matches(_Required, <<"*">>) -> true;
scope_matches(Required, Provided) when Required =:= Provided -> true;
scope_matches(Required, Provided) ->
    case {split_scope(Required), split_scope(Provided)} of
        {{Res, _}, {Res, <<"manage">>}} -> true;
        {{Res, <<"publish">>}, {Res, <<"write">>}} -> true;
        {{Res, <<"retry">>}, {Res, <<"write">>}} -> true;
        {{Res, <<"replay">>}, {Res, <<"write">>}} -> true;
        _ -> false
    end.

split_scope(Scope) when is_binary(Scope) ->
    case binary:split(Scope, <<".">>, [global]) of
        [Res, Act] -> {Res, Act};
        _ -> undefined
    end.

has_prefix(Path, Prefix) when is_binary(Path), is_binary(Prefix) ->
    PfxSize = byte_size(Prefix),
    case byte_size(Path) >= PfxSize of
        true  -> binary:part(Path, 0, PfxSize) =:= Prefix;
        false -> false
    end.

%% Get scopes for a token (api_key mode only).
%% service_token callers receive scopes directly in authenticate/4:
%%   HL_EMBEDDED_MODE=true  -> data-plane scope allowlist
%%   otherwise              -> [<<"*">>]
%%   HL_ADMIN_KEY  → [<<"admin">>, <<"*">>]  (all scopes + admin-only ops)
%%   HL_API_KEY    → [<<"*">>]               (all scopes, backward compat)
%%   dynamic key   → whatever was set at creation
get_scopes(Token) ->
    AdminKey = list_to_binary(hl_config:get_str("HL_ADMIN_KEY", "")),
    EnvKey   = list_to_binary(hl_config:get_str("HL_API_KEY", "")),
    if
        AdminKey =/= <<>> andalso Token =:= AdminKey ->
            [<<"admin">>, <<"*">>];
        EnvKey =/= <<>> andalso Token =:= EnvKey ->
            [<<"*">>];
        true ->
            case hl_api_key_store:lookup_scopes(Token) of
                []     -> [];
                Scopes -> Scopes
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

reply_missing_tenant(Req, Env) ->
    logger:warning(#{event => missing_tenant_header,
                     path  => cowboy_req:path(Req)}),
    Body = jsx:encode(#{<<"error">> => <<"missing_tenant">>,
                        <<"code">>  => <<"X_TENANT_ID_REQUIRED">>,
                        <<"message">> =>
                            <<"X-Tenant-Id header is required in service_token auth mode">>}),
    Req2 = cowboy_req:reply(400,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req),
    {stop, Req2, Env}.

ensure_service_tenant(TenantId) ->
    case catch hl_tenant_store:create(TenantId, TenantId) of
        {ok, _} ->
            %% New tenant appeared via service_token + X-Tenant-Id.
            %% Warm runtime state immediately so first post-restart publish routes.
            _ = catch hl_subscription_cache:load_tenant_sync(TenantId),
            _ = catch hl_tenant_manager:load_tenant_sync(TenantId),
            ok;
        {error, already_exists} ->
            ok;
        {error, Reason} ->
            {error, Reason};
        {'EXIT', _} ->
            %% During startup races (store not fully up), avoid hard auth failures.
            ok;
        _ ->
            ok
    end.

reply_tenant_bootstrap_failed(Req, Env, TenantId, Reason) ->
    logger:error(#{event => tenant_bootstrap_failed,
                   path => cowboy_req:path(Req),
                   tenant_id => TenantId,
                   reason => Reason}),
    Body = jsx:encode(#{<<"error">> => <<"tenant_unavailable">>,
                        <<"code">>  => <<"TENANT_BOOTSTRAP_FAILED">>,
                        <<"tenant_id">> => TenantId}),
    Req2 = cowboy_req:reply(503,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req),
    {stop, Req2, Env}.
