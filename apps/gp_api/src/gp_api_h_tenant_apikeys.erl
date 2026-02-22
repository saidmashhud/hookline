%% Handler for tenant-scoped API key management.
%%
%% Routes:
%%   GET    /v1/tenants/:tenant_id/api-keys            — list keys (admin)
%%   POST   /v1/tenants/:tenant_id/api-keys            — create key (admin)
%%   DELETE /v1/tenants/:tenant_id/api-keys/:key_id    — revoke key (admin)
%%
%% All operations require the "admin" scope.
-module(gp_api_h_tenant_apikeys).
-export([init/2]).

init(Req0, Opts) ->
    case gp_api_auth:require_scope(Req0, <<"admin">>) of
        ok ->
            Method = cowboy_req:method(Req0),
            handle(Method, Req0, Opts);
        {stop, _Code} ->
            {ok, Req0, Opts}
    end.

%% POST /v1/tenants/:tenant_id/api-keys
handle(<<"POST">>, Req0, Opts) ->
    TargetTenant = cowboy_req:binding(tenant_id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Params = decode_body(Body),
    Name   = maps:get(<<"name">>,   Params, <<"default">>),
    Scopes = valid_scopes(maps:get(<<"scopes">>, Params, [<<"*">>])),
    CallerTenantId = maps:get(tenant_id, Req1, <<"admin">>),
    case gp_api_key_store:put(TargetTenant, Name, Scopes, CallerTenantId) of
        {ok, Token, KeyId} ->
            Now = erlang:system_time(millisecond),
            reply_json(201, #{
                <<"id">>         => KeyId,
                <<"key">>        => Token,
                <<"name">>       => Name,
                <<"tenant_id">>  => TargetTenant,
                <<"scopes">>     => Scopes,
                <<"created_at">> => Now,
                <<"note">>       => <<"Store the key value — it will not be shown again.">>
            }, Req1, Opts)
    end;

%% GET /v1/tenants/:tenant_id/api-keys
handle(<<"GET">>, Req0, Opts) ->
    TargetTenant = cowboy_req:binding(tenant_id, Req0),
    Items = gp_api_key_store:list(TargetTenant),
    reply_json(200, #{<<"items">> => Items, <<"tenant_id">> => TargetTenant},
               Req0, Opts);

%% DELETE /v1/tenants/:tenant_id/api-keys/:key_id
handle(<<"DELETE">>, Req0, Opts) ->
    TargetTenant = cowboy_req:binding(tenant_id, Req0),
    KeyId        = cowboy_req:binding(key_id, Req0),
    case KeyId of
        undefined ->
            gp_api_error:reply(Req0, 400, validation_error, <<"key_id required">>),
            {ok, Req0, Opts};
        _ ->
            case gp_api_key_store:delete(TargetTenant, KeyId) of
                ok ->
                    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
                    {ok, Req, Opts};
                {error, not_found} ->
                    gp_api_error:reply(Req0, 404, not_found, <<"Key not found">>),
                    {ok, Req0, Opts};
                {error, forbidden} ->
                    gp_api_error:reply(Req0, 403, forbidden,
                                       <<"Key belongs to a different tenant">>),
                    {ok, Req0, Opts}
            end
    end;

handle(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

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
