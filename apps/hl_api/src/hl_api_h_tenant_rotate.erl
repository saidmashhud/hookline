%% POST /v1/tenants/:id/rotate-key
%%
%% Rotate an API key for a tenant with a configurable overlap window so
%% in-flight requests using the old token continue to succeed.
%%
%% Request body (JSON):
%%   {
%%     "key_id":          "<uuid>",     % required — key to retire
%%     "overlap_seconds": 3600          % optional — default 3600 (1 hour)
%%   }
%%
%% Response (200 OK):
%%   {
%%     "key_id":             "<new-uuid>",
%%     "token":              "sk_live_…",
%%     "old_key_id":         "<old-uuid>",
%%     "old_key_expires_at": <unix-seconds>
%%   }
%%
%% Behaviour:
%%   1. A fresh API key is created for the tenant (inherits [<<"*">>] scopes).
%%   2. The old key's hash is placed in hl_key_rotation with the computed
%%      expiry timestamp — auth middleware falls back to this store when
%%      the main store returns not_found, preserving the overlap window.
%%   3. After overlap_seconds the old key entry is removed from both the
%%      main store and the rotation store.
-module(hl_api_h_tenant_rotate).
-export([init/2]).

init(Req0, Opts) ->
    case hl_api_auth:require_scope(Req0, <<"admin">>) of
        ok ->
            TenantId = cowboy_req:binding(id, Req0),
            handle_post(TenantId, Req0, Opts);
        {stop, _} ->
            {ok, Req0, Opts}
    end.

handle_post(TenantId, Req0, Opts) ->
    case hl_tenant_store:exists(TenantId) of
        false ->
            hl_api_error:reply(Req0, 404, not_found, <<"Tenant not found">>),
            {ok, Req0, Opts};
        true ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            Params         = decode_body(Body),
            OldKeyId       = maps:get(<<"key_id">>, Params, undefined),
            OverlapSeconds = maps:get(<<"overlap_seconds">>, Params, 3600),
            rotate(TenantId, OldKeyId, OverlapSeconds, Req1, Opts)
    end.

rotate(_TenantId, undefined, _OverlapSeconds, Req0, Opts) ->
    hl_api_error:reply(Req0, 400, bad_request, <<"key_id is required">>),
    {ok, Req0, Opts};
rotate(TenantId, OldKeyId, OverlapSeconds, Req0, Opts) ->
    %% Create a new key before retiring the old one.
    case hl_api_key_store:put(TenantId, <<"rotated">>, [<<"*">>], <<"rotation">>) of
        {ok, NewToken, NewKeyId} ->
            ExpiresAt = erlang:system_time(millisecond) + OverlapSeconds * 1000,
            %% Register old key in the expiring overlap store.
            case hl_api_key_store:lookup_hash_by_id(OldKeyId) of
                {ok, OldHash} ->
                    hl_key_rotation:put_expiring(TenantId, OldHash, ExpiresAt);
                _ ->
                    ok
            end,
            %% Schedule hard-deletion of the old key after the overlap window.
            schedule_delete(TenantId, OldKeyId, OverlapSeconds),
            logger:info(#{event          => api_key_rotated,
                          tenant_id      => TenantId,
                          old_key_id     => OldKeyId,
                          new_key_id     => NewKeyId,
                          overlap_seconds => OverlapSeconds}),
            reply_json(200, #{
                <<"key_id">>             => NewKeyId,
                <<"token">>              => NewToken,
                <<"old_key_id">>         => OldKeyId,
                <<"old_key_expires_at">> => ExpiresAt div 1000
            }, Req0, Opts);

        {error, Reason} ->
            Msg = list_to_binary(io_lib:format("~p", [Reason])),
            hl_api_error:reply(Req0, 500, internal_error, Msg),
            {ok, Req0, Opts}
    end.

%% Spawn a one-shot process that deletes the old key after the overlap window.
schedule_delete(TenantId, KeyId, OverlapSeconds) ->
    DelayMs = OverlapSeconds * 1000,
    spawn(fun() ->
        timer:sleep(DelayMs),
        hl_api_key_store:delete(TenantId, KeyId),
        logger:info(#{event      => rotated_key_expired,
                      tenant_id  => TenantId,
                      key_id     => KeyId})
    end).

decode_body(<<>>) -> #{};
decode_body(B) ->
    case catch jsx:decode(B, [return_maps]) of
        M when is_map(M) -> M;
        _                -> #{}
    end.

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
