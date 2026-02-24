-module(hl_api_h_endpoints).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle(Method, Req0, Opts).

%% POST /v1/endpoints
handle(<<"POST">>, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    TId = get_tenant(Req1),
    case jsx:decode(Body, [return_maps]) of
        Map when is_map(Map) ->
            case hl_core_schema:validate_endpoint(Map#{<<"tenant_id">> => TId}) of
                ok ->
                    EndpointId = hl_core_uuid:generate_str(),
                    Now = erlang:system_time(millisecond),
                    Ep = Map#{
                        <<"id">>          => EndpointId,
                        <<"endpoint_id">> => EndpointId,
                        <<"tenant_id">>   => TId,
                        <<"created_at">>  => Now
                    },
                    Data = #{<<"cmd">>         => <<"store.put_endpoint">>,
                             <<"endpoint_id">> => EndpointId,
                             <<"payload">>     => jsx:encode(Ep),
                             <<"tenant_id">>   => TId},
                    case hl_store_client:put_endpoint(Data) of
                        {ok, _} ->
                            hl_tenant_manager:start_actor(Ep),
                            reply_json(201, Ep, Req1, Opts);
                        {error, R} ->
                            hl_api_error:reply(Req1, 500, store_error,
                                list_to_binary(io_lib:format("~p", [R]))),
                            {ok, Req1, Opts}
                    end;
                {error, {missing_field, F}} ->
                    hl_api_error:reply(Req1, 400, validation_error,
                        <<"Missing field: ", F/binary>>),
                    {ok, Req1, Opts};
                {error, {invalid_field, F, Reason}} ->
                    Msg = <<"Invalid field '", F/binary, "': ", Reason/binary>>,
                    hl_api_error:reply(Req1, 400, validation_error, Msg),
                    {ok, Req1, Opts};
                {error, _} ->
                    hl_api_error:reply(Req1, 400, validation_error, <<"Invalid endpoint data">>),
                    {ok, Req1, Opts}
            end;
        _ ->
            hl_api_error:reply(Req1, 400, invalid_json, <<>>),
            {ok, Req1, Opts}
    end;

%% GET /v1/endpoints  or  GET /v1/endpoints/:id
handle(<<"GET">>, Req0, Opts) ->
    TId = get_tenant(Req0),
    case cowboy_req:binding(id, Req0) of
        undefined ->
            case hl_store_client:list_endpoints(TId) of
                {ok, Resp} -> reply_json(200, Resp, Req0, Opts);
                {error, R}  ->
                    hl_api_error:reply(Req0, 500, store_error,
                        list_to_binary(io_lib:format("~p", [R]))),
                    {ok, Req0, Opts}
            end;
        EndpointId ->
            case hl_store_client:get_endpoint(TId, EndpointId) of
                {ok, #{<<"endpoint">> := EpRaw}} ->
                    Ep = case EpRaw of
                        B when is_binary(B) -> jsx:decode(B, [return_maps]);
                        M when is_map(M)    -> M
                    end,
                    reply_json(200, Ep, Req0, Opts);
                {error, <<"not found">>} ->
                    hl_api_error:reply(Req0, 404, not_found, <<"Endpoint not found">>),
                    {ok, Req0, Opts};
                {error, R} ->
                    hl_api_error:reply(Req0, 500, store_error,
                        list_to_binary(io_lib:format("~p", [R]))),
                    {ok, Req0, Opts}
            end
    end;

%% PATCH /v1/endpoints/:id  — merge fields into existing endpoint
handle(<<"PATCH">>, Req0, Opts) ->
    EndpointId = cowboy_req:binding(id, Req0),
    if EndpointId =:= undefined ->
        hl_api_error:reply(Req0, 400, validation_error, <<"endpoint id required">>),
        {ok, Req0, Opts};
    true ->
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        TId = get_tenant(Req1),
        case jsx:decode(Body, [return_maps]) of
            Delta when is_map(Delta) ->
                case hl_store_client:get_endpoint(TId, EndpointId) of
                    {ok, #{<<"endpoint">> := EpRaw}} ->
                        Existing = case EpRaw of
                            B when is_binary(B) -> jsx:decode(B, [return_maps]);
                            M when is_map(M)    -> M
                        end,
                        %% Merge: existing fields overridden by delta (except protected fields)
                        Protected = [<<"endpoint_id">>, <<"tenant_id">>, <<"created_at">>],
                        SafeDelta = maps:without(Protected, Delta),
                        Updated   = maps:merge(Existing, SafeDelta#{<<"updated_at">> =>
                                                erlang:system_time(millisecond)}),
                        Data = #{<<"cmd">>         => <<"store.put_endpoint">>,
                                 <<"endpoint_id">> => EndpointId,
                                 <<"payload">>     => jsx:encode(Updated),
                                 <<"tenant_id">>   => TId},
                        case hl_store_client:put_endpoint(Data) of
                            {ok, _} ->
                                hl_tenant_manager:update_actor(Updated),
                                reply_json(200, Updated, Req1, Opts);
                            {error, R} ->
                                hl_api_error:reply(Req1, 500, store_error,
                                    list_to_binary(io_lib:format("~p", [R]))),
                                {ok, Req1, Opts}
                        end;
                    {error, <<"not found">>} ->
                        hl_api_error:reply(Req1, 404, not_found, <<"Endpoint not found">>),
                        {ok, Req1, Opts};
                    {error, R} ->
                        hl_api_error:reply(Req1, 500, store_error,
                            list_to_binary(io_lib:format("~p", [R]))),
                        {ok, Req1, Opts}
                end;
            _ ->
                hl_api_error:reply(Req1, 400, invalid_json, <<>>),
                {ok, Req1, Opts}
        end
    end;

%% DELETE /v1/endpoints/:id
handle(<<"DELETE">>, Req0, Opts) ->
    TId = get_tenant(Req0),
    case cowboy_req:binding(id, Req0) of
        undefined ->
            hl_api_error:reply(Req0, 400, validation_error, <<"endpoint id required">>),
            {ok, Req0, Opts};
        EndpointId ->
            case hl_store_client:delete_endpoint(TId, EndpointId) of
                {ok, _} ->
                    hl_tenant_manager:stop_actor(EndpointId),
                    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
                    {ok, Req, Opts};
                {error, <<"not found">>} ->
                    hl_api_error:reply(Req0, 404, not_found, <<"Endpoint not found">>),
                    {ok, Req0, Opts};
                {error, R} ->
                    hl_api_error:reply(Req0, 500, store_error,
                        list_to_binary(io_lib:format("~p", [R]))),
                    {ok, Req0, Opts}
            end
    end;

handle(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(hl_config:get_str("HL_TENANT_ID", "default"))).

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
