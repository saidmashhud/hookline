-module(hl_api_h_subscriptions).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle(Method, Req0, Opts).

handle(<<"POST">>, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    TId = get_tenant(Req1),
    case jsx:decode(Body, [return_maps]) of
        Map when is_map(Map) ->
            case hl_core_schema:validate_subscription(Map#{<<"tenant_id">> => TId}) of
                ok ->
                    SubId = hl_core_uuid:generate_str(),
                    Now = erlang:system_time(millisecond),
                    Sub = Map#{
                        <<"id">>              => SubId,
                        <<"subscription_id">> => SubId,
                        <<"tenant_id">>       => TId,
                        <<"created_at">>      => Now
                    },
                    case hl_store_client:put_subscription(Sub) of
                        {ok, _} ->
                            hl_subscription_cache:add(Sub),
                            reply_json(201, Sub, Req1, Opts);
                        {error, R} ->
                            hl_api_error:reply(Req1, 500, store_error,
                                list_to_binary(io_lib:format("~p", [R]))),
                            {ok, Req1, Opts}
                    end;
                {error, {missing_field, F}} ->
                    hl_api_error:reply(Req1, 400, validation_error,
                        <<"Missing: ", F/binary>>),
                    {ok, Req1, Opts}
            end;
        _ ->
            hl_api_error:reply(Req1, 400, invalid_json, <<>>),
            {ok, Req1, Opts}
    end;

handle(<<"GET">>, Req0, Opts) ->
    TId = get_tenant(Req0),
    QS  = cowboy_req:parse_qs(Req0),
    FilterEpId = proplists:get_value(<<"endpoint_id">>, QS, undefined),
    case hl_store_client:list_subscriptions(TId) of
        {ok, #{<<"items">> := Items} = Resp} ->
            Filtered = case FilterEpId of
                undefined -> Items;
                EpId ->
                    lists:filter(fun(S) ->
                        maps:get(<<"endpoint_id">>, S, undefined) =:= EpId
                    end, Items)
            end,
            reply_json(200, Resp#{<<"items">> => Filtered}, Req0, Opts);
        {ok, Resp} -> reply_json(200, Resp, Req0, Opts);
        {error, R}  ->
            hl_api_error:reply(Req0, 500, store_error,
                list_to_binary(io_lib:format("~p", [R]))),
            {ok, Req0, Opts}
    end;

handle(<<"DELETE">>, Req0, Opts) ->
    TId = get_tenant(Req0),
    case cowboy_req:binding(id, Req0) of
        undefined ->
            hl_api_error:reply(Req0, 400, validation_error, <<"subscription_id required">>),
            {ok, Req0, Opts};
        SubId ->
            case hl_store_client:delete_subscription(TId, SubId) of
                {ok, _} ->
                    hl_subscription_cache:remove(TId, SubId),
                    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
                    {ok, Req, Opts};
                {error, <<"not found">>} ->
                    hl_api_error:reply(Req0, 404, not_found, <<"Subscription not found">>),
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
