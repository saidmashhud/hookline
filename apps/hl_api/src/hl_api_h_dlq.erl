-module(hl_api_h_dlq).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Action = maps:get(action, Opts, undefined),
    handle(Method, Action, Req0, Opts).

handle(<<"GET">>, _Action, Req0, Opts) ->
    TId = get_tenant(Req0),
    QS  = cowboy_req:parse_qs(Req0),
    FilterEpId = proplists:get_value(<<"endpoint_id">>, QS, undefined),
    case hl_store_client:list_dlq(TId) of
        {ok, #{<<"items">> := Items} = Resp} ->
            Filtered = case FilterEpId of
                undefined -> Items;
                EpId ->
                    lists:filter(fun(E) ->
                        maps:get(<<"endpoint_id">>, E, undefined) =:= EpId
                    end, Items)
            end,
            reply_json(200, Resp#{<<"items">> => Filtered}, Req0, Opts);
        {ok, Resp} -> reply_json(200, Resp, Req0, Opts);
        {error, R} ->
            hl_api_error:reply(Req0, 500, store_error,
                list_to_binary(io_lib:format("~p", [R]))),
            {ok, Req0, Opts}
    end;

handle(<<"POST">>, requeue, Req0, Opts) ->
    %% Requeue a DLQ entry
    TId = get_tenant(Req0),
    case cowboy_req:binding(id, Req0) of
        undefined ->
            hl_api_error:reply(Req0, 400, missing_id, <<"job_id required">>),
            {ok, Req0, Opts};
        JobId ->
            case hl_store_client:requeue_dlq(TId, JobId) of
                {ok, _} -> reply_json(200, #{<<"ok">> => true}, Req0, Opts);
                {error, <<"not found">>} ->
                    hl_api_error:reply(Req0, 404, not_found, <<"DLQ entry not found">>),
                    {ok, Req0, Opts};
                {error, R} ->
                    hl_api_error:reply(Req0, 500, store_error,
                        list_to_binary(io_lib:format("~p", [R]))),
                    {ok, Req0, Opts}
            end
    end;

handle(<<"DELETE">>, _Action, Req0, Opts) ->
    TId = get_tenant(Req0),
    case cowboy_req:binding(id, Req0) of
        undefined ->
            hl_api_error:reply(Req0, 400, validation_error, <<"job_id required">>),
            {ok, Req0, Opts};
        JobId ->
            case hl_store_client:delete_dlq(TId, JobId) of
                {ok, _} ->
                    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
                    {ok, Req, Opts};
                {error, _} ->
                    hl_api_error:reply(Req0, 404, not_found, <<"DLQ entry not found">>),
                    {ok, Req0, Opts}
            end
    end;

handle(_, _, Req0, Opts) ->
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
