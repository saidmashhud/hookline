-module(gp_api_h_deliveries).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle(Method, Req0, Opts).

handle(<<"GET">>, Req0, Opts) ->
    case cowboy_req:binding(id, Req0) of
        undefined -> handle_list(Req0, Opts);
        AttemptId -> handle_single(AttemptId, Req0, Opts)
    end;

handle(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

handle_single(AttemptId, Req0, Opts) ->
    case gp_store_client:list_attempts(#{<<"attempt_id">> => AttemptId, <<"limit">> => 1}) of
        {ok, #{<<"items">> := [Item | _]}} ->
            reply_json(200, Item, Req0, Opts);
        {ok, #{<<"items">> := []}} ->
            gp_api_error:reply(Req0, 404, not_found, <<"Attempt not found">>),
            {ok, Req0, Opts};
        {error, R} ->
            gp_api_error:reply(Req0, 500, store_error,
                list_to_binary(io_lib:format("~p", [R]))),
            {ok, Req0, Opts}
    end.

handle_list(Req0, Opts) ->
    QS         = cowboy_req:parse_qs(Req0),
    JobId      = proplists:get_value(<<"job_id">>,      QS, <<>>),
    EventId    = proplists:get_value(<<"event_id">>,    QS, <<>>),
    EndpointId = proplists:get_value(<<"endpoint_id">>, QS, <<>>),
    Limit      = qs_int(QS, <<"limit">>, 50),

    Filter = maps:filter(fun(_, V) -> V =/= <<>> end, #{
        <<"job_id">>      => JobId,
        <<"event_id">>    => EventId,
        <<"endpoint_id">> => EndpointId,
        <<"limit">>       => Limit
    }),

    case gp_store_client:list_attempts(Filter) of
        {ok, #{<<"items">> := Items}} ->
            reply_json(200, #{<<"items">> => Items, <<"count">> => length(Items)},
                       Req0, Opts);
        {error, R} ->
            gp_api_error:reply(Req0, 500, store_error,
                list_to_binary(io_lib:format("~p", [R]))),
            {ok, Req0, Opts}
    end.

qs_int(QS, Key, Default) ->
    case proplists:get_value(Key, QS) of
        undefined -> Default;
        V -> try binary_to_integer(V) catch _:_ -> Default end
    end.

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
