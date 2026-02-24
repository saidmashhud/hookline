-module(hl_api_h_replay).
-export([init/2, init_table/0]).

-define(REPLAY_TABLE, hl_replay_results).

init_table() ->
    case ets:info(?REPLAY_TABLE) of
        undefined ->
            ets:new(?REPLAY_TABLE, [named_table, public, {read_concurrency, true}]);
        _ -> ok
    end.

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle(Method, Req0, Opts).

%% POST /v1/replay
%% Body: {event_id} | {from_ms, to_ms, topic, endpoint_id, mode, limit}
%% mode: "all" (default) | "failed_only"
handle(<<"POST">>, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    TId = get_tenant(Req1),
    case jsx:decode(Body, [return_maps]) of
        #{<<"event_id">> := EventId} = Params ->
            EndpointId = maps:get(<<"endpoint_id">>, Params, undefined),
            replay_single(TId, EventId, EndpointId, Req1, Opts);
        Params when is_map(Params) ->
            replay_batch(TId, Params, Req1, Opts);
        _ ->
            hl_api_error:reply(Req1, 400, validation_error,
                <<"event_id or time range required">>),
            {ok, Req1, Opts}
    end;

%% GET /v1/replay/:id — status of a past replay
handle(<<"GET">>, Req0, Opts) ->
    case cowboy_req:binding(id, Req0) of
        undefined ->
            reply_json(200, #{<<"items">> => []}, Req0, Opts);
        ReplayId ->
            case ets:lookup(?REPLAY_TABLE, ReplayId) of
                [{_, Result}] ->
                    reply_json(200, Result, Req0, Opts);
                [] ->
                    hl_api_error:reply(Req0, 404, not_found, <<"Replay not found">>),
                    {ok, Req0, Opts}
            end
    end;

handle(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

replay_single(TId, EventId, EndpointId, Req, Opts) ->
    case hl_store_client:get_event(TId, EventId) of
        {ok, #{<<"event">> := EventRaw}} ->
            Event = decode_json(EventRaw),
            do_replay_events(TId, [Event], EndpointId, Req, Opts);
        {error, _} ->
            hl_api_error:reply(Req, 404, not_found, <<"Event not found">>),
            {ok, Req, Opts}
    end.

replay_batch(TId, Params, Req, Opts) ->
    FromMs     = maps:get(<<"from_ms">>,     Params, 0),
    ToMs       = maps:get(<<"to_ms">>,       Params, 0),
    Topic      = maps:get(<<"topic">>,       Params, undefined),
    EndpointId = maps:get(<<"endpoint_id">>, Params, undefined),
    Mode       = maps:get(<<"mode">>,        Params, <<"all">>),
    Limit      = maps:get(<<"limit">>,       Params, 100),

    StoreOpts = maps:filter(fun(_, V) -> V =/= 0 andalso V =/= undefined end, #{
        <<"from_ms">> => FromMs,
        <<"to_ms">>   => ToMs,
        <<"limit">>   => Limit
    }),

    case hl_store_client:list_events(TId, StoreOpts) of
        {ok, #{<<"items">> := AllEvents}} ->
            Events1 = case Topic of
                undefined -> AllEvents;
                P -> [E || E <- AllEvents,
                           hl_core_topic:matches(P, maps:get(<<"topic">>, E, <<>>))]
            end,
            Events2 = case Mode of
                <<"failed_only">> ->
                    %% Only replay events that have DLQ entries
                    DlqIds = dlq_event_ids(TId),
                    [E || E <- Events1,
                          sets:is_element(
                              maps:get(<<"event_id">>, E,
                                       maps:get(<<"id">>, E, <<>>)),
                              DlqIds)];
                _ ->
                    Events1
            end,
            do_replay_events(TId, Events2, EndpointId, Req, Opts);
        {error, R} ->
            hl_api_error:reply(Req, 500, store_error, iobin(R)),
            {ok, Req, Opts}
    end.

dlq_event_ids(TId) ->
    case hl_store_client:list_dlq(TId) of
        {ok, #{<<"items">> := Items}} ->
            sets:from_list([maps:get(<<"event_id">>, I, <<>>) || I <- Items]);
        _ ->
            sets:new()
    end.

do_replay_events(TId, Events, EndpointId, Req, Opts) ->
    ReplayId = hl_core_uuid:generate_str(),
    Results = lists:map(fun(Event) ->
        EventId = maps:get(<<"event_id">>, Event, maps:get(<<"id">>, Event, <<>>)),
        case hl_core:route_event(TId, Event) of
            {ok, Subs} ->
                FilteredSubs = case EndpointId of
                    undefined -> Subs;
                    EpId ->
                        [S || S <- Subs,
                              maps:get(<<"endpoint_id">>, S, undefined) =:= EpId]
                end,
                {ok, JobIds} = hl_core:create_delivery_jobs(Event, FilteredSubs, TId),
                #{<<"event_id">> => EventId, <<"job_ids">> => JobIds};
            {error, R} ->
                #{<<"event_id">> => EventId, <<"error">> => iobin(R)}
        end
    end, Events),
    Resp = #{
        <<"replay_id">> => ReplayId,
        <<"replayed">>  => length(Results),
        <<"scheduled">> => length([R || R <- Results, not maps:is_key(<<"error">>, R)]),
        <<"results">>   => Results
    },
    %% Store result for GET /replay/:id
    init_table(),
    ets:insert(?REPLAY_TABLE, {ReplayId, Resp}),
    reply_json(200, Resp, Req, Opts).

decode_json(Raw) when is_binary(Raw) -> jsx:decode(Raw, [return_maps]);
decode_json(M) when is_map(M)        -> M.

iobin(R) -> list_to_binary(io_lib:format("~p", [R])).

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(hl_config:get_str("HL_TENANT_ID", "default"))).

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
