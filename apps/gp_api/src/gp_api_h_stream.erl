-module(gp_api_h_stream).
-export([init/2, info/3, terminate/3]).

init(Req0, Opts) ->
    QS       = cowboy_req:parse_qs(Req0),
    Topic    = proplists:get_value(<<"topic">>, QS, <<"#">>),
    TenantId = get_tenant(Req0),
    %% Check Last-Event-ID for resume
    LastEventId = cowboy_req:header(<<"last-event-id">>, Req0, undefined),

    Req1 = cowboy_req:stream_reply(200, #{
        <<"content-type">>  => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">>    => <<"keep-alive">>
    }, Req0),

    %% Initial keepalive comment
    cowboy_req:stream_body(<<":\n\n">>, nofin, Req1),

    %% Schedule periodic keepalives (30s)
    erlang:send_after(30000, self(), keepalive),

    %% Subscribe before replay to avoid missing events during catch-up
    gp_stream_pubsub:subscribe(TenantId, Topic, self()),
    gp_delivery_metrics:inc_stream_clients(),

    %% Schedule replay of missed events if resuming
    case LastEventId of
        undefined -> ok;
        <<>>      -> ok;
        _         -> self() ! {replay_missed, LastEventId}
    end,

    {cowboy_loop, Req1, Opts#{tenant_id => TenantId, topic => Topic}}.

info({replay_missed, AfterEventId}, Req, Opts) ->
    TenantId = maps:get(tenant_id, Opts),
    Topic    = maps:get(topic, Opts, <<"#">>),
    case gp_store_client:list_events(TenantId, #{
            <<"after_id">> => AfterEventId,
            <<"limit">>    => 200
         }) of
        {ok, #{<<"items">> := Items}} ->
            lists:foreach(fun(Event) ->
                ETopic = maps:get(<<"topic">>, Event, <<>>),
                case gp_core_topic:matches(Topic, ETopic) of
                    true  -> send_sse_event(Event, Req);
                    false -> ok
                end
            end, Items);
        _ -> ok
    end,
    {ok, Req, Opts};

info({gp_stream, _TenantId, Event}, Req, Opts) ->
    Topic   = maps:get(<<"topic">>, Event, <<>>),
    Pattern = maps:get(topic, Opts, <<"#">>),
    case gp_core_topic:matches(Pattern, Topic) of
        true  ->
            send_sse_event(Event, Req),
            TId = maps:get(tenant_id, Opts, <<>>),
            gp_delivery_metrics:inc_stream_events(TId, Topic);
        false -> ok
    end,
    {ok, Req, Opts};

info(keepalive, Req, Opts) ->
    cowboy_req:stream_body(<<":\n\n">>, nofin, Req),
    erlang:send_after(30000, self(), keepalive),
    {ok, Req, Opts};

info(_Msg, Req, Opts) ->
    {ok, Req, Opts}.

terminate(_Reason, _Req, #{tenant_id := TId, topic := Topic}) ->
    gp_stream_pubsub:unsubscribe(TId, Topic, self()),
    gp_delivery_metrics:dec_stream_clients(),
    ok;
terminate(_Reason, _Req, _Opts) ->
    ok.

send_sse_event(Event, Req) ->
    EventId = maps:get(<<"event_id">>, Event,
              maps:get(<<"id">>, Event, <<>>)),
    Topic   = maps:get(<<"topic">>, Event, <<>>),
    Data    = jsx:encode(Event),
    IdLine  = case EventId of
        <<>> -> [];
        Id   -> [<<"id: ">>, Id, <<"\n">>]
    end,
    EventLine = case Topic of
        <<>> -> [];
        T    -> [<<"event: ">>, T, <<"\n">>]
    end,
    Frame = iolist_to_binary([IdLine, EventLine, <<"data: ">>, Data, <<"\n\n">>]),
    cowboy_req:stream_body(Frame, nofin, Req).

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(gp_config:get_str("GP_TENANT_ID", "default"))).
