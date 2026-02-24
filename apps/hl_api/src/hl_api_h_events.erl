-module(hl_api_h_events).
-export([init/2]).

-define(MAX_PAYLOAD_BYTES, 524288).  %% 512 KB

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle(Method, Req0, Opts).

%% POST /v1/events
handle(<<"POST">>, Req0, Opts) ->
    MaxBytes = hl_config:get_int("HL_MAX_PAYLOAD_BYTES", ?MAX_PAYLOAD_BYTES),
    %% Read up to MaxBytes+1 so Cowboy returns {more,...} if body exceeds limit
    case cowboy_req:read_body(Req0, #{length => MaxBytes + 1}) of
        {more, _Body, Req1} ->
            %% Body larger than limit — reject immediately
            hl_api_error:reply(Req1, 413, payload_too_large,
                <<"Request body exceeds maximum allowed size">>),
            {ok, Req1, Opts};
        {ok, Body, Req1} when byte_size(Body) > MaxBytes ->
            hl_api_error:reply(Req1, 413, payload_too_large,
                <<"Request body exceeds maximum allowed size">>),
            {ok, Req1, Opts};
        {ok, Body, Req1} ->
            TId = get_tenant(Req1),
            %% Extract W3C Trace Context from inbound request for end-to-end propagation
            Traceparent = cowboy_req:header(<<"traceparent">>, Req1, <<>>),
            Tracestate  = cowboy_req:header(<<"tracestate">>, Req1, <<>>),
            handle_post(TId, Body, Req1, Opts, Traceparent, Tracestate)
    end;

%% GET /v1/events/:id  or  GET /v1/events?limit=N&cursor=...&from_ms=...&to_ms=...&topic=...
handle(<<"GET">>, Req0, Opts) ->
    TId = get_tenant(Req0),
    case cowboy_req:binding(id, Req0) of
        undefined ->
            QS      = cowboy_req:parse_qs(Req0),
            Limit   = qs_int(QS, <<"limit">>,   50),
            Cursor  = proplists:get_value(<<"cursor">>,   QS, <<>>),
            AfterId = proplists:get_value(<<"after_id">>, QS, <<>>),
            FromMs  = qs_int64(QS, <<"from_ms">>, 0),
            ToMs    = qs_int64(QS, <<"to_ms">>,   0),

            Opts2 = maps:filter(fun(_, V) -> V =/= <<>> andalso V =/= 0 end, #{
                <<"limit">>    => Limit,
                <<"cursor">>   => Cursor,
                <<"after_id">> => AfterId,
                <<"from_ms">>  => FromMs,
                <<"to_ms">>    => ToMs
            }),
            case hl_store_client:list_events(TId, Opts2) of
                {ok, Resp} ->
                    Items   = maps:get(<<"items">>, Resp, []),
                    Next    = maps:get(<<"next_cursor">>, Resp, undefined),
                    Body = case Next of
                        undefined -> #{<<"items">> => Items, <<"count">> => length(Items)};
                        NC        -> #{<<"items">> => Items, <<"count">> => length(Items),
                                       <<"next_cursor">> => NC}
                    end,
                    reply_json(200, Body, Req0, Opts);
                {error, R} ->
                    hl_api_error:reply(Req0, 500, store_error,
                        list_to_binary(io_lib:format("~p", [R]))),
                    {ok, Req0, Opts}
            end;

        EventId ->
            case hl_store_client:get_event(TId, EventId) of
                {ok, #{<<"event">> := EventJSON}} ->
                    Event = case EventJSON of
                        B when is_binary(B) -> jsx:decode(B, [return_maps]);
                        M when is_map(M)    -> M
                    end,
                    reply_json(200, Event, Req0, Opts);
                {error, <<"not found">>} ->
                    hl_api_error:reply(Req0, 404, not_found, <<"Event not found">>),
                    {ok, Req0, Opts};
                {error, Reason} ->
                    hl_api_error:reply(Req0, 500, internal_error,
                        list_to_binary(io_lib:format("~p", [Reason]))),
                    {ok, Req0, Opts}
            end
    end;

handle(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

%% Route POST to leader (local publish) or proxy to leader node.
handle_post(TenantId, Body, Req0, Opts, Traceparent, Tracestate) ->
    case hl_leader:is_leader() of
        true  -> handle_publish_local(TenantId, Body, Req0, Opts, Traceparent, Tracestate);
        false -> handle_publish_proxy(TenantId, Body, Req0, Opts)
    end.

handle_publish_local(TId, Body, Req1, Opts, Traceparent, Tracestate) ->
    case hl_core:check_queue_depth(TId) of
        {error, overloaded} ->
            Req2 = cowboy_req:set_resp_header(<<"retry-after">>, <<"5">>, Req1),
            hl_api_error:reply(Req2, 429, overloaded,
                <<"Queue depth exceeded; retry after 5 seconds">>),
            {ok, Req2, Opts};
        ok ->
            case jsx:decode(Body, [return_maps]) of
                Map when is_map(Map) ->
                    %% Inject trace context into event metadata for delivery propagation
                    Map2 = case Traceparent of
                        <<>> -> Map;
                        _    ->
                            TraceMap = case Tracestate of
                                <<>> -> #{<<"traceparent">> => Traceparent};
                                _    -> #{<<"traceparent">> => Traceparent,
                                          <<"tracestate">> => Tracestate}
                            end,
                            Map#{<<"_trace">> => TraceMap}
                    end,
                    case hl_core:publish_event(TId, Map2) of
                        {ok, Event} ->
                            reply_json(201, Event, Req1, Opts);
                        {ok, Event, deduped} ->
                            reply_json(200,
                                Event#{<<"deduped">> => true}, Req1, Opts);
                        {error, {missing_field, F}} ->
                            hl_api_error:reply(Req1, 400, validation_error,
                                <<"Missing field: ", F/binary>>),
                            {ok, Req1, Opts};
                        {error, Reason} ->
                            hl_api_error:reply(Req1, 500, internal_error,
                                list_to_binary(io_lib:format("~p", [Reason]))),
                            {ok, Req1, Opts}
                    end;
                _ ->
                    hl_api_error:reply(Req1, 400, invalid_json, <<>>),
                    {ok, Req1, Opts}
            end
    end.

handle_publish_proxy(TenantId, Body, Req0, Opts) ->
    case hl_leader_proxy:forward(TenantId, Body) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            Req1 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                RespBody, Req0),
            {ok, Req1, Opts};
        {ok, {{_, 201, _}, _, RespBody}} ->
            Req1 = cowboy_req:reply(201,
                #{<<"content-type">> => <<"application/json">>},
                RespBody, Req0),
            {ok, Req1, Opts};
        {ok, {{_, Status, _}, _, RespBody}} ->
            Req1 = cowboy_req:reply(Status, #{}, RespBody, Req0),
            {ok, Req1, Opts};
        {error, no_leader} ->
            Req1 = cowboy_req:reply(503, #{},
                <<"{\"error\":\"no_leader\"}">>, Req0),
            {ok, Req1, Opts};
        {error, _} ->
            Req1 = cowboy_req:reply(502, #{},
                <<"{\"error\":\"proxy_error\"}">>, Req0),
            {ok, Req1, Opts}
    end.

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(hl_config:get_str("HL_TENANT_ID", "default"))).

qs_int(QS, Key, Default) ->
    case proplists:get_value(Key, QS) of
        undefined -> Default;
        V -> try binary_to_integer(V) catch _:_ -> Default end
    end.

qs_int64(QS, Key, Default) ->
    case proplists:get_value(Key, QS) of
        undefined -> Default;
        <<>>      -> Default;
        V -> try binary_to_integer(V) catch _:_ -> Default end
    end.

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
