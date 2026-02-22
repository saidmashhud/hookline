-module(gp_api_h_health).
-export([init/2]).

init(Req0, #{check := liveness} = Opts) ->
    Body = jsx:encode(#{<<"status">> => <<"ok">>}),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, Opts};

init(Req0, #{check := readiness} = Opts) ->
    %% Check store connectivity
    Status = case gp_store_client:claim_jobs(0, 1) of
        {ok, _}    -> 200;
        {error, _} -> 503
    end,
    Body = case Status of
        200 -> jsx:encode(#{<<"status">> => <<"ready">>});
        503 -> jsx:encode(#{<<"status">> => <<"unavailable">>,
                            <<"reason">> => <<"store unreachable">>})
    end,
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, Opts}.
