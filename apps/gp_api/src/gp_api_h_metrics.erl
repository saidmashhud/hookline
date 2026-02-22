-module(gp_api_h_metrics).
-export([init/2]).

init(Req0, Opts) ->
    Body = prometheus_text_format:format(),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain; version=0.0.4; charset=utf-8">>},
        Body, Req0),
    {ok, Req, Opts}.
