-module(gp_dev_inbox_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gp_dev_inbox_sup:start_link().

stop(_State) ->
    ok.
