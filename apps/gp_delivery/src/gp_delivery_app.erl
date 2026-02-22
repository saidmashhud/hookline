-module(gp_delivery_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Init ETS tables used by rate limiter and semaphore
    gp_delivery_rate:init(),
    gp_delivery_sem:init(),
    %% Register Prometheus metrics
    gp_delivery_metrics:init(),
    gp_delivery_sup:start_link().

stop(_State) ->
    ok.
