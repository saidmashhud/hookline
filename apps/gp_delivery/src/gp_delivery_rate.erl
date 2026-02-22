%% Per-endpoint token-bucket rate limiting.
%% Rate (RPS) is read from the endpoint config, not hardcoded.
-module(gp_delivery_rate).
-export([init/0, check/2]).

-define(TABLE, gp_delivery_rate_ets).
-define(BURST_MULT, 2).   %% burst = rate * BURST_MULT

init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set,
                             {write_concurrency, true}]);
        _ -> ?TABLE
    end.

%% check(EndpointId, RateRPS) -> ok | {error, rate_limited}
%% Uses 1-second sliding window counters.
check(EndpointId, RateRPS) ->
    BurstLimit = RateRPS * ?BURST_MULT,
    Now   = erlang:system_time(millisecond),
    Window = 1000,
    Key    = {EndpointId, Now div Window},
    Count  = ets:update_counter(?TABLE, Key, {2, 1}, {Key, 0}),
    if
        Count =< BurstLimit -> ok;
        true -> {error, rate_limited}
    end.
