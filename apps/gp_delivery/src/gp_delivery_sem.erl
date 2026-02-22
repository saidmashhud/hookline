%% Per-endpoint in-flight semaphore.
%% Enforces max_in_flight from endpoint config.
-module(gp_delivery_sem).
-export([init/0, acquire/2, release/1]).

-define(TABLE, gp_delivery_sem_ets).

init() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set,
                             {write_concurrency, true}]);
        _ -> ?TABLE
    end.

%% Atomically increment counter; fail if over limit.
%% Returns ok | {error, at_capacity}
acquire(EndpointId, MaxInFlight) ->
    Key = {inflight, EndpointId},
    Count = ets:update_counter(?TABLE, Key, {2, 1}, {Key, 0}),
    if
        Count =< MaxInFlight ->
            ok;
        true ->
            ets:update_counter(?TABLE, Key, {2, -1}, {Key, 0}),
            {error, at_capacity}
    end.

%% Decrement counter on completion (success or failure).
release(EndpointId) ->
    Key = {inflight, EndpointId},
    %% Guard against going below 0
    ets:update_counter(?TABLE, Key, {2, -1, 0, 0}, {Key, 0}),
    ok.
