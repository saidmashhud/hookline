-module(gp_core_retry_test).
-include_lib("eunit/include/eunit.hrl").

zero_attempt_test() ->
    ?assertEqual(0, gp_core_retry:backoff_secs(0, #{})).

first_attempt_test() ->
    Delay = gp_core_retry:backoff_secs(1, #{}),
    ?assert(Delay >= 0),
    ?assert(Delay =< 1).  %% base=1, cap=1

backoff_grows_test() ->
    %% At attempt 10, delay should be larger than attempt 1
    D1  = gp_core_retry:backoff_secs(1, #{}),
    D10 = lists:max([gp_core_retry:backoff_secs(10, #{}) || _ <- lists:seq(1, 20)]),
    ?assert(D10 >= D1).

max_cap_test() ->
    %% Delay should never exceed max (3600s)
    lists:foreach(fun(N) ->
        D = gp_core_retry:backoff_secs(N, #{}),
        ?assert(D =< 3600)
    end, lists:seq(1, 30)).

max_attempts_test() ->
    ?assertEqual(10, gp_core_retry:max_attempts()).
