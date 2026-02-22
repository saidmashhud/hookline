-module(gp_core_retry).
-export([backoff_secs/2, max_attempts/0]).

-define(BASE_DELAY,   1).    %% seconds
-define(MAX_DELAY,    3600). %% 1 hour cap
-define(MAX_ATTEMPTS, 10).   %% default per TZ

max_attempts() ->
    gp_config:get_int("GP_RETRY_MAX_ATTEMPTS", ?MAX_ATTEMPTS).

%% Exponential backoff with full jitter
%% delay = random(0, min(cap, base * 2^attempt))
backoff_secs(Attempt, _Opts) when Attempt =< 0 ->
    0;
backoff_secs(Attempt, _Opts) ->
    Cap   = min(?MAX_DELAY, ?BASE_DELAY * (1 bsl min(Attempt - 1, 30))),
    trunc(rand:uniform() * Cap).
