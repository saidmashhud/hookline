-module(gp_core_topic).
-export([matches/2]).

%% matches(Pattern, Topic) -> boolean()
%% Pattern supports:
%%   "orders.created"  — exact match
%%   "orders.*"        — single-segment wildcard
%%   "orders.#"        — multi-segment wildcard (AMQP-style)
%%   "*"               — any single segment
%%   "#"               — any topic

matches(Pattern, Topic) when is_binary(Pattern), is_binary(Topic) ->
    PatParts   = binary:split(Pattern, <<".">>, [global]),
    TopicParts = binary:split(Topic,   <<".">>, [global]),
    match_parts(PatParts, TopicParts).

match_parts([], []) -> true;
match_parts([<<"#">>], _) -> true;
match_parts([<<"#">> | _], []) -> false;
match_parts([<<"#">> | PRest], [_ | TRest]) ->
    %% # can consume zero or more segments
    match_parts([<<"#">> | PRest], TRest) orelse
    match_parts(PRest, TRest);
match_parts([<<"*">>  | PRest], [_ | TRest]) ->
    match_parts(PRest, TRest);
match_parts([P | PRest], [T | TRest]) when P =:= T ->
    match_parts(PRest, TRest);
match_parts(_, _) -> false.
