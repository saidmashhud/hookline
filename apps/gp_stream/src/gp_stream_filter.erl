-module(gp_stream_filter).
-export([matches/2]).

%% Delegate to gp_core_topic for pattern matching
matches(Pattern, Topic) ->
    gp_core_topic:matches(Pattern, Topic).
