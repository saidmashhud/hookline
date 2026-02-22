%% Event filter DSL.
%%
%% Supported operators (all map-based):
%%   #{<<"eq">>       => Path, <<"value">> => Val}
%%   #{<<"neq">>      => Path, <<"value">> => Val}
%%   #{<<"gt">>       => Path, <<"value">> => Val}
%%   #{<<"gte">>      => Path, <<"value">> => Val}
%%   #{<<"lt">>       => Path, <<"value">> => Val}
%%   #{<<"lte">>      => Path, <<"value">> => Val}
%%   #{<<"contains">> => Path, <<"value">> => Val}
%%   #{<<"and">>      => [Filter, ...]}
%%   #{<<"or">>       => [Filter, ...]}
%%   #{<<"not">>      => Filter}
%%
%% Path is a dot-separated binary: <<"data.amount">>
-module(gp_core_filter).
-export([matches/2]).

matches(undefined, _Event) -> true;
matches(null, _Event)      -> true;
matches(Filter, Event) when is_map(Filter) ->
    eval(Filter, Event).

eval(#{<<"eq">> := Path, <<"value">> := Val}, Event) ->
    get_path(Path, Event) =:= Val;
eval(#{<<"neq">> := Path, <<"value">> := Val}, Event) ->
    get_path(Path, Event) =/= Val;
eval(#{<<"gt">> := Path, <<"value">> := Val}, Event) ->
    V = get_path(Path, Event),
    is_number(V) andalso is_number(Val) andalso V > Val;
eval(#{<<"gte">> := Path, <<"value">> := Val}, Event) ->
    V = get_path(Path, Event),
    is_number(V) andalso is_number(Val) andalso V >= Val;
eval(#{<<"lt">> := Path, <<"value">> := Val}, Event) ->
    V = get_path(Path, Event),
    is_number(V) andalso is_number(Val) andalso V < Val;
eval(#{<<"lte">> := Path, <<"value">> := Val}, Event) ->
    V = get_path(Path, Event),
    is_number(V) andalso is_number(Val) andalso V =< Val;
eval(#{<<"contains">> := Path, <<"value">> := Val}, Event) ->
    V = get_path(Path, Event),
    is_binary(V) andalso is_binary(Val) andalso
        binary:match(V, Val) =/= nomatch;
eval(#{<<"and">> := Filters}, Event) ->
    lists:all(fun(F) -> eval(F, Event) end, Filters);
eval(#{<<"or">> := Filters}, Event) ->
    lists:any(fun(F) -> eval(F, Event) end, Filters);
eval(#{<<"not">> := Filter}, Event) ->
    not eval(Filter, Event);
eval(_, _) ->
    true.

get_path(Path, Map) when is_binary(Path), is_map(Map) ->
    Parts = binary:split(Path, <<".">>, [global]),
    lists:foldl(fun
        (Key, M) when is_map(M) -> maps:get(Key, M, undefined);
        (_,   _)                -> undefined
    end, Map, Parts);
get_path(_, _) ->
    undefined.
