-module(gp_config).
-export([get_str/2, get_int/2, get_bool/2]).

get_str(EnvKey, Default) when is_binary(EnvKey) ->
    get_str(binary_to_list(EnvKey), Default);
get_str(EnvKey, Default) when is_list(EnvKey) ->
    case os:getenv(EnvKey) of
        false -> Default;
        Val   -> Val
    end.

get_int(EnvKey, Default) ->
    case get_str(EnvKey, undefined) of
        undefined -> Default;
        Val       ->
            try list_to_integer(Val)
            catch _:_ -> Default
            end
    end.

get_bool(EnvKey, Default) ->
    case get_str(EnvKey, undefined) of
        undefined -> Default;
        "true"    -> true;
        "1"       -> true;
        "false"   -> false;
        "0"       -> false;
        _         -> Default
    end.
