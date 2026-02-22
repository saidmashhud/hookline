-module(gp_core_schema).
-export([validate_event/1, validate_endpoint/1, validate_subscription/1]).

-define(REQUIRED(Map, Key), case maps:find(Key, Map) of
    {ok, V} when V =/= <<>>, V =/= null -> ok;
    _ -> {error, {missing_field, Key}}
end).

validate_event(Map) when is_map(Map) ->
    with_required(Map, [<<"topic">>, <<"tenant_id">>], fun(_) -> ok end);
validate_event(_) -> {error, {invalid_type, expected_object}}.

validate_endpoint(Map) when is_map(Map) ->
    with_required(Map, [<<"url">>, <<"tenant_id">>], fun(M) ->
        case maps:get(<<"url">>, M) of
            URL when is_binary(URL) ->
                case binary:match(URL, [<<"http://">>, <<"https://">>]) of
                    {0, _} -> ok;
                    _      -> {error, {invalid_field, <<"url">>, <<"must start with http(s)://">>}}
                end;
            _ -> {error, {invalid_field, <<"url">>, <<"must be string">>}}
        end
    end);
validate_endpoint(_) -> {error, {invalid_type, expected_object}}.

validate_subscription(Map) when is_map(Map) ->
    with_required(Map, [<<"endpoint_id">>, <<"topic_pattern">>, <<"tenant_id">>],
                  fun(_) -> ok end);
validate_subscription(_) -> {error, {invalid_type, expected_object}}.

with_required(Map, Keys, Fun) ->
    case check_required(Map, Keys) of
        ok -> Fun(Map);
        Err -> Err
    end.

check_required(_Map, []) -> ok;
check_required(Map, [Key | Rest]) ->
    case maps:find(Key, Map) of
        {ok, V} when V =/= <<>>, V =/= null, V =/= undefined ->
            check_required(Map, Rest);
        _ ->
            {error, {missing_field, Key}}
    end.
