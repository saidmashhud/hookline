-module(gp_core_schema_test).
-include_lib("eunit/include/eunit.hrl").

valid_event_test() ->
    Map = #{<<"topic">> => <<"orders.created">>, <<"tenant_id">> => <<"t1">>},
    ?assertEqual(ok, gp_core_schema:validate_event(Map)).

missing_topic_test() ->
    Map = #{<<"tenant_id">> => <<"t1">>},
    ?assertEqual({error, {missing_field, <<"topic">>}},
                 gp_core_schema:validate_event(Map)).

valid_endpoint_test() ->
    Map = #{<<"url">> => <<"https://example.com/hook">>,
            <<"tenant_id">> => <<"t1">>},
    ?assertEqual(ok, gp_core_schema:validate_endpoint(Map)).

invalid_url_test() ->
    Map = #{<<"url">> => <<"ftp://example.com/hook">>,
            <<"tenant_id">> => <<"t1">>},
    ?assertMatch({error, {invalid_field, <<"url">>, _}},
                 gp_core_schema:validate_endpoint(Map)).

valid_subscription_test() ->
    Map = #{<<"endpoint_id">>  => <<"ep1">>,
            <<"topic_pattern">> => <<"orders.*">>,
            <<"tenant_id">>    => <<"t1">>},
    ?assertEqual(ok, gp_core_schema:validate_subscription(Map)).
