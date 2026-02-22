-module(gp_core_uuid).
-export([generate/0, generate_str/0]).

generate() ->
    uuid:get_v4().

generate_str() ->
    uuid:uuid_to_string(generate(), binary_standard).
