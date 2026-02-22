-module(gp_store_protocol).

-export([encode/2, decode/1]).

%% Encode a command map to length-prefixed JSON bytes
encode(Cmd, Args) when is_binary(Cmd), is_map(Args) ->
    Msg = maps:merge(#{<<"cmd">> => Cmd}, Args),
    JSON = jsx:encode(Msg),
    Len = byte_size(JSON),
    <<Len:32/big, JSON/binary>>.

%% Decode a response JSON binary
decode(Bin) ->
    try jsx:decode(Bin, [return_maps])
    catch _:_ -> {error, invalid_json}
    end.
