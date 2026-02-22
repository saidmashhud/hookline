-module(gp_core_signature).
-export([sign/2, verify/3, header_value/3, signing_input/2]).

%% HMAC-SHA256 webhook signature
%% Spec: sign over "{timestamp_ms}.{body}", format "v1=<hex>"

%% Build the canonical signing input: "{timestamp_ms}.{body}"
signing_input(TimestampMs, Body) when is_integer(TimestampMs), is_binary(Body) ->
    Ts = integer_to_binary(TimestampMs),
    <<Ts/binary, ".", Body/binary>>.

%% Low-level HMAC-SHA256
sign(Secret, Input) when is_binary(Secret), is_binary(Input) ->
    crypto:mac(hmac, sha256, Secret, Input).

%% Constant-time comparison
verify(Secret, Input, Sig) when is_binary(Secret), is_binary(Input), is_binary(Sig) ->
    Expected = sign(Secret, Input),
    crypto:hash_equals(Expected, Sig).

%% Returns the X-GP-Signature header value: "v1=<hex>"
%% Call with the full signing_input/2 result.
header_value(Secret, TimestampMs, Body) when is_binary(Secret) ->
    Input = signing_input(TimestampMs, Body),
    Sig   = sign(Secret, Input),
    Hex   = binary:encode_hex(Sig, lowercase),
    <<"v1=", Hex/binary>>.
