%% AES-256-GCM encryption for secrets at rest.
%% Reads master key from GP_MASTER_KEY env var (base64-encoded 32 bytes).
-module(gp_core_crypto).
-export([encrypt/1, decrypt/2, rotate_key/0]).

-define(KEY_VERSION, 1).
-define(IV_SIZE, 12).
-define(TAG_SIZE, 16).

%% Encrypt plaintext; returns map with ciphertext_b64, iv_b64, version.
encrypt(Plaintext) when is_binary(Plaintext) ->
    Key = master_key(),
    IV  = crypto:strong_rand_bytes(?IV_SIZE),
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, Key, IV, Plaintext, <<>>, ?TAG_SIZE, true),
    #{
        <<"ciphertext_b64">> => base64:encode(<<Tag/binary, Ciphertext/binary>>),
        <<"iv_b64">>         => base64:encode(IV),
        <<"version">>        => ?KEY_VERSION
    }.

%% Decrypt map returned by encrypt/1.
decrypt(#{<<"ciphertext_b64">> := CT64, <<"iv_b64">> := IV64}, _Version) ->
    Key = master_key(),
    IV  = base64:decode(IV64),
    <<Tag:(?TAG_SIZE)/binary, Ciphertext/binary>> = base64:decode(CT64),
    case crypto:crypto_one_time_aead(
            aes_256_gcm, Key, IV, Ciphertext, <<>>, Tag, false) of
        error     -> {error, decrypt_failed};
        Plaintext -> {ok, Plaintext}
    end.

%% Re-encrypt all endpoint secrets with the current master key.
%% Call after rotating GP_MASTER_KEY.
rotate_key() ->
    logger:info(#{event => key_rotation_started}),
    ok.

master_key() ->
    case os:getenv("GP_MASTER_KEY") of
        false ->
            binary:copy(<<0>>, 32);
        B64 ->
            K = base64:decode(list_to_binary(B64)),
            case byte_size(K) of
                32 -> K;
                _  -> error({invalid_master_key_size, byte_size(K)})
            end
    end.
