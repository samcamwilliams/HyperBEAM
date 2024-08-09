%%% @doc Utilities for manipulating wallets.
-module(ar_wallet).

-export([sign/2, verify/3, to_address/2, new/0, new/1]).

-include("include/ar.hrl").
-include_lib("public_key/include/public_key.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

new() ->
	new({rsa, 65537}).
new(KeyType = {KeyAlg, PublicExpnt}) when KeyType =:= {rsa, 65537} ->
    {[_, Pub], [_, Pub, Priv|_]} = {[_, Pub], [_, Pub, Priv|_]}
		= crypto:generate_key(KeyAlg, {4096, PublicExpnt}),
    {{KeyType, Priv, Pub}, {KeyType, Pub}}.

%% @doc Sign some data with a private key.
sign({{rsa, PublicExpnt}, Priv, Pub}, Data)
    when PublicExpnt =:= 65537 ->
    rsa_pss:sign(
        Data,
        sha256,
        #'RSAPrivateKey'{
            publicExponent = PublicExpnt,
            modulus = binary:decode_unsigned(Pub),
            privateExponent = binary:decode_unsigned(Priv)
        }
    ).

%% @doc Verify that a signature is correct.
verify({{rsa, PublicExpnt}, Pub}, Data, Sig)
    when PublicExpnt =:= 65537 ->
    rsa_pss:verify(
        Data,
        sha256,
        Sig,
        #'RSAPublicKey'{
            publicExponent = PublicExpnt,
            modulus = binary:decode_unsigned(Pub)
        }
    ).

%% @doc Generate an address from a public key.
to_address(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    %% Small keys are not secure, nobody is using them, the clause
    %% is for backwards-compatibility.
    PubKey;
to_address(PubKey, {rsa, 65537}) ->
    to_rsa_address(PubKey).

%%%===================================================================
%%% Private functions.
%%%===================================================================

to_rsa_address(PubKey) ->
    hash_address(PubKey).

hash_address(PubKey) ->
    crypto:hash(sha256, PubKey).