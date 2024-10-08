%%% @doc Utilities for manipulating wallets.
-module(ar_wallet).
-export([sign/2, verify/3, to_address/1, to_address/2, new/0, new/1]).
-export([new_keyfile/2, load_keyfile/1, load_key/1]).

-include("include/ar.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(WALLET_DIR, ".").

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
to_address(Pubkey) ->
	to_address(Pubkey, ?DEFAULT_KEY_TYPE).
to_address(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    %% Small keys are not secure, nobody is using them, the clause
    %% is for backwards-compatibility.
    PubKey;
to_address({{_, _, PubKey}, {_, PubKey}}, {rsa, 65537}) ->
    to_address(PubKey);
to_address(PubKey, {rsa, 65537}) ->
    to_rsa_address(PubKey).

%% @doc Generate a new wallet public and private key, with a corresponding keyfile.
%% The provided key is used as part of the file name.
new_keyfile(KeyType, WalletName) when is_list(WalletName) ->
    new_keyfile(KeyType, list_to_binary(WalletName));
new_keyfile(KeyType, WalletName) ->
	{Pub, Priv, Key} =
		case KeyType of
			{?RSA_SIGN_ALG, PublicExpnt} ->
				{[Expnt, Pb], [Expnt, Pb, Prv, P1, P2, E1, E2, C]} =
					crypto:generate_key(rsa, {?RSA_PRIV_KEY_SZ, PublicExpnt}),
				Ky =
					jiffy:encode(
						{
							[
								{kty, <<"RSA">>},
								{ext, true},
								{e, ar_util:encode(Expnt)},
								{n, ar_util:encode(Pb)},
								{d, ar_util:encode(Prv)},
								{p, ar_util:encode(P1)},
								{q, ar_util:encode(P2)},
								{dp, ar_util:encode(E1)},
								{dq, ar_util:encode(E2)},
								{qi, ar_util:encode(C)}
							]
						}
					),
				{Pb, Prv, Ky};
			{?ECDSA_SIGN_ALG, secp256k1} ->
				{OrigPub, Prv} = crypto:generate_key(ecdh, secp256k1),
				<<4:8, PubPoint/binary>> = OrigPub,
				PubPointMid = byte_size(PubPoint) div 2,
				<<X:PubPointMid/binary, Y:PubPointMid/binary>> = PubPoint,
				Ky =
					jiffy:encode(
						{
							[
								{kty, <<"EC">>},
								{crv, <<"secp256k1">>},
								{x, ar_util:encode(X)},
								{y, ar_util:encode(Y)},
								{d, ar_util:encode(Prv)}
							]
						}
					),
				{compress_ecdsa_pubkey(OrigPub), Prv, Ky};
			{?EDDSA_SIGN_ALG, ed25519} ->
				{{_, Prv, Pb}, _} = new(KeyType),
				Ky =
					jiffy:encode(
						{
							[
								{kty, <<"OKP">>},
								{alg, <<"EdDSA">>},
								{crv, <<"Ed25519">>},
								{x, ar_util:encode(Pb)},
								{d, ar_util:encode(Prv)}
							]
						}
					),
				{Pb, Prv, Ky}
		end,
	Filename = wallet_filepath(WalletName, Pub, KeyType),
	filelib:ensure_dir(Filename),
	file:write_file(Filename, Key),
	{{KeyType, Priv, Pub}, {KeyType, Pub}}.

wallet_filepath(Wallet) ->
	filename:join([?WALLET_DIR, binary_to_list(Wallet)]).

wallet_filepath2(Wallet) ->
	filename:join([?WALLET_DIR, binary_to_list(Wallet)]).

%% @doc Read the keyfile for the key with the given address from disk.
%% Return not_found if arweave_keyfile_[addr].json or [addr].json is not found
%% in [data_dir]/?WALLET_DIR.
load_key(Addr) ->
	Path = ar_util:encode(Addr),
	case filelib:is_file(Path) of
		false ->
			Path2 = wallet_filepath2(ar_util:encode(Addr)),
			case filelib:is_file(Path2) of
				false ->
					not_found;
				true ->
					load_keyfile(Path2)
			end;
		true ->
			load_keyfile(Path)
	end.

%% @doc Extract the public and private key from a keyfile.
load_keyfile(File) ->
	{ok, Body} = file:read_file(File),
	{Key} = jiffy:decode(Body),
	{Pub, Priv, KeyType} =
		case lists:keyfind(<<"kty">>, 1, Key) of
			{<<"kty">>, <<"EC">>} ->
				{<<"x">>, XEncoded} = lists:keyfind(<<"x">>, 1, Key),
				{<<"y">>, YEncoded} = lists:keyfind(<<"y">>, 1, Key),
				{<<"d">>, PrivEncoded} = lists:keyfind(<<"d">>, 1, Key),
				OrigPub = iolist_to_binary([<<4:8>>, ar_util:decode(XEncoded),
						ar_util:decode(YEncoded)]),
				Pb = compress_ecdsa_pubkey(OrigPub),
				Prv = ar_util:decode(PrivEncoded),
				KyType = {?ECDSA_SIGN_ALG, secp256k1},
				{Pb, Prv, KyType};
			{<<"kty">>, <<"OKP">>} ->
				{<<"x">>, PubEncoded} = lists:keyfind(<<"x">>, 1, Key),
				{<<"d">>, PrivEncoded} = lists:keyfind(<<"d">>, 1, Key),
				Pb = ar_util:decode(PubEncoded),
				Prv = ar_util:decode(PrivEncoded),
				KyType = {?EDDSA_SIGN_ALG, ed25519},
				{Pb, Prv, KyType};
			_ ->
				{<<"n">>, PubEncoded} = lists:keyfind(<<"n">>, 1, Key),
				{<<"d">>, PrivEncoded} = lists:keyfind(<<"d">>, 1, Key),
				Pb = ar_util:decode(PubEncoded),
				Prv = ar_util:decode(PrivEncoded),
				KyType = {?RSA_SIGN_ALG, 65537},
				{Pb, Prv, KyType}
		end,
	{{KeyType, Priv, Pub}, {KeyType, Pub}}.

%%%===================================================================
%%% Private functions.
%%%===================================================================

to_rsa_address(PubKey) ->
    hash_address(PubKey).

hash_address(PubKey) ->
    crypto:hash(sha256, PubKey).

%%%===================================================================
%%% Private functions.
%%%===================================================================

wallet_filepath(WalletName, PubKey, KeyType) ->
	wallet_filepath(wallet_name(WalletName, PubKey, KeyType)).

wallet_name(wallet_address, PubKey, KeyType) ->
	ar_util:encode(to_address(PubKey, KeyType));
wallet_name(WalletName, _, _) ->
	WalletName.

compress_ecdsa_pubkey(<<4:8, PubPoint/binary>>) ->
	PubPointMid = byte_size(PubPoint) div 2,
	<<X:PubPointMid/binary, Y:PubPointMid/integer-unit:8>> = PubPoint,
	PubKeyHeader =
		case Y rem 2 of
			0 -> <<2:8>>;
			1 -> <<3:8>>
		end,
	iolist_to_binary([PubKeyHeader, X]).