%%% @doc Implements a two-step authentication process for HTTP requests, using
%%% the `Basic' authentication scheme, implementing the `commit' and `verify'
%%% scheme of `~message@1.0'.
%%%
%%% Its implementation proxies to the `~httpsig@1.0' secret key HMAC commitment
%%% scheme. Additionally, it implements a `generator' function that can be used
%%% to call PBKDF2 with the user's authentication information.
-module(dev_codec_http_auth).
-export([commit/3, verify/3]).
-export([generate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The default salt to use for the PBKDF2 algorithm. This value must be
%% global across all nodes that intend to have a shared keyspace, although in
%% instances where this is not possible, users may specify non-standard salts
%% to the `/generate' path with the `salt' request key.
-define(DEFAULT_SALT, <<"constant:ao">>).

%% @doc Generate or extract a new secret and commit to the message with the
%% `~httpsig@1.0/commit?type=hmac-sha256&scheme=secret' commitment mechanism.
commit(Base, Req, Opts) ->
    case generate(Base, Req, Opts) of
        {ok, Key} ->
            {ok, CommitRes} =
                dev_codec_httpsig_proxy:commit(
                    <<"http-auth@1.0">>,
                    Key,
                    Base,
                    Req,
                    Opts
                ),
            ?event({commit_result, CommitRes}),
            {ok, CommitRes};
        {error, Err} ->
            {error, Err}
    end.

%% @doc Verify a given `Base' message with a given `Key' using the `~httpsig@1.0'
%% HMAC commitment scheme.
verify(Base, RawReq, Opts) ->
    ?event({verify_invoked, {base, Base}, {req, RawReq}}),
    {ok, Key} = generate(Base, RawReq, Opts),
    ?event({verify_found_key, {key, Key}, {base, Base}, {req, RawReq}}),
    {ok, VerifyRes} =
        dev_codec_httpsig_proxy:verify(
            Key,
            Base,
            RawReq,
            Opts
        ),
    ?event({verify_result, VerifyRes}),
    {ok, VerifyRes}.

%% @doc Collect authentication information from the client. If the `raw' flag
%% is set to `true', return the raw authentication information. Otherwise,
%% derive a key from the authentication information and return it.
generate(_Msg, Req, Opts) ->
    case hb_maps:get(<<"authorization">>, Req, undefined, Opts) of
        <<"Basic ", Auth/binary>> ->
            Decoded = base64:decode(Auth),
            case hb_maps:get(<<"raw">>, Req, false, Opts) of
                true -> {ok, Decoded};
                false -> derive_key(Decoded, Req, Opts)
            end;
        undefined ->
            {error,
                #{
                    <<"status">> => 401,
                    <<"www-authenticate">> => <<"Basic">>,
                    <<"details">> => <<"No authorization header provided.">>
                }
            };
        Unrecognized ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"details">> =>
                        <<"Unrecognized authorization header: ", Unrecognized/binary>>
                }
            }
    end.

%% @doc Derive a key from the authentication information using the PBKDF2
%% algorithm and user specified parameters.
derive_key(Decoded, Req, Opts) ->
    Alg = hb_util:atom(hb_maps:get(<<"alg">>, Req, <<"sha256">>, Opts)),
    Salt = hb_maps:get(<<"salt">>, Req, ?DEFAULT_SALT, Opts),
    Iterations = hb_maps:get(<<"iterations">>, Req, 600_000, Opts),
    KeyLength = hb_maps:get(<<"key-length">>, Req, 32, Opts),
    ?event(
        {derive_key,
            {alg, Alg},
            {salt, Salt},
            {iterations, Iterations},
            {key_length, KeyLength}
        }
    ),
    case hb_crypto:pbkdf2(Alg, Decoded, Salt, Iterations, KeyLength) of
        {ok, Key} -> {ok, hb_util:encode(Key)};
        {error, Err} ->
            ?event(
                {pbkdf2_error,
                    {alg, Alg},
                    {salt, Salt},
                    {iterations, Iterations},
                    {key_length, KeyLength},
                    {error, Err}
                }
            ),
            {error,
                #{
                    <<"status">> => 500,
                    <<"details">> => <<"Failed to derive key.">>
                }
            }
    end.