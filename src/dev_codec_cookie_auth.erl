%%% @moduledoc Implements the authentication mechanisms of the cookie codec.
%%% See the [cookie codec](dev_codec_cookie.html) documentation for more details.
-module(dev_codec_cookie_auth).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-export([commit/3, verify/3]).

%% @doc Generate a new secret (if no `committer' specified), and use it as the
%% key for the `httpsig@1.0' commitment. If a `committer' is given, we search 
%% for it in the cookie message instead of generating a new secret. See the
%% module documentation of `dev_codec_cookie' for more details on its scheme.
commit(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    % Calculate the key to use for the commitment.
    Key =
        case find_secret(Request, Opts) of
            {error, no_committer} -> crypto:strong_rand_bytes(32);
            {error, not_found} ->
                throw({error, <<"Necessary cookie not found in request.">>});
            {ok, Secret} -> Secret
        end,
    % Generate the commitment, find it, change the `commitment-device' to
    % `cookie@1.0', and add it back to the message.
    ExistingComms = hb_maps:get(<<"commitments">>, Base, #{}, Opts),
    CommittedMsg =
        hb_message:commit(
            hb_message:uncommitted(Base, Opts),
            Opts,
            Request#{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"hmac-sha256">>,
                <<"scheme">> => <<"secret">>,
                <<"key">> => Key
            }
        ),
    {ok, CommitmentID, Commitment} =
        hb_message:commitment(
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"hmac-sha256">>
            },
            CommittedMsg,
            Opts
        ),
    ModCommittedMsg =
        CommittedMsg#{
            <<"commitments">> =>
                ExistingComms#{
                    CommitmentID =>
                        Commitment#{
                            <<"commitment-device">> => <<"cookie@1.0">>
                        }
                }
        },
    ?event({cookie_commitment, {id, CommitmentID}, {commitment, ModCommittedMsg}}),
    CookieAddr = dev_codec_httpsig_keyid:secret_key_to_committer(Key),
    % Create the cookie parameters, using the name as the key and the secret as
    % the value.
    CookieParams = #{ CookieAddr => hb_util:encode(Key) },
    % Set the cookie on the message with the new commitment and return.
    dev_codec_cookie:set_cookie(ModCommittedMsg, CookieParams, Opts).

%% @doc Verify the hmac commitment with the key being the secret from the 
%% request cookies. We find the appropriate cookie from the cookie message by
%% the committer ID given in the request message.
verify(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    ?event({verify, {base, Base}, {request, Request}}),
    {ok, Secret} = find_secret(Request, Opts),
    % Reformat the request to include the secret as a key.
    ProxyRequest =
        Request#{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"path">> => <<"verify">>,
            <<"key">> => Secret
        },
    ?event({proxy_request, ProxyRequest}),
    {ok, hb_message:verify(Base, ProxyRequest, Opts)}.

%% @doc Find the secret key for the given committer, if it exists in the cookie.
find_secret(Request, Opts) ->
    maybe
        {ok, Committer} ?= hb_maps:find(<<"committer">>, Request, Opts),
        find_secret(Committer, Request, Opts)
    else error -> {error, no_committer}
    end.
find_secret(Committer, Request, Opts) ->
    maybe
        {ok, Cookie} ?= dev_codec_cookie:from(Request, #{}, Opts),
        {ok, _Secret} ?= hb_maps:find(Committer, Cookie, Opts)
    else error -> {error, not_found}
    end.

%%% Tests

%% @doc Set keys in a cookie and verify that they can be parsed into a message.
set_cookie_test() ->
    Node = hb_http_server:start_node(#{}),
    {ok, SetRes} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0/set-cookie?k1=v1&k2=v2">>,
            #{}
        ),
    ?event(debug_cookie, {set_cookie_test, {set_res, SetRes}}),
    ?assertMatch(#{ <<"set-cookie">> := _ }, SetRes),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/from">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.

%% @doc Call the cookie codec's `commit' and `verify' functions directly.
directly_invoke_commit_verify_test() ->
    Base = #{ <<"test-key">> => <<"test-value">> },
    CommittedMsg =
        hb_message:commit(
            Base,
            #{},
            #{
                <<"commitment-device">> => <<"cookie@1.0">>
            }
        ),
    ?event({committed_msg, CommittedMsg}),
    ?assertEqual(1, length(hb_message:signers(CommittedMsg, #{}))),
    VerifyReq =
        apply_cookie(
            CommittedMsg#{
                <<"committers">> => hb_message:signers(CommittedMsg, #{})
            },
            CommittedMsg,
            #{}
        ),
    VerifyReqWithoutComms = hb_maps:without([<<"commitments">>], VerifyReq, #{}),
    ?event({verify_req_without_comms, VerifyReqWithoutComms}),
    ?assert(hb_message:verify(CommittedMsg, VerifyReqWithoutComms, #{})),
    ok.

%% @doc Generate a secret cookie with a random name and verify it. Ensure that
%% the returned message has the cookie removed.
http_generate_verify_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a random name.
    {ok, Base} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~cookie@1.0/commit">>,
                <<"test-key">> => <<"value">>,
                <<"committed">> => [<<"test-key">>]
            },
            #{}
        ),
    ?event({committed_response, Base}),
    VerifyReq =
        apply_cookie(
            Base#{
                <<"path">> => <<"verify">>
            },
            Base,
            #{}
        ),
    ?event({verify_req, VerifyReq}),
    % Verify the secret cookie. `verify' will return the request message with the
    % cookie removed if the secret verifies.
    {ok, Res} = hb_http:get(Node, VerifyReq, #{}),
    assert_valid_verification(Res, #{}),
    ok.

%%% Test Helpers

%% @doc Takes the cookies from the `GenerateResponse' and applies them to the
%% `Target' message.
apply_cookie(NextReq, GenerateResponse, Opts) ->
    {ok, Cookie} = hb_maps:find(<<"set-cookie">>, GenerateResponse, Opts),
    NextReq#{ <<"cookie">> => Cookie }.

%% @doc Assert that the response is a valid verification.
assert_valid_verification(Res, Opts) ->
    ?assertNotEqual(<<"false">>, Res),
    ?assertNot(lists:member(<<"set-cookie">>, hb_maps:keys(Res, Opts))),
    ?assertNot(lists:member(<<"cookie">>, hb_maps:keys(Res, Opts))),
    ok.