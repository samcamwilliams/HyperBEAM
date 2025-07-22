%%% @moduledoc Implements the authentication mechanisms of the cookie codec.
%%% See the [cookie codec](dev_codec_cookie.html) documentation for more details.
-module(dev_codec_cookie_auth).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-export([commit/3, verify/3]).

%% @doc Generate a new secret, nonce, and name (secret reference). See the
%% module documentation of `dev_codec_cookie' for more details on its scheme.
commit(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    % Generate a random nonce.
    Key = crypto:strong_rand_bytes(32),
    CommittedMsg =
        hb_message:commit(
            Key,
            Opts,
            Request#{
                <<"commitment-device">> => <<"cookie@1.0">>,
                <<"type">> => <<"hmac-sha256">>,
                <<"key">> => Key
            }
        ),
    Name = hb_maps:get(<<"name">>, Request, undefined, Opts),
    % Create the cookie parameters, omitting the keys specified in the `omit'
    % parameter if any.
    CookieParams = #{ Name => hb_util:human_id(Key) },
    % Set the cookie on the base message and return the name.
    dev_codec_cookie:set_cookie(CommittedMsg, CookieParams, Opts).

%% @doc Verify the cookie in the request message against the cookie in the base
%% message. If a name key is provided in the request message, it is used instead
%% of the name key in the request's cookie. This allows a caller to take a
%% user-given message containing a cookie and verify it was created using a
%% specific name during the `generate' call. The request message is returned
%% with the cookie removed.
verify(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    {ok, Cookie} = dev_codec_cookie:from(Request, #{}, Opts),
    {ok, Name} = hb_maps:find(<<"name">>, Request, Opts),
    {ok, Secret} = hb_maps:find(Name, Cookie, Opts),
    case hb_message:verify(Secret, Request, Opts) of
        {ok, _} -> {ok, dev_codec_cookie:without(Base, Opts)};
        {error, Reason} -> {error, Reason}
    end.

%%% Tests

%% @doc Set keys in a cookie and verify that they can be parsed into a message.
set_cookie_test() ->
    Node = hb_http_server:start_node(#{}),
    {ok, SetRes} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0/set_cookie?k1=v1&k2=v2">>,
            #{}
        ),
    ?event(debug_cookie, {set_cookie_test, {set_res, SetRes}}),
    ?assertMatch(#{ <<"set-cookie">> := _ }, SetRes),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/from">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.

%% @doc Generate a secret cookie with a random name and verify it. Ensure that
%% the returned message has the cookie removed.
generate_verify_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a random name.
    {ok, Base} = hb_http:get(Node, <<"/~cookie@1.0/commit">>, #{}),
    VerifyReq =
        apply_cookie(
            #{ <<"path">> => <<"/~cookie@1.0/verify">> },
            Base,
            #{}
        ),
    % Verify the secret cookie. `verify' will return the request message with the
    % cookie removed if the secret verifies.
    {ok, Res} = hb_http:get(Node, VerifyReq, #{}),
    assert_valid_verification(Res, #{}),
    ok.

%% @doc Generate a secret cookie with a specific name and verify it. We also
%% verify that if the wrong name is provided, the verification fails.
generate_verify_with_name_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a specific name and the base verification
    % request.
    {ok, Base} = hb_http:get(Node, <<"/~cookie@1.0/commit?name=correct">>, #{}),
    VerifyReq =
        apply_cookie(
            #{ <<"path">> => <<"/~cookie@1.0/verify">> },
            Base,
            #{}
        ),
    % Verify that providing no name (letting the cookie name be used) succeeds.
    {ok, Res} = hb_http:get(Node, VerifyReq, #{}),
    assert_valid_verification(Res, #{}),
    % Verify that providing the correct name succeeds.
    {ok, Res2} = hb_http:get(Node, VerifyReq#{ <<"name">> => <<"correct">> }, #{}),
    assert_valid_verification(Res2, #{}),
    % Verify that the wrong name fails.
    {Status, _} = hb_http:get(Node, VerifyReq#{ <<"name">> => <<"wrong">> }, #{}),
    ?assertNotEqual(ok, Status),
    ok.

%% @doc Generate a secret cookie with a specified name, which must be ommitted
%% from the cookie granted to the caller. Another device may use a cookie 
%% generated in this way to be able to verify the caller's identity, without the
%% caller being able to know their own name.
generate_verify_hidden_name_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a specific name, which should not be included
    % in the cookie granted to the caller.
    {ok, Base} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0/commit?name=invisible&omit=name">>,
            #{}
        ),
    VerifyReq =
        apply_cookie(
            #{ <<"path">> => <<"/~cookie@1.0/verify">> },
            Base,
            #{}
        ),
    {ok, ParsedReq} = dev_codec_cookie:from(VerifyReq, #{}, #{}),
    % Ensure that the given cookie contains a secret, but no name.
    ?assertMatch(#{ <<"secret">> := _ }, ParsedReq),
    ?assertNotMatch(#{ <<"name">> := _ }, ParsedReq),
    % Verify that the cookie cannot be used without the name being added.
    {Status, _} = hb_http:get(Node, VerifyReq, #{}),
    ?assertNotEqual(ok, Status),
    % Verify that providing the correct name succeeds.
    {ok, Res2} = hb_http:get(Node, VerifyReq#{ <<"name">> => <<"invisible">> }, #{}),
    assert_valid_verification(Res2, #{}),
    ok.

%%% Test Helpers

%% @doc Takes the cookies from the `GenerateResponse' and applies them to the
%% `Target' message.
apply_cookie(NextReq, GenerateResponse, Opts) ->
    ?event({apply_cookie, {next_req, NextReq}, {generate, GenerateResponse}}),
    ?event({opts, Opts}),
    {ok, Cookie} = hb_maps:find(<<"set-cookie">>, GenerateResponse, Opts),
    {ok, CookiesEncoded} = dev_codec_cookie:to(Cookie, NextReq#{ <<"bundle">> => true },Opts),
    ?event(debug_cookie, {cookies_encoded, CookiesEncoded}),
    hb_ao:set(
        NextReq,
        #{ <<"cookie">> => CookiesEncoded, <<"set-cookie">> => unset },
        dev_codec_cookie:opts(Opts)
    ).

%% @doc Assert that the response is a valid verification.
assert_valid_verification(Res, Opts) ->
    ?assertNotEqual(<<"false">>, Res),
    ?assertNot(lists:member(<<"set-cookie">>, hb_maps:keys(Res, Opts))),
    ?assertNot(lists:member(<<"cookie">>, hb_maps:keys(Res, Opts))),
    ok.