%%% @doc A utility device that manages setting and parsing the cookies found in
%%% requests from the caller. Additionally, a `generate' and `verify' API is
%%% provided to generate and verify a secret stored in the cookies of the caller.
%%% 
%%% The cryptographic scheme used in the generate and verify APIs is as follows:
%%% 
%%%     generate:
%%%         secret = 32 random bytes
%%%         name = req.name or base64url(sha256(secret))
%%%         commitment = sha256(secret ++ name)
%%%         return cookie(#{ secret, commitment, name })
%%% 
%%%     verify:
%%%         expected = sha256(req.cookie.secret ++ (req.name || req.cookie.name))
%%%         return req.cookie.commitment == expected
%%% 
%%% This scheme enables three different modes of operation:
%%% 
%%% 1. Generation with a random name.
%%%    Allowing: The caller to be identified by the node as the holder of the
%%%    secret for a name, but not necessarily known/remembered by the node. If
%%%    the node adds the `name' key to the verification request, they may gain
%%%    certainty that the caller is also indeed a holder of a specific secret
%%%    generated previously.
%%% 2. Generation with a specific name.
%%%    Allowing: The node to ensure that the caller has a secret that relates
%%%    only to the name provided during generation.
%%% 3. Generation with a specific name, but omitting the name from the cookie.
%%%    Allowing: The node to have a unique identifier for the caller that is
%%%    unknown to them.
%%% 
%%% Note: While names given by the caller of `generate' are combined with the
%%% secret to generate the commitment, allowing user's to be oblivious to their
%%% name, `~cookie@1.0` makes no guarantee that chosen names are unique. If no
%%% name is provided to the `generate' call, however, the name is chosen at
%%% random from a 256-bit address space.
%%% 
%%% This device supports the following paths:
%%% 
%%% `/generate': Sets a `secret', `nonce', and `name' key in the cookies of the
%%% caller. The name may be set by the caller, or will be calculated as the hash
%%% of the nonce. The `secret' is a SHA-256 hash commitment of the nonce and
%%% name. Optionally takes an `omit' parameter to omit the caller's `name' from
%%% the cookie.
%%% `/verify': Verifies the caller's request by checking the parameters in the
%%% request match the parameters in the cookies of the base message. Optionally,
%%% the `name' may be provided in the request to ensure that the caller is the
%%% holder of an associated secret.
%%% `/set-cookie': Sets the keys in the request message in the cookies of the
%%% caller.
-module(dev_cookie).
%%% Public set/parse API.
-export([set_cookie/3, parse/3]).
%%% Public verification API.
-export([generate/3, verify/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Get the options to use for functions in the cookie device. We use the
%% `priv_store' option if set, such that evaluations are not inadvertently
%% persisted in public storage. Additionally, we ensure that no cache entries
%% are generated from downstream AO-Core resolutions.
cookie_opts(Opts) ->
    Opts#{
        store =>
            case hb_opts:get(priv_store, undefined, Opts) of
                undefined -> hb_opts:get(store, undefined, Opts);
                PrivStore -> PrivStore
            end,
        cache_control => [<<"no-store">>, <<"no-cache">>]
    }.

%% @doc Set the keys in the request message in the cookies of the caller.
set_cookie(Base, Req, RawOpts) ->
    Opts = cookie_opts(RawOpts),
    {ok, CookieMsg} = parse(Base, Opts),
    NewCookieMsg = hb_ao:set(CookieMsg, Req#{ <<"path">> => unset}, Opts),
    TABM = hb_message:convert(NewCookieMsg, tabm, <<"structured@1.0">>, Opts),
    Cookie = hb_util:bin(cow_cookie:cookie(hb_maps:to_list(TABM))),
    Res = {ok, Base#{ <<"set-cookie">> => Cookie }},
    ?event({set_cookie, {base, Base}, {req, Req}, {res, Res}}, Opts),
    Res.

%% @doc Remove the cookie from the given message.
without(Base, RawOpts) ->
    Opts = cookie_opts(RawOpts),
    {ok, hb_ao:set(Base, #{ <<"cookie">> => unset }, Opts)}.

%% @doc Parse the cookies in the base message and return them to the caller.
parse(Base, Opts) -> parse(Base, #{}, Opts).
parse(Base, _Req, RawOpts) ->
    Opts = cookie_opts(RawOpts),
    case hb_ao:get(<<"cookie">>, Base, Opts) of
        not_found -> {ok, #{}};
        Cookies ->
            try cow_cookie:parse_cookie(Cookies) of
                CookiePairs ->
                    {ok, maps:from_list(CookiePairs)}
            catch
                _:Reason ->
                    {ok, #{}}
            end
    end.

%% @doc Generate a new secret, nonce, and name set in the cookies of the caller.
generate(Base, Request, RawOpts) ->
    Opts = cookie_opts(RawOpts),
    % Generate a random nonce.
    Secret = crypto:strong_rand_bytes(32),
    Name =
        case hb_ao:get(<<"name">>, Request, Opts) of
            not_found ->
                % If no name is provided, use the hash of the secret such that
                % providing the secret could be used to verify the caller and
                % re-calculate the name if necessary.
                hb_util:human_id(crypto:hash(sha256, Secret));
            N ->
                % If the name is provided use it as a salt for the secret.
                N
        end,
    Commitment = calculate_commitment(Secret, Name, Opts),
    % Create the cookie parameters, omitting the keys specified in the `omit'
    % parameter if any.
    CookieParams =
        hb_maps:without(
            case hb_ao:get(<<"omit">>, Request, Opts) of
                not_found -> [];
                KeyList when is_list(KeyList) ->
                    lists:map(fun hb_util:bin/1, KeyList);
                Key ->
                    [hb_util:bin(Key)]
            end,
            #{
                <<"name">> => Name,
                <<"secret">> => hb_util:human_id(Secret),
                <<"commitment">> => hb_util:human_id(Commitment)
            },
            Opts
        ),
    % Set the cookie on the base message and return the name.
    set_cookie(Base, CookieParams, Opts).

%% @doc Verify the cookie in the request message against the cookie in the base
%% message. If a name key is provided in the request message, it is used instead
%% of the name key in the request's cookie. This allows a caller to take a
%% user-given message containing a cookie and verify it was created using a
%% specific name during the `generate' call.
verify(Base, Request, RawOpts) ->
    Opts = cookie_opts(RawOpts),
    maybe
        % Parse the commitment from the base message.
        {ok, BaseCookieMsg} = parse(Base, Opts),
        {ok, ExpectedCommitment} ?=
            hb_maps:find(
                <<"commitment">>,
                BaseCookieMsg,
                Opts
            ),
        % Parse the secret and name from the request message, favoring the name
        % provided in the request.
        {ok, RequestCookieMsg} = parse(Request, Opts),
        {ok, EncSecret} ?= hb_maps:find(<<"secret">>, RequestCookieMsg, Opts),
        Secret = hb_util:decode(EncSecret),
        {ok, Name} ?=
            case hb_maps:find(<<"name">>, Request, Opts) of
                error -> hb_maps:find(<<"name">>, RequestCookieMsg, Opts);
                N -> N
            end,
        ExpectedCommitment ?= calculate_commitment(Secret, Name, Opts),
        without(Base, Opts)
    else
        _ ->
            % If any of the above patterns fail, return a failure.
            {error, <<"false">>}
    end.

%% @doc Calculate the commitment for a given secret and name.
calculate_commitment(Secret, Name, _Opts) ->
    hb_util:human_id(crypto:hash(sha256, <<Secret/binary, Name/binary>>)).

%%% Tests

%% @doc Set keys in a cookie and verify that they can be parsed into a message.
set_cookie_test() ->
    Node = hb_http_server:start_node(#{}),
    {ok, SetRes} = hb_http:get(Node, <<"/~cookie@1.0/set-cookie?k1=v1&k2=v2">>, #{}),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/parse">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.

%% @doc Generate a secret cookie with a random name and verify it. Ensure that
%% the returned message has the cookie removed.
generate_verify_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a random name.
    {ok, Base} = hb_http:get(Node, <<"/~cookie@1.0/generate">>, #{}),
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
    {ok, Base} = hb_http:get(Node, <<"/~cookie@1.0/generate?name=correct">>, #{}),
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
            <<"/~cookie@1.0/generate?name=invisible&omit=name">>,
            #{}
        ),
    VerifyReq =
        apply_cookie(
            #{ <<"path">> => <<"/~cookie@1.0/verify">> },
            Base,
            #{}
        ),
    {ok, ParsedReq} = parse(VerifyReq, #{}),
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
apply_cookie(Target, GenerateResponse, Opts) ->
    {ok, Cookie} = hb_maps:find(<<"set-cookie">>, GenerateResponse, Opts),
    hb_ao:set(
        Target,
        #{ <<"cookie">> => Cookie, <<"set-cookie">> => unset },
        cookie_opts(Opts)
    ).

%% @doc Assert that the response is a valid verification.
assert_valid_verification(Res, Opts) ->
    ?assertNotEqual(<<"false">>, Res),
    ?assertNot(lists:member(<<"set-cookie">>, hb_maps:keys(Res, Opts))),
    ?assertNot(lists:member(<<"cookie">>, hb_maps:keys(Res, Opts))),
    ok.