%%% @doc A hook that signs inbound HTTP requests from users if they are not 
%%% already signed.
%%% It can be deployed on a node by adding the an `on/request' key to a node's
%%% configuration message.
-module(dev_codec_cookie_hook).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Keys that the preprocessor hook should not sign by default.
-define(DEFAULT_IGNORE_KEYS, [<<"cookie">>, <<"set-cookie">>]).

%% @doc A preprocessor hook key that takes a message and attempts to sign it,
%% if a `cookie' field is present and there are no existing signatures.
request(Base, HookReq, Opts) ->
    ?event({cookie_on_request_called, {base, Base}, {hook_req, HookReq}}),
    % Retrieve the existing parsed messages from the request, which we return
    % unmodified if the preprocessor fails.
    ExistingMessages = hb_maps:get(<<"body">>, HookReq, Opts),
    maybe
        {ok, Request} ?= hb_maps:find(<<"request">>, HookReq, Opts),
        % We are only interested in messages that have no existing signatures.
        [] ?= hb_message:signers(Request, Opts),
        % If there is no existing cookie, we resolve the `generate' path to
        % generate a new wallet and set the cookie.
        ?event({cookie_on_request_extracting_cookies, Request}),
        Cookies =
            case dev_codec_cookie:extract(Request, #{}, Opts) of
                {ok, EmptyCookie} when map_size(EmptyCookie) == 0 ->
                    ?event({cookie_on_request_generating_cookie, Request}),
                    {ok, GenerateRes} =
                        dev_wallet:generate(Base, Request, Opts),
                    {ok, NewCookies} =
                        dev_codec_cookie:extract(GenerateRes, Request, Opts),
                    NewCookies;
                {ok, ExistingCookies} -> ExistingCookies
            end,
        % Get the new node options, such that we have a fresh copy with the
        % new wallet installed, if it was generated.
        NewOpts = hb_http_server:get_opts(Opts),
        IgnoredKeys =
            hb_opts:get(
                wallet_preprocessor_ignore,
                ?DEFAULT_IGNORE_KEYS,
                NewOpts
            ),
        WithoutIgnoredKeys = hb_maps:without(IgnoredKeys, Request, NewOpts),
        {ok, WithCookie} = dev_codec_cookie:store(WithoutIgnoredKeys, Cookies, Opts),
        % We attempt to sign the message, now that we know we have a message 
        % with the cookie.
        ?event(
            {preprocessor_commit,
                {with_cookie, WithCookie},
                {to_sign, WithCookie}
            }
        ),
        {ok, Signed} ?=
            dev_wallet:commit(
                WithCookie,
                WithCookie,
                NewOpts
            ),
        {ok, SignedWithCookie} = dev_codec_cookie:store(Signed, Cookies, Opts),
        % Add any ignored keys back to the signed message.
        SignedWithIgnored =
            hb_maps:merge(
                SignedWithCookie,
                hb_maps:with(IgnoredKeys, Request, NewOpts),
                NewOpts
            ),
        ?event(
            {committed,
                {committor, hb_message:signers(SignedWithIgnored, NewOpts)},
                {signed, SignedWithIgnored}
            }
        ),
        % Parse by `hb_singleton` into a list of messages to execute. Insert
        % a message to set the cookie in the result as the final step.
        Parsed = hb_singleton:from(SignedWithIgnored, NewOpts),
        {ok, OnlyCookieMsg} = dev_codec_cookie:store(#{}, Cookies, Opts),
        {ok, #{ <<"set-cookie">> := SetCookie }} =
            dev_codec_cookie:to(
                OnlyCookieMsg,
                #{ <<"format">> => <<"set-cookie">> },
                Opts
            ),
        WithSetCookie =
            Parsed ++
            [#{ <<"path">> => <<"set">>, <<"set-cookie">> => SetCookie }],
        % Return the signed message, with the cookie added to the response.
        {ok, #{ <<"body">> => WithSetCookie }}
    else
        error ->
            ?event({preprocessor_error, invoked_without_request, {base, Base}}),
            {ok, #{ <<"body">> => ExistingMessages }};
        Signers when is_list(Signers) ->
            ?event({preprocessor_skipping, {signers, Signers}, {base, Base}}),
            {ok, #{ <<"body">> => ExistingMessages }};
        {error, Err} ->
            ?event({preprocessor_error, {error, Err}, {base, Base}}),
            {ok, #{ <<"body">> => ExistingMessages }}
    end.

preprocessor_cookie_signing_test() ->
    % Start a node with a preprocessor hook that signs the request.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"cookie@1.0">>,
                        <<"path">> => <<"request">>
                    }
                }
            }
        ),
    % Run a request and check that the response is signed.
    {ok, Response} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/commitments">>,
                <<"body">> => <<"Test data">>
            },
            #{}
        ),
    % Filter the response to only include signed commitments.
    Signers = signers_from_commitments_response(Response, ServerWallet),
    ?event(
        {response, {found_signers, Signers}}
    ),    
    ?assertEqual(1, length(Signers)),
    % Generate a further request and check that the same address is used.
    [CookieAddress] = Signers,
    #{ <<"priv">> := CookiePriv } = Response,
    ?event(
        {cookie_from_response,
            {cookie_priv, CookiePriv},
            {cookie_address, CookieAddress}
        }
    ),
    {ok, Response2} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/commitments">>,
                <<"body">> => <<"Test data2">>,
                <<"priv">> => CookiePriv
            },
            #{}
        ),
    ?assertEqual(
        [CookieAddress],
        signers_from_commitments_response(Response2, ServerWallet)
    ).

%% @doc The cookie hook test(s) call `GET /commitments', which returns the 
%% commitments found on the client request during execution on the server.
%% This function filters the response to return only the signers of that message,
%% excluding the server's own signature.
signers_from_commitments_response(Response, ServerWallet) ->
    ServerAddress = ar_wallet:to_address(ServerWallet),
    hb_maps:values(hb_maps:filtermap(
        fun(Key, Value) when ?IS_ID(Key) ->
            Type = hb_maps:get(<<"type">>, Value, not_found, #{}),
            Committer = hb_maps:get(<<"committer">>, Value, not_found, #{}),
            case {Type, Committer} of
                {<<"rsa-pss-sha512">>, ServerAddress} -> false;
                {<<"rsa-pss-sha512">>, _} -> {true, Committer};
                _ -> false
            end;
           (_Key, _Value) ->
            false
        end,
        Response,
        #{}
    )).