%%% @doc A hook that signs inbound HTTP requests from users if they are not 
%%% already signed.
%%% It can be deployed on a node by adding the an `on/request' key to a node's
%%% configuration message.
-module(dev_codec_cookie_hook).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Keys that the preprocessor hook should not sign by default.
-define(DEFAULT_IGNORE_KEYS,
    [
        <<"cookie">>,
        <<"set-cookie">>,
        <<"path">>,
        <<"method">>
    ]
).

%%% @doc The default key to use to indicate that any given message in a request
%%% should be signed.
-define(DEFAULT_SIGN_KEY, <<"!">>).

%% @doc A preprocessor hook key that takes a message and attempts to sign it,
%% if a `cookie' field is present and there are no existing signatures.
request(Base, HookReq, Opts) ->
    ?event({cookie_on_request_called, {base, Base}, {hook_req, HookReq}}),
    % Retrieve the existing parsed messages from the request, which we return
    % unmodified if the preprocessor fails.
    ExistingMessages = hb_maps:get(<<"body">>, HookReq, Opts),
    maybe
        ?event({cookie_on_request_extracting_request, HookReq}),
        {ok, Request} ?= hb_maps:find(<<"request">>, HookReq, Opts),
        % We are only interested in messages that have no existing signatures.
        % Go to the `[]' else branch if there are existing signatures, which
        % returns the existing messages unmodified.
        [] ?= hb_message:signers(Request, Opts),
        ?event(request_has_no_signers),
        % Normalize the cookie on the request.
        {ok, WithCookie, NewOpts} ?= normalize_cookie(Base, Request, Opts),
        ?event(normalized_cookie),
        % Sign the outer request message.
        {ok, SignedWithCookie} ?= sign_message(WithCookie, NewOpts),
        ?event(signed_cookie),
        % Sign messages in the parsed stream individually if they have the
        % node's `cookie-sign' key.
        {ok, MessageSequence} ?=
            sign_individual_messages(SignedWithCookie, NewOpts),
        ?event(signed_individual_messages),
        % Add a `set' message to the end of the message sequence, with the
        % set-cookie message.
        {ok, MessageSequenceWithSetCookie} ?=
            add_set_cookie(
                SignedWithCookie,
                MessageSequence,
                NewOpts
            ),
        ?event({returning_message_sequence, MessageSequenceWithSetCookie}),
        {ok, #{ <<"body">> => MessageSequenceWithSetCookie }}
    else
        ExistingSigners when is_list(ExistingSigners) ->
            ?event({hook_skipping, {signers, ExistingSigners}, {base, Base}}),
            {ok, #{ <<"body">> => ExistingMessages }};
        error ->
            ?event({hook_error, invoked_without_request, {base, Base}}),
            {ok, #{ <<"body">> => ExistingMessages }};
        {error, Err} ->
            ?event({hook_error, {error, Err}, {base, Base}}),
            {ok, #{ <<"body">> => ExistingMessages }}
    end.

%% @doc Ensure that there is a viable set of cookies on the request. Returns 
%% both the normalized message and the new node message. If there is no existing
%% cookie, we resolve the `generate' path to generate a new wallet and set the
%% cookie.
normalize_cookie(_Base, Request, Opts) ->
    % If there is no existing cookie, we resolve the `generate' path to
    % generate a new wallet and set the cookie.
    ?event({normalizing_cookie, Request}),
    Cookies =
        case dev_codec_cookie:extract(Request, #{}, Opts) of
            {ok, EmptyCookie} when map_size(EmptyCookie) == 0 ->
                ?event({generating_cookie, Request}),
                GenerateRes = dev_secret:generate(#{}, Request, Opts),
                ?event({generated_cookie, GenerateRes}),
                {ok, Generated} = GenerateRes,
                ?event({generated_cookie_message, Generated}),
                {ok, NewCookies} =
                    dev_codec_cookie:extract(Generated, Request, Opts),
                ?event({extracted_cookie, NewCookies}),
                NewCookies;
            {ok, ExistingCookies} ->
                ?event({existing_cookie, ExistingCookies}),
                ExistingCookies
        end,
    % Get the new node options, such that we have a fresh copy with the
    % new wallet installed, if it was generated.
    ?event({getting_new_opts, Opts}),
    NewOpts = hb_http_server:get_opts(Opts),
    ?event({storing_cookie, Cookies}),
    {ok, WithCookie} = dev_codec_cookie:store(Request, Cookies, NewOpts),
    ?event({stored_cookie, WithCookie}),
    {ok, WithCookie, NewOpts}.

%% @doc Internal function to sign a message with a cookie.
sign_message(Msg, Opts) ->
    % We attempt to sign the message, now that we know we have a message 
    % with the cookie.
    maybe
        IgnoredKeys =
            hb_opts:get(
                wallet_hook_ignore,
                ?DEFAULT_IGNORE_KEYS,
                Opts
            ),
        WithoutIgnoredKeys = hb_maps:without(IgnoredKeys, Msg, Opts),
        % We attempt to sign the message, now that we know we have a message 
        % with the cookie.
        ?event(
            {preprocessor_commit,
                {to_sign, WithoutIgnoredKeys}
            }
        ),
        {ok, Signed} ?=
            dev_secret:commit(
                WithoutIgnoredKeys,
                WithoutIgnoredKeys,
                Opts
            ),
        % Add any ignored keys back to the signed message.
        SignedWithIgnored =
            hb_maps:merge(
                Signed,
                hb_maps:with(IgnoredKeys, Msg, Opts),
                Opts
            ),
        ?event(
            {committed,
                {committer, hb_message:signers(SignedWithIgnored, Opts)},
                {signed, SignedWithIgnored}
            }
        ),
        {ok, SignedWithIgnored}
    else
        {error, Err} ->
            ?event({sign_message_error, {error, Err}, {msg, Msg}}),
            {error, Err}
    end.

%% @doc Parse a request into a list of messages, sign the messages that request
%% individual signatures, and return the sequence of messages to be executed.
sign_individual_messages(SignedWithCookie, Opts) ->
    % Parse by `hb_singleton` into a list of messages to execute. Extract 
    % the cookie from the request.
    Parsed = hb_singleton:from(SignedWithCookie, Opts),
    ?event(debug_bang, {parsed, {parsed, Parsed}, {with_cookie, SignedWithCookie}}),
    % Process the parsed messages for elements that request individual 
    % signatures.
    IndividuallySigned = apply_cookie_sign(Parsed, SignedWithCookie, Opts),
    {ok, IndividuallySigned}.

%% @doc Sign all messages in the list that have a `cookie-sign: true' key.
apply_cookie_sign(Msg, WithCookie, Opts) ->
    apply_cookie_sign(
        hb_opts:get(
            cookie_hook_commit_key,
            ?DEFAULT_SIGN_KEY,
            Opts
        ),
        Msg,
        WithCookie,
        Opts
    ).
apply_cookie_sign(_Key, [], _, _Opts) -> [];
apply_cookie_sign(Key, [Msg | Rest], WithCookie, Opts) when is_map(Msg) ->
    case hb_util:atom(hb_maps:get(Key, Msg, false, Opts)) of
        true ->
            Uncommitted = hb_message:uncommitted(Msg, Opts),
            ?event(debug_bang,
                {signing_individual_message,
                    {uncommitted, Uncommitted},
                    {with_cookie, WithCookie}
                }
            ),
            maybe
                {ok, Signed} ?= sign_message(Uncommitted, Opts),
                [
                    Signed
                |
                    apply_cookie_sign(Key, Rest, WithCookie, Opts)
                ]
            else
                {error, Err} ->
                    ?event(
                        {
                            error_signing_individual_message,
                            {error, Err},
                            {detected_key, Key},
                            {msg, Msg}
                        }
                    ),
                    [{error, Err}]
            end;
        _ ->
            [ Msg | apply_cookie_sign(Key, Rest, WithCookie, Opts) ]
    end;
apply_cookie_sign(Key, [Msg | Rest], WithCookie, Opts) ->
    [Msg | apply_cookie_sign(Key, Rest, WithCookie, Opts)].

%% @doc Add a `set-cookie' message to the response.
add_set_cookie(SignedMsg, MessageSequence, Opts) ->
    {ok, #{ <<"set-cookie">> := SetCookie }} =
        dev_codec_cookie:to(
            SignedMsg,
            #{ <<"format">> => <<"set-cookie">> },
            Opts
        ),
    {
        ok,
        MessageSequence
            ++ [#{ <<"path">> => <<"set">>, <<"set-cookie">> => SetCookie }]
    }.

%%% Tests

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