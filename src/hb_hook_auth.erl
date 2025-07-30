%%% @doc Common authentication hook framework for request signing.
%%% 
%%% This module provides a reusable pattern for implementing authentication hooks
%%% that verify request headers and conditionally sign messages. It abstracts the
%%% common functionality found in dev_codec_cookie_hook and can be used with
%%% different authentication schemes (cookie, HTTP auth, etc.).
-module(hb_hook_auth).
-export([process_request/3]).
-include("include/hb.hrl").

%% @doc Process an incoming request through an authentication hook.
%% This function implements the common pattern of:
%% 1. Checking if the request already has signatures
%% 2. Normalizing/extracting authentication credentials  
%% 3. Signing the request and any marked messages
%% 4. Adding authentication state to the response
process_request(Base, HookReq, Opts) ->
    ExistingMessages = hb_maps:get(<<"body">>, HookReq, Opts),
    maybe
        {ok, Request} ?= hb_maps:find(<<"request">>, HookReq, Opts),
        [] ?= hb_message:signers(Request, Opts),
        ?event(auth_hook_no_signers),
        % Get the auth provider from options
        AuthProvider = hb_opts:get(auth_provider, dev_codec_cookie_auth, Opts),
        % Normalize authentication (generate if needed)
        {ok, NormalizedReq, NewOpts} ?= 
            normalize_auth(AuthProvider, Base, Request, Opts),
        ?event(auth_hook_normalized),
        % Sign the request  
        {ok, SignedReq} ?= sign_request(AuthProvider, NormalizedReq, NewOpts),
        ?event(auth_hook_signed),
        % Process individual messages if needed
        {ok, MessageSequence} ?=
            process_messages(AuthProvider, SignedReq, NewOpts),
        ?event(auth_hook_processed_messages),
        % Add authentication state to response
        {ok, FinalSequence} ?=
            finalize_response(AuthProvider, SignedReq, MessageSequence, NewOpts),
        ?event({auth_hook_returning, FinalSequence}),
        {ok, #{ <<"body">> => FinalSequence }}
    else
        ExistingSigners when is_list(ExistingSigners) ->
            ?event({auth_hook_skipping, {signers, ExistingSigners}}),
            {ok, #{ <<"body">> => ExistingMessages }};
        error ->
            ?event({auth_hook_error, no_request}),
            {ok, #{ <<"body">> => ExistingMessages }};
        {error, Err} ->
            ?event({auth_hook_error, Err}),
            {ok, #{ <<"body">> => ExistingMessages }}
    end.

%% @doc Normalize authentication credentials, generating new ones if needed
normalize_auth(AuthProvider, _Base, Request, Opts) ->
    case hb_ao:resolve(AuthProvider, Request#{ <<"path">> => <<"normalize">> }, Opts) of
        {error, not_found} ->
            {ok, Request, Opts};
        {ok, Normalized} ->
            % Find the new options again, in case the auth provider modified them
            {ok, Normalized, hb_http_server:get_opts(Opts)}
    end.

%% @doc Sign a request using the configured auth provider
sign_request(AuthProvider, Msg, Opts) when AuthProvider == dev_wallet ->
    % Wallet signs without ignored keys
    IgnoredKeys = hb_opts:get(wallet_hook_ignore, default_ignored_keys(), Opts),
    WithoutIgnored = hb_maps:without(IgnoredKeys, Msg, Opts),
    case dev_wallet:commit(WithoutIgnored, WithoutIgnored, Opts) of
        {ok, Signed} ->
            SignedWithIgnored = 
                hb_maps:merge(
                    Signed,
                    hb_maps:with(IgnoredKeys, Msg, Opts),
                    Opts
                ),
            {ok, SignedWithIgnored};
        {error, Err} ->
            {error, Err}
    end;
sign_request(AuthProvider, Msg, Opts) ->
    % Other providers use their standard commit interface
    AuthProvider:commit(Msg, Msg, Opts).

%% @doc Process a sequence of messages, signing those marked for signing
process_messages(AuthProvider, SignedReq, Opts) ->
    Parsed = hb_singleton:from(SignedReq, Opts),
    SignKey = hb_opts:get(auth_hook_commit_key, <<"!">>, Opts),
    Processed = process_messages(AuthProvider, SignKey, Parsed, Opts),
    {ok, Processed}.
process_messages(_AuthProvider, _Key, [], _Opts) -> [];
process_messages(AuthProvider, Key, [Msg | Rest], Opts) when is_map(Msg) ->
    case hb_util:atom(hb_maps:get(Key, Msg, false, Opts)) of
        true ->
            Uncommitted = hb_message:uncommitted(Msg, Opts),
            case sign_request(AuthProvider, Uncommitted, Opts) of
                {ok, Signed} ->
                    [
                        Signed
                    |
                        process_messages(AuthProvider, Key, Rest, Opts)
                    ];
                {error, Err} ->
                    ?event({auth_hook_sign_error, Err}),
                    [{error, Err}]
            end;
        _ ->
            [Msg | process_messages(AuthProvider, Key, Rest, Opts)]
    end;
process_messages(AuthProvider, Key, [Msg | Rest], Opts) ->
    [Msg | process_messages(AuthProvider, Key, Rest, Opts)].

%% @doc Finalize the response by adding authentication state
finalize_response(dev_codec_cookie_auth, SignedMsg, MessageSequence, Opts) ->
    % Cookie auth adds set-cookie to response
    {ok, #{ <<"set-cookie">> := SetCookie }} =
        dev_codec_cookie:to(
            SignedMsg,
            #{ <<"format">> => <<"set-cookie">> },
            Opts
        ),
    {
        ok,
        MessageSequence ++
            [#{ <<"path">> => <<"set">>, <<"set-cookie">> => SetCookie }]
    };
finalize_response(_AuthProvider, _SignedMsg, MessageSequence, _Opts) ->
    % Other providers don't need response modification
    {ok, MessageSequence}.

%% @doc Default keys to ignore when signing
default_ignored_keys() ->
    [<<"cookie">>, <<"set-cookie">>, <<"path">>, <<"method">>].