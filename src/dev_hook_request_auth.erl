%%% @doc Common authentication hook framework for on-request signing.
%%% 
%%% This module provides a reusable pattern for implementing authentication hooks
%%% that verify request headers and conditionally sign messages. It abstracts the
%%% common functionality found in dev_codec_cookie_hook and can be used with
%%% different authentication schemes (cookie, HTTP auth, etc.).
%%% 
%%% This module looks for a `hook_auth_provider' key in the options, and uses
%%% that to determine how to sign the request.
-module(dev_hook_request_auth).
-export([on_request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Default keys to ignore when signing
-define(DEFAULT_IGNORED_KEYS,
    [
        <<"cookie">>,
        <<"set-cookie">>,
        <<"path">>,
        <<"method">>,
        <<"authorization">>
    ]
).

%% @doc Process an incoming request through an authentication provider. The
%% provider should be a message optionally implementing the following keys:
%% <pre>
%%     `normalize-path': The path to call the `normalize' function.
%%     `finalize-path': The path to call the `finalize' function.
%%     `sign': Whether to sign the request.
%%     `ignored-keys': A list of keys to ignore when signing (can be overridden
%%     by the user request).
%% </pre>
%% 
on_request(_Base, HookReq, Opts) ->
    ExistingMessages = hb_maps:find(<<"body">>, HookReq, Opts),
    maybe
        % Get the auth provider from options and short-circuit if none is
        % provided.
        {ok, AuthProvider} ?= find_provider(HookReq, Opts),
        % Check if the request already has signatures
        {ok, Request} ?= hb_maps:find(<<"request">>, HookReq, Opts),
        [] ?= hb_message:signers(Request, Opts),
        ?event(auth_hook_no_signers),
        % Call the auth provider to normalize authentication (generate if needed)
        {ok, NormalizedReq, NewOpts} ?= normalize_auth(AuthProvider, Request, Opts),
        ?event(auth_hook_normalized),
        % Sign the request  
        {ok, SignedReq} ?= sign_request(AuthProvider, NormalizedReq, NewOpts),
        ?event(auth_hook_signed),
        % Process individual messages if needed
        {ok, MessageSequence} ?=
            maybe_sign_messages(
                AuthProvider,
                SignedReq,
                NewOpts
            ),
        ?event(auth_hook_processed_messages),
        % Call the auth provider to finalize the response
        {ok, FinalSequence} ?=
            finalize(
                AuthProvider,
                SignedReq,
                MessageSequence,
                NewOpts
            ),
        ?event({auth_hook_returning, FinalSequence}),
        {ok, #{ <<"body">> => FinalSequence }}
    else
        {error, Resp = #{ <<"status">> := 401 }} ->
            ?event({auth_hook_error, {unauthorized, Resp}}),
            {ok, #{ <<"body">> => Resp }};
        {error, no_auth_provider} ->
            ?event(auth_hook_no_auth_provider),
            {ok, #{ <<"body">> => ExistingMessages }};
        error ->
            ?event({auth_hook_error, no_request}),
            {ok, #{ <<"body">> => ExistingMessages }};
        ExistingSigners when is_list(ExistingSigners) ->
            ?event({auth_hook_skipping, {signers, ExistingSigners}}),
            {ok, #{ <<"body">> => ExistingMessages }};
        {error, Err} ->
            ?event({auth_hook_error, Err}),
            {ok, #{ <<"body">> => ExistingMessages }}
    end.

%% @doc Normalize authentication credentials, generating new ones if needed
normalize_auth(AuthProvider, Request, Opts) ->
    case call_provider(<<"normalize">>, AuthProvider, Request, Opts) of
        {error, not_found} ->
            ?event(auth_hook_no_auth_provider),
            {ok, Request, Opts};
        {ok, Normalized} ->
            ?event({auth_hook_normalized, Normalized}),
            NewOpts = hb_http_server:get_opts(Opts),
            case NewOpts of
                Opts -> ?event(auth_hook_no_opts_change);
                _ -> ?event({auth_hook_opts_changed, NewOpts})
            end,
            % Find the new options again, in case the auth provider modified them
            {ok, Normalized, NewOpts}
    end.

%% @doc Sign a request using the configured auth provider
sign_request(AuthProvider, Msg, Opts) ->
    case hb_maps:get(<<"sign">>, AuthProvider, true, Opts) of
        false ->
            % Skip signing and return the normalized message.
            ?event({provider_requested_signing_skip, AuthProvider}),
            {ok, Msg};
        true ->
            % Wallet signs without ignored keys
            IgnoredKeys = ignored_keys(Msg, Opts),
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
            end
    end.

%% @doc Process a sequence of messages, signing those marked for signing
maybe_sign_messages(AuthProvider, SignedReq, Opts) ->
    Parsed = hb_singleton:from(SignedReq, Opts),
    ?event({auth_hook_parsed_messages, {sequence_length, length(Parsed)}}),
    SignKey = hb_opts:get(auth_hook_commit_key, <<"!">>, Opts),
    Processed = maybe_sign_messages(AuthProvider, SignKey, Parsed, Opts),
    {ok, Processed}.
maybe_sign_messages(_AuthProvider, _Key, [], _Opts) -> [];
maybe_sign_messages(AuthProvider, Key, [Msg | Rest], Opts) when is_map(Msg) ->
    case hb_util:atom(hb_maps:get(Key, Msg, false, Opts)) of
        true ->
            Uncommitted = hb_message:uncommitted(Msg, Opts),
            ?event({auth_hook_signing_message, {uncommitted, Msg}}),
            case sign_request(AuthProvider, Uncommitted, Opts) of
                {ok, Signed} ->
                    [
                        Signed
                    |
                        maybe_sign_messages(AuthProvider, Key, Rest, Opts)
                    ];
                {error, Err} ->
                    ?event({auth_hook_sign_error, Err}),
                    [{error, Err}]
            end;
        _ ->
            [Msg | maybe_sign_messages(AuthProvider, Key, Rest, Opts)]
    end;
maybe_sign_messages(AuthProvider, Key, [Msg | Rest], Opts) ->
    [Msg | maybe_sign_messages(AuthProvider, Key, Rest, Opts)].

%% @doc Finalize the response by adding authentication state
finalize(AuthProvider, SignedReq, MessageSequence, Opts) ->
    % Add the signed request and message sequence to the response, mirroring the
    % structure of a normal `~hook@1.0' on-request hook.
    Req =
        #{
            <<"request">> => SignedReq,
            <<"body">> => MessageSequence
        },
    case call_provider(<<"finalize">>, AuthProvider, Req, Opts) of
        {ok, Finalized} ->
            ?event({auth_hook_finalized, Finalized}),
            {ok, Finalized};
        {error, not_found} ->
            ?event(auth_hook_no_finalize_handler),
            {ok, Req}
    end.

%%% Utility functions

%% @doc Get the auth provider from the base message or the defaults.
find_provider(Base, Opts) ->
    case hb_maps:get(<<"auth-provider">>, Base, Opts) of
        no_auth_provider ->
            case hb_opts:get(hook_auth_provider, no_auth_provider, Opts) of
                no_auth_provider -> {error, no_auth_provider};
                AuthProvider -> AuthProvider
            end;
        AuthProvider ->
            {ok, AuthProvider}
    end.

%% @doc Find the appropriate handler for a key in the auth provider.
call_provider(Key, AuthProvider, Request, Opts) ->
    ExecKey = hb_maps:get(<< Key, "-path">>, AuthProvider, Key, Opts),
    hb_ao:resolve(AuthProvider, Request#{ <<"path">> => ExecKey }, Opts).

%% @doc Default keys to ignore when signing
ignored_keys(Msg, Opts) ->
    hb_maps:get(
        <<"ignored-keys">>,
        Msg,
        hb_opts:get(
            hook_auth_ignored_keys,
            ?DEFAULT_IGNORED_KEYS,
            Opts
        )
    ).

%%% Tests

cookie_test() ->
    % Start a node with an auth-provider that uses the cookie device.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"auth-provider">> =>
                            #{ <<"device">> => <<"cookie@1.0">> }
                    }
                }
            }
        ),
    % Run a request and check that the response is signed. The cookie device
    % will generate a new cookie for the client.
    {ok, Response} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
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
    % Generate a further request and check that the same address is used. Extract
    % the cookie given in the first request and use it to sign the second.
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
    % Check that the second request is signed with the same address as the first.
    ?assertEqual(
        [CookieAddress],
        signers_from_commitments_response(Response2, ServerWallet)
    ).

http_auth_test() ->
    % Start a node with the `~http-auth@1.0' device as the auth-provider.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"auth-provider">> =>
                            #{ <<"device">> => <<"http-auth@1.0">> }
                    }
                }
            }
        ),
    % Run a request and check that the response is a 401 with the
    % `www-authenticate' header.
    Resp1 =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data">>
            },
            #{}
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 401, <<"www-authenticate">> := _ }},
        Resp1
    ),
    % Run a request with the `Authorization' header and check that the response
    % is signed.
    AuthStr = << "Basic ", (base64:encode(<<"user:pass">>))/binary >>,
    Resp2 =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data">>,
                <<"Authorization">> => AuthStr
            },
            #{}
        ),
    ?assertMatch(
        {ok, #{ <<"status">> := 200 }},
        Resp2
    ),
    % Filter the response to only include signed commitments.
    Signers = signers_from_commitments_response(Resp2, ServerWallet),
    ?event(
        {response, {found_signers, Signers}}
    ),    
    ?assertEqual(1, length(Signers)),
    % Generate a further request and check that the same address is used.
    [Signer] = Signers,
    {ok, Resp3} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data2">>,
                <<"Authorization">> => AuthStr
            },
            #{}
        ),
    ?assertEqual(
        [Signer],
        signers_from_commitments_response(Resp3, ServerWallet)
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