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

%% @doc Process an incoming request through a key provider. The key provider
%% should be a message optionally implementing the following keys:
%% <pre>
%%     `generate-path': The path to call the `generate' function.
%%     `finalize-path': The path to call the `finalize' function.
%%     `skip-commit': Whether to skip committing the request.
%%     `ignored-keys': A list of keys to ignore when signing (can be overridden
%%     by the user request).
%% </pre>
%% 
on_request(Base, HookReq, Opts) ->
    ?event({auth_hook_request, {base, Base}, {hook_req, HookReq}}),
    ExistingMessages = hb_maps:find(<<"body">>, HookReq, Opts),
    maybe
        % Get the key provider from options and short-circuit if none is
        % provided.
        {ok, Provider} ?= find_provider(Base, Opts),
        % Check if the request already has signatures
        {ok, Request} ?= hb_maps:find(<<"request">>, HookReq, Opts),
        [] ?= hb_message:signers(Request, Opts),
        ?event(auth_hook_no_signers),
        % Call the key provider to normalize authentication (generate if needed)
        {ok, IntermediateProvider, NormReq} ?=
            generate_key(Provider, Request, Opts),
        % Call `~wallet@1.0' to generate a wallet if needed. Returns refreshed
        % options.
        {ok, NormProvider, NewOpts} ?=
            generate_wallet(IntermediateProvider, NormReq, Opts),
        ?event(
            {auth_hook_normalized,
                {intermediate_provider, IntermediateProvider},
                {norm_provider, NormProvider},
                {norm_req, NormReq}
            }
        ),
        % Sign the full request  
        {ok, SignedReq} ?= sign_request(NormProvider, NormReq, NewOpts),
        ?event(auth_hook_signed),
        % Process individual messages if needed
        {ok, MessageSequence} ?=
            maybe_sign_messages(
                NormProvider,
                SignedReq,
                NewOpts
            ),
        ?event(auth_hook_processed_messages),
        % Call the key provider to finalize the response
        {ok, FinalSequence} ?=
            finalize(
                NormProvider,
                SignedReq,
                MessageSequence,
                NewOpts
            ),
        ?event({auth_hook_returning, FinalSequence}),
        {ok, #{ <<"body">> => FinalSequence }}
    else
        {error, AuthError} ->
            ?event({auth_hook_auth_error, AuthError}),
            {error, AuthError};
        error ->
            ?event({auth_hook_error, no_request}),
            {ok, #{ <<"body">> => ExistingMessages }};
        ExistingSigners when is_list(ExistingSigners) ->
            ?event({auth_hook_skipping, {signers, ExistingSigners}}),
            {ok, #{ <<"body">> => ExistingMessages }};
        Other ->
            ?event({auth_hook_unexpected_result, Other}),
            Other
    end.

%% @doc Normalize authentication credentials, generating new ones if needed.
generate_key(Provider, Request, Opts) ->
    case call_provider(<<"generate">>, Provider, Request, Opts) of
        {error, not_found} ->
            ?event({no_generate_handler, Provider}),
            {ok, Provider, strip_sensitive(Request, Opts)};
        {error, Err} ->
            % Forward the error. The main handler will fail to match this and
            % return the error to the user.
            ?event({generate_error, Err}),
            {error, Err};
        {ok, Key} when is_binary(Key) ->
            % The provider returned a direct key, calculate the committer and
            % generate a wallet for it, if needed.
            ?event({key_from_provider, Key}),
            {ok, Provider#{ <<"key">> => Key }, strip_sensitive(Request, Opts)};
        {ok, NormalizedReq} when is_map(NormalizedReq) ->
            % If there is a `wallet' field in the request, we move it to the
            % provider, else continue with the existing provider.
            ?event({normalized_req, NormalizedReq}),
            case hb_maps:find(<<"key">>, NormalizedReq, Opts) of
                {ok, Key} ->
                    ?event({key_found_in_normalized_req, Key}),
                    {
                        ok,
                        Provider#{ <<"key">> => Key },
                        strip_sensitive(NormalizedReq, Opts)
                    };
                error ->
                    ?event({no_key_in_normalized_req, NormalizedReq}),
                    {ok, Provider, strip_sensitive(NormalizedReq, Opts)}
            end
    end.

%% @doc Strip the `key' field from a request.
strip_sensitive(Request, Opts) ->
    hb_maps:without([<<"key">>], Request, Opts).

%% @doc Generate a wallet with the key if the `wallet' field is not present in
%% the provider after normalization.
generate_wallet(Provider, Request, Opts) ->
    {ok, #{ <<"body">> := WalletID }} =
        dev_wallet:generate(Provider, Request, Opts),
    ?event({generated_wallet, WalletID}),
    {ok, Provider, refresh_opts(Opts)}.

%% @doc Sign a request using the configured key provider
sign_request(Provider, Msg, Opts) ->
    case hb_maps:get(<<"skip-commit">>, Provider, true, Opts) of
        false ->
            % Skip signing and return the normalized message.
            ?event({provider_requested_signing_skip, Provider}),
            {ok, Msg};
        true ->
            % Wallet signs without ignored keys
            IgnoredKeys = ignored_keys(Msg, Opts),
            WithoutIgnored = hb_maps:without(IgnoredKeys, Msg, Opts),
            % Call the wallet to sign the request.
            case dev_wallet:commit(WithoutIgnored, Provider, Opts) of
                {ok, Signed} ->
                    ?event({auth_hook_signed, Signed}),
                    SignedWithIgnored = 
                        hb_maps:merge(
                            Signed,
                            hb_maps:with(IgnoredKeys, Msg, Opts),
                            Opts
                        ),
                    {ok, SignedWithIgnored};
                {error, Err} ->
                    ?event({auth_hook_sign_error, Err}),
                    {error, Err}
            end
    end.

%% @doc Process a sequence of messages, signing those marked for signing
maybe_sign_messages(Provider, SignedReq, Opts) ->
    Parsed = hb_singleton:from(SignedReq, Opts),
    ?event({auth_hook_parsed_messages, {sequence_length, length(Parsed)}}),
    SignKey = hb_opts:get(auth_hook_commit_key, <<"!">>, Opts),
    Processed = maybe_sign_messages(Provider, SignKey, Parsed, Opts),
    {ok, Processed}.
maybe_sign_messages(_Provider, _Key, [], _Opts) -> [];
maybe_sign_messages(Provider, Key, [Msg | Rest], Opts) when is_map(Msg) ->
    case hb_util:atom(hb_maps:get(Key, Msg, false, Opts)) of
        true ->
            Uncommitted = hb_message:uncommitted(Msg, Opts),
            ?event({auth_hook_signing_message, {uncommitted, Msg}}),
            case sign_request(Provider, Uncommitted, Opts) of
                {ok, Signed} ->
                    [
                        Signed
                    |
                        maybe_sign_messages(Provider, Key, Rest, Opts)
                    ];
                {error, Err} ->
                    ?event({auth_hook_sign_error, Err}),
                    [{error, Err}]
            end;
        _ ->
            [Msg | maybe_sign_messages(Provider, Key, Rest, Opts)]
    end;
maybe_sign_messages(Provider, Key, [Msg | Rest], Opts) ->
    [Msg | maybe_sign_messages(Provider, Key, Rest, Opts)].

%% @doc Finalize the response by adding authentication state
finalize(KeyProvider, SignedReq, MessageSequence, Opts) ->
    % Add the signed request and message sequence to the response, mirroring the
    % structure of a normal `~hook@1.0' on-request hook.
    Req =
        #{
            <<"request">> => SignedReq,
            <<"body">> => MessageSequence
        },
    case call_provider(<<"finalize">>, KeyProvider, Req, Opts) of
        {ok, Finalized} ->
            ?event({auth_hook_finalized, Finalized}),
            {ok, Finalized};
        {error, not_found} ->
            ?event(auth_hook_no_finalize_handler),
            {ok, MessageSequence}
    end.

%%% Utility functions

%% @doc Refresh the options and log an event if they have changed.
refresh_opts(Opts) ->
    NewOpts = hb_http_server:get_opts(Opts),
    case NewOpts of
        Opts -> ?event(auth_hook_no_opts_change);
        _ ->
            ?event(
                {auth_hook_opts_changed,
                    {size_diff,
                        erlang:external_size(NewOpts) -
                            erlang:external_size(Opts)
                    }
                }
            )
    end,
    NewOpts.

%% @doc Get the key provider from the base message or the defaults.
find_provider(Base, Opts) ->
    case hb_maps:get(<<"key-provider">>, Base, no_key_provider, Opts) of
        no_key_provider ->
            case hb_opts:get(hook_key_provider, no_key_provider, Opts) of
                no_key_provider -> {error, no_key_provider};
                KeyProvider -> KeyProvider
            end;
        KeyProvider when is_binary(KeyProvider) ->
            {ok, #{ <<"device">> => KeyProvider }};
        KeyProvider when is_map(KeyProvider) ->
            {ok, KeyProvider};
        _ ->
            {error, invalid_auth_provider}
    end.

%% @doc Find the appropriate handler for a key in the key provider.
call_provider(Key, KeyProvider, Request, Opts) ->
    ?event({call_provider, {key, Key}, {key_provider, KeyProvider}, {req, Request}}),
    ExecKey = hb_maps:get(<< Key/binary, "-path">>, KeyProvider, Key, Opts),
    ?event({call_provider, {exec_key, ExecKey}}),
    case hb_ao:resolve(KeyProvider, Request#{ <<"path">> => ExecKey }, Opts) of
        {ok, Msg} when is_map(Msg) ->
            % The result is a message. We revert the path to its original value.
            case hb_maps:find(<<"path">>, Request, Opts) of
                {ok, Path} -> {ok, Msg#{ <<"path">> => Path }};
                _ -> {ok, Msg}
            end;
        {ok, _} = Res ->
            % The result is a non-message. We return it as-is.
            Res;
        {error, Err} ->
            ?event({call_provider_error, Err}),
            {error, Err}
    end.

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
    % Start a node with an key-provider that uses the cookie device.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"key-provider">> =>
                            #{
                                <<"device">> => <<"cookie@1.0">>
                            }
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
    % Start a node with the `~http-auth@1.0' device as the key-provider.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"key-provider">> =>
                            #{
                                <<"device">> => <<"http-auth@1.0">>,
                                <<"auth">> =>
                                    #{ <<"device">> => <<"http-auth@1.0">> }
                            }
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
                <<"authorization">> => AuthStr
            },
            #{}
        ),
    ?assertMatch(
        {ok, #{ <<"status">> := 200 }},
        Resp2
    ),
    % Filter the response to only include signed commitments.
    Signers = signers_from_commitments_response(hb_util:ok(Resp2), ServerWallet),
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
                <<"authorization">> => AuthStr
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