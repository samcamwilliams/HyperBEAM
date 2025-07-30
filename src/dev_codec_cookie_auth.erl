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
    SecretRes =
        case find_secret(Request, Opts) of
            {ok, RawSecret} -> {ok, RawSecret};
            {error, no_committer} ->
                generate_secret(Base, Request, Opts);
            {error, not_found} ->
                throw({error, <<"Necessary cookie not found in request.">>})
        end,
    case SecretRes of
        {ok, Secret} -> commit(Secret, Base, Request, Opts);
        {error, Err} -> {error, Err}
    end.

commit(Key, Base, Request, Opts) ->
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
    CookieAddr = dev_codec_httpsig_keyid:secret_key_to_committer(hb_util:decode(Key)),
    % Create the cookie parameters, using the name as the key and the secret as
    % the value.
    BaseCookieParams = #{ <<"secret-", CookieAddr/binary>> => Key },
    % Add the reference of the commitment request to the cookie, if it is
    % present.
    CookieParams =
        case hb_maps:find(<<"nonce">>, Request, Opts) of
            error -> BaseCookieParams;
            {ok, Reference} ->
                ExistingNonces = find_nonces(Request, Opts),
                BaseCookieParams#{
                    <<"nonces-", CookieAddr/binary>> =>
                        serialize_nonces(
                            [Reference | ExistingNonces],
                            Opts
                        )
                }
        end,
    % Set the cookie on the message with the new commitment and return.
    dev_codec_cookie:store(ModCommittedMsg, CookieParams, Opts).

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

generate_secret(_Base, Request, Opts) ->
    case hb_maps:get(<<"generator">>, Request, undefined, Opts) of
        undefined ->
            % If no generator is specified, use the default generator.
            case hb_opts:get(cookie_default_generator, <<"random">>, Opts) of
                <<"random">> ->
                    default_generator(Opts);
                Provider ->
                    execute_generator(Provider, Opts)
        end;
        Provider ->
            % Execute the user's generator function.
            execute_generator(Provider, Opts)
    end.

default_generator(_Opts) ->
    {ok, hb_util:encode(crypto:strong_rand_bytes(32))}.

execute_generator(GeneratorPath, Opts) when is_binary(GeneratorPath) ->
    hb_ao:resolve(GeneratorPath, Opts);
execute_generator(Generator, Opts) ->
    Path = hb_maps:get(<<"path">>, Generator, <<"generate">>, Opts),
    hb_ao:resolve(Generator#{ <<"path">> => Path }, Opts).

%% @doc Find the secret key for the given committer, if it exists in the cookie.
find_secret(Request, Opts) ->
    maybe
        {ok, Committer} ?= hb_maps:find(<<"committer">>, Request, Opts),
        find_secret(Committer, Request, Opts)
    else error -> {error, no_committer}
    end.
find_secret(Committer, Request, Opts) ->
    maybe
        {ok, Cookie} ?= dev_codec_cookie:extract(Request, #{}, Opts),
        {ok, _Secret} ?= hb_maps:find(<<"secret-", Committer/binary>>, Cookie, Opts)
    else error -> {error, not_found}
    end.

%% @doc Find the references for the given committer, if they exist in the cookie.
find_nonces(Request, Opts) ->
    maybe
        {ok, Cookie} ?= dev_codec_cookie:extract(Request, #{}, Opts),
        {ok, Committer} ?= hb_maps:find(<<"committer">>, Request, Opts),
        {ok, RefsBin} ?= hb_maps:find(<<"nonces-", Committer/binary>>, Cookie, Opts),
        deserialize_nonces(RefsBin, Opts)
    else error -> []
    end.

%% @doc Deserialize a reference string into a list of references.
deserialize_nonces(Reference, _Opts) ->
    binary:split(Reference, <<",">>, [global]).

%% @doc Serialize a list of references into a string.
serialize_nonces(References, _Opts) ->
    hb_util:bin(string:join(lists:map(fun hb_util:list/1, References), ", ")).

%%% Tests

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

%% @doc Set keys in a cookie and verify that they can be parsed into a message.
http_set_get_cookies_test() ->
    Node = hb_http_server:start_node(#{}),
    {ok, SetRes} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0/store?k1=v1&k2=v2">>,
            #{}
        ),
    ?event(debug_cookie, {set_cookie_test, {set_res, SetRes}}),
    ?assertMatch(#{ <<"set-cookie">> := _ }, SetRes),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/extract">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.

%%% Test Helpers

%% @doc Takes the cookies from the `GenerateResponse' and applies them to the
%% `Target' message.
apply_cookie(NextReq, GenerateResponse, Opts) ->
    {ok, Cookie} = dev_codec_cookie:extract(GenerateResponse, #{}, Opts),
    {ok, NextWithParsedCookie} = dev_codec_cookie:store(NextReq, Cookie, Opts),
    {ok, NextWithCookie} =
        dev_codec_cookie:to(
            NextWithParsedCookie,
            #{ <<"format">> => <<"cookie">> },
            Opts
        ),
    NextWithCookie.