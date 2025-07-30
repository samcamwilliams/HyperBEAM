%%% @doc Implements a two-step authentication process for HTTP requests, using
%%% the `Basic' authentication scheme.
-module(dev_http_auth).
-export([generate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_SALT, <<"constant:ao">>).

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
    Iterations = hb_maps:get(<<"iterations">>, Req, 1000, Opts),
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