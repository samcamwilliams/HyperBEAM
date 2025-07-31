%%% @doc A utility device that manages setting and encoding/decoding the cookies
%%% found in requests from the caller. This device implements the `~cookie@1.0'
%%% codec, inline with the `~message@1.0' schema for conversion. It does not
%%% support commitments, but does act as a valid target format for unsigned
%%% messages.
%%% 
%%% Additionally, a `commit' and `verify' API is provided to commit and
%%% verify a secret stored in the cookies of the caller.
%%% 
%%% The cryptographic scheme used in the commit and verify APIs is as follows:
%%% 
%%%     commit:
%%%         secret = 32 random bytes or existing secret from cookie
%%%         name = base64url(sha256(secret))
%%%         commitment = sha256(secret) and scheme:secret
%%%         return cookie(#{ name => secret })
%%% 
%%%     verify:
%%%         committer = req.committer
%%%         expecting cookie format in form of #{
%%%             committer => secret,
%%%         }
%%%         return httpsig@1.0 message verified with key = secret
%%% 
%%% 
%%% This device supports the following paths:
%%% 
%%% `/commit': Sets a `secret' key in the cookies of the caller. The name of 
%%% the cookie is calculated as the hash of the secret. 
%%% `/verify': Verifies the caller's request by checking the committer in the
%%% request matches the secret in the cookies of the base message.
%%% `/set-cookie': Sets the keys in the request message in the cookies of the
%%% caller.
-module(dev_codec_cookie).
%%% Public cookie manipulation API.
-export([get_cookie/3, store/3, extract/3, reset/2]).
%%% Public message codec API.
-export([to/3, from/3]).
%%% Public commit/verify API.
-export([commit/3, verify/3]).
%%% Preprocessor hook API.
-export([normalize/3, finalize/3]).
%%% Public utility functions.
-export([opts/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Get the private store options to use for functions in the cookie device.
opts(Opts) -> hb_private:opts(Opts).

%%% ~message@1.0 Commitments API keys.
commit(Base, Req, RawOpts) -> dev_codec_cookie_auth:commit(Base, Req, RawOpts).
verify(Base, Req, RawOpts) -> dev_codec_cookie_auth:verify(Base, Req, RawOpts).

%% @doc Preprocessor keys that utilize cookies and the `~wallet@1.0' device to
%% sign inbound HTTP requests from users if they are not already signed. We use
%% the `~hook@1.0' authentication framework to implement this.
normalize(Base, Req, Opts) ->
    dev_codec_cookie_auth:normalize(Base, Req, Opts).

%% @doc Finalize an `on-request' hook by adding the `set-cookie' header to the
%% end of the message sequence.
finalize(Base, Request, Opts) ->
    dev_codec_cookie_auth:finalize(Base, Request, Opts).

%% @doc Get the cookie with the given key from the base message. The format of
%% the cookie is determined by the `format' key in the request message:
%% - `default': The cookie is returned in its raw form. It will be a message
%%   if the source was a `set-cookie' header line containing attributes/flags,
%%   or a binary if only the value was provided (as with the `cookie' header).
%% - `set-cookie': The cookie is normalized to a message with `value',
%%   `attributes', and `flags' keys.
%% - `cookie': The cookie is normalized to a binary, ommitting any attributes
%%   or flags.
%% 
%% The `format' may be specified in the request message as the `req:format' key.
%% If no `format' is specified, the default is `default'.
get_cookie(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    {ok, Cookies} = extract(Base, Req, Opts),
    Key = hb_maps:get(<<"key">>, Req, undefined, Opts),
    case hb_maps:get(Key, Cookies, undefined, Opts) of
        undefined -> {error, not_found};
        Cookie ->
            Format = hb_maps:get(<<"format">>, Req, <<"default">>, Opts),
            case Format of
                <<"default">> -> {ok, Cookie};
                <<"set-cookie">> -> {ok, normalize_cookie_value(Cookie)};
                <<"cookie">> -> {ok, value(Cookie)}
            end
    end.

%% @doc Return the parsed and normalized cookies from a message.
extract(Msg, Req, Opts) ->
    {ok, MsgWithCookie} = from(Msg, Req, Opts),
    Cookies = hb_private:get(<<"cookie">>, MsgWithCookie, #{}, Opts),
    {ok, Cookies}.

%% @doc Set the keys in the request message in the cookies of the caller. Removes
%% a set of base keys from the request message before setting the remainder as
%% cookies.
store(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    ?event({store, {base, Base}, {req, Req}}),
    {ok, ExistingCookies} = extract(Base, Req, Opts),
    ?event({store, {existing_cookies, ExistingCookies}}),
    {ok, ResetBase} = reset(Base, Opts),
    ?event({store, {reset_base, ResetBase}}),
    MsgToSet =
        hb_maps:without(
            [
                <<"path">>,
                <<"accept-bundle">>,
                <<"ao-peer">>,
                <<"host">>,
                <<"method">>,
                <<"body">>
            ],
            hb_private:reset(Req),
            Opts
        ),
    ?event({store, {msg_to_set, MsgToSet}}),
    NewCookies = hb_maps:merge(ExistingCookies, MsgToSet, Opts),
    NewBase = hb_private:set(ResetBase, <<"cookie">>, NewCookies, Opts),
    {ok, NewBase}.

%% @doc Remove all cookie keys from the given message (including `cookie' and
%% `set-cookie' in the base, and `priv/cookie' in the request message).
reset(Base, RawOpts) ->
    Opts = opts(RawOpts),
    WithoutBaseCookieKeys =
        hb_maps:without(
            [<<"cookie">>, <<"set-cookie">>],
            Base,
            Opts
        ),
    WithoutPrivCookie =
        hb_private:set(
            WithoutBaseCookieKeys,
            <<"cookie">>,
            unset,
            Opts
        ),
    {ok, WithoutPrivCookie}.

%% @doc Convert a message containing cookie sources (`cookie', `set-cookie',
%% or `priv/cookie') into a message containing the cookies serialized as the
%% specified `format' (given in the request message). The `format' may take the
%% following values:
%% 
%% - `set-cookie': A list of encoded cookie binary header lines (e.g.
%%   `"key1=value1; attr1=value2; flag1; flag2..."').
%% - `cookie': A single, concatenated cookie header line without attributes or
%%   flags (e.g. `"key1=value1; key2=value2; ..."').
%% 
%% Note that the `format: cookie' form is information lossy: All provided
%% attributes and flags are discarded.
to(Msg, Req, Opts) ->
    ?event({to, {msg, Msg}, {req, Req}}),
    CookieOpts = opts(Opts),
    LoadedMsg = hb_cache:ensure_all_loaded(Msg, CookieOpts),
    ?event({to, {loaded_msg, LoadedMsg}}),
    do_to(LoadedMsg, Req, CookieOpts).
do_to(Msg, Req = #{ <<"format">> := <<"set-cookie">> }, Opts) when is_map(Msg) ->
    ?event({to_set_cookie, {msg, Msg}, {req, Req}}),
    {ok, ExtractedParsedCookies} = extract(Msg, Req, Opts),
    {ok, ResetBase} = reset(Msg, Opts),
    SetCookieLines =
        maps:values(
            maps:map(
                fun to_set_cookie_line/2,
                ExtractedParsedCookies
            )
        ),
    MsgWithSetCookie =
        ResetBase#{
            <<"set-cookie">> => SetCookieLines
        },
    {ok, MsgWithSetCookie};
do_to(Msg, Req = #{ <<"format">> := <<"cookie">> }, Opts) when is_map(Msg) ->
    ?event({to_cookie, {msg, Msg}, {req, Req}}),
    {ok, ExtractedParsedCookies} = extract(Msg, Req, Opts),
    {ok, ResetBase} = reset(Msg, Opts),
    CookieLines =
        hb_maps:values(
            hb_maps:map(
            fun to_cookie_line/2,
                ExtractedParsedCookies,
                Opts
            ),
            Opts
        ),
    ?event({to_cookie, {cookie_lines, CookieLines}}),
    CookieLine = join(CookieLines, <<"; ">>),
    {ok, ResetBase#{ <<"cookie">> => CookieLine }};
do_to(Msg, _Req, _Opts) when is_map(Msg) ->
    error({cookie_to_error, {no_format_specified, Msg}});
do_to(Msg, _Req, _Opts) ->
    error({cookie_to_error, {unexpected_message_format, Msg}}).

%% @doc Convert a single cookie into a `set-cookie' header line. The cookie 
%% may come in the form of `key => binary' or `key => cookie-message', where
%% the cookie-message is a map with the following keys:
%% 
%% - `value': The raw binary cookie value.
%% - `attributes': A map of cookie attribute key-value pairs.
%% - `flags`: A list of cookie flags, represented as binaries.
%% 
%% If the cookie is a binary, we normalize it to a cookie-message before 
%% processing.
%% Note: Assumes that the cookies have all been loaded from the cache fully.
to_set_cookie_line(Key, RawCookie) ->
    Cookie = normalize_cookie_value(RawCookie),
    % Encode the cookie key-value pair as a string to use as the base.
    ValueBin =
        <<
            Key/binary, "=\"",
            (maps:get(<<"value">>, Cookie))/binary,
            "\""
        >>,
    % Encode the cookie attributes as key-value (non-quoted) pairs, separated
    % by `;'.
    ?event({to_line, {key, Key}, {cookie, {explicit, Cookie}}, {value, ValueBin}}),
    AttributesBin =
        case maps:get(<<"attributes">>, Cookie, #{}) of
            EmptyAttributes when map_size(EmptyAttributes) == 0 ->
                ?event({attributes, {none_in, Cookie}}),
                <<>>;
            Attributes ->
                ?event({attributes, Attributes}),
                JointAttributes =
                    join(
                        [
                                << AttrKey/binary, "=", AttrValue/binary >>
                            ||
                                {AttrKey, AttrValue} <- to_sorted_list(Attributes)
                        ],
                        <<"; ">>
                    ),
                << "; ", JointAttributes/binary >>
        end,
    FlagsBin =
        case maps:get(<<"flags">>, Cookie, []) of
            [] -> <<>>;
            Flags -> << "; ", (join(Flags, <<"; ">>))/binary >>
        end,
    << ValueBin/binary, AttributesBin/binary, FlagsBin/binary >>.

%% @doc Convert a single cookie into a `cookie' header component. These
%% components can be joined to form a `cookie' header line. This function
%% reuses the `to_set_cookie_line' function to generate the components, but
%% unsets the `attributes' and `flags' keys first.
to_cookie_line(Key, Cookie) ->
    to_set_cookie_line(Key, value(Cookie)).

%% @doc Normalize a message containing a `cookie', `set-cookie', and potentially
%% a `priv/cookie' key into a message with only the `priv/cookie' key.
from(Msg, Req, Opts) ->
    CookieOpts = opts(Opts),
    LoadedMsg = hb_cache:ensure_all_loaded(Msg, Opts),
    do_from(LoadedMsg, Req, CookieOpts).
do_from(Msg, Req, Opts) when is_map(Msg) ->
    {ok, ResetBase} = reset(Msg, Opts),
    % Get the cookies, parsed, from each available source.
    {ok, FromCookie} = from_cookie(Msg, Req, Opts),
    {ok, FromSetCookie} = from_set_cookie(Msg, Req, Opts),
    FromPriv = hb_private:get(<<"cookie">>, Msg, #{}, Opts),
    % Merge all found cookies into a single map.
    MergedMsg = hb_maps:merge(FromCookie, FromSetCookie, Opts),
    AllParsed = hb_maps:merge(MergedMsg, FromPriv, Opts),
    % Set the cookies in the private element of the message.
    {ok, hb_private:set(ResetBase, <<"cookie">>, AllParsed, Opts)};
do_from(CookiesMsg, _Req, _Opts) ->
    error({cookie_from_error, {unexpected_message_format, CookiesMsg}}).

%% @doc Convert the `cookie' key into a parsed cookie message. `cookie' headers
%% are in the format of `key1=value1; key2=value2; ...'. There are no attributes
%% or flags, so we split on `;' and return a map of key-value pairs. We also
%% decode the values, in case they are URI-encoded.
from_cookie(#{ <<"cookie">> := Cookie }, Req, Opts) ->
    from_cookie(Cookie, Req, Opts);
from_cookie(Cookies, Req, Opts) when is_list(Cookies) ->
    MergedParsed =
        lists:foldl(
            fun(Cookie, Acc) ->
                {ok, Parsed} = from_cookie(Cookie, Req, Opts),
                hb_maps:merge(Acc, Parsed, Opts)
            end,
            #{},
            Cookies
        ),
    {ok, MergedParsed};
from_cookie(Cookie, _Req, _Opts) when is_binary(Cookie) ->
    BinaryCookiePairs = split(semicolon, Cookie),
    KeyValList =
        lists:map(
            fun(BinaryCookiePair) ->
                {[Key, Value], _Rest} = split(pair, BinaryCookiePair),
                {Key, hb_escape:decode(Value)}
            end,
            BinaryCookiePairs
        ),
    NormalizedMessage = maps:from_list(KeyValList),
    {ok, NormalizedMessage};
from_cookie(_MsgWithoutCookie, _Req, _Opts) ->
    % The cookie key is not present in the message, so we return an empty map.
    {ok, #{}}.

%% @doc Convert a `set-cookie' header line into a cookie message. The `set-cookie'
%% header has a `key=value' pair, and possibly attributes and flags. The form
%% looks as follows: `key=value; attr1=value1; attr2=value2; flag1; flag2'.
from_set_cookie(#{ <<"set-cookie">> := Cookie }, Req, Opts) ->
    ?event({from_set_cookie, {cookie, Cookie}}),
    from_set_cookie(Cookie, Req, Opts);
from_set_cookie(MsgWithoutSet, _Req, _Opts) when is_map(MsgWithoutSet) ->
    % The set-cookie key is not present in the message, so we return an empty map.
    {ok, #{}};
from_set_cookie(Lines, Req, Opts) when is_list(Lines) ->
    MergedParsed =
        lists:foldl(
            fun(Line, Acc) ->
                {ok, Parsed} = from_set_cookie(Line, Req, Opts),
                hb_maps:merge(Acc, Parsed)
            end,
            #{},
            Lines
        ),
    {ok, MergedParsed};
from_set_cookie(Line, _Req, Opts) when is_binary(Line) ->
    {[Key, Value], Rest} = split(pair, Line),
    ValueDecoded = hb_escape:decode(Value),
    % If there is no remaining binary after the pair, we have a simple key-value
    % pair, returning just the binary as the value. Otherwise, we split the
    % remaining binary into attributes and flags and return a message with the
    % value and those parsed elements.
    case Rest of
        <<>> -> {ok, #{ Key => ValueDecoded }};
        _ ->
            AllAttrs = split(semicolon, Rest),
            % We partition the attributes into pairs and flags, where flags are
            % any attributes that do not contain an `=' character.
            {AttrPairs, Flags} =
                lists:partition(
                    fun(Attr) ->
                        case hb_util:split_depth_string_aware_single($=, Attr) of
                            {no_match, _, _} -> false;
                            {_, _, _} -> true
                        end
                    end,
                    AllAttrs
                ),
            % We sort the flags and generate an attributes map from the pairs.
            SortedFlags = to_sorted_list(Flags),
            UnquotedFlags = lists:map(fun unquote/1, SortedFlags),
            ?event(
                {from_line,
                    {key, Key},
                    {value, {explicit, Value}},
                    {attrs, AttrPairs},
                    {flags, UnquotedFlags}
                }
            ),
            Attributes =
                maps:from_list(
                    lists:map(
                        fun(AttrPairBin) ->
                            {[AttrKey, AttrValue], _} = split(pair, AttrPairBin),
                            AttrKeyTrimmed = trim_bin(AttrKey),
                            AttrValueTrimmed = trim_bin(AttrValue),
                            {AttrKeyTrimmed, unquote(AttrValueTrimmed)}
                        end,
                        AttrPairs
                    )
                ),
            MaybeAttributes =
                if map_size(Attributes) > 0 -> #{ <<"attributes">> => Attributes };
                true -> #{}
                end,
            MaybeFlags =
                if length(UnquotedFlags) > 0 -> #{ <<"flags">> => UnquotedFlags };
                true -> #{}
                end,
            MaybeAllAttributes = hb_maps:merge(MaybeAttributes, MaybeFlags, Opts),
            {ok, #{ Key => MaybeAllAttributes#{ <<"value">> => ValueDecoded }}}
    end.

%%% Internal helpers

%% @doc Takes a message or list of binaries and returns a sorted list of key-
%% value pairs. Assumes that the message has been loaded from the cache fully.
to_sorted_list(Msg) when is_map(Msg) ->
    lists:keysort(
        1,
        [
            {trim_bin(hb_util:bin(K)), trim_bin(V)}
            || {K, V} <- maps:to_list(Msg)
        ]
    );
to_sorted_list(Binaries) when is_list(Binaries) ->
    lists:sort(
        lists:map(
            fun(Bin) -> trim_bin(hb_util:bin(Bin)) end,
            Binaries
        )
    ).

%% @doc Take a single parse cookie and return only the value (ignoring attributes
%% and flags).
value(Msg) when is_map(Msg) ->
    maps:get(<<"value">>, Msg, Msg);
value(Bin) when is_binary(Bin) ->
    Bin.

%% @doc Normalize a cookie value to a map with the following keys:
%% - `value': The raw binary cookie value.
%% - `attributes': A map of cookie attribute key-value pairs.
%% - `flags`: A list of cookie flags, represented as binaries.
normalize_cookie_value(Msg) when is_map(Msg) ->
    Msg#{
        <<"value">> => maps:get(<<"value">>, Msg, Msg),
        <<"attributes">> => maps:get(<<"attributes">>, Msg, #{}),
        <<"flags">> => maps:get(<<"flags">>, Msg, [])
    };
normalize_cookie_value(Bin) when is_binary(Bin) ->
    #{
        <<"value">> => Bin,
        <<"attributes">> => #{},
        <<"flags">> => []
    }.

%% Helper
trim_bin(Bin) when is_binary(Bin) ->
    list_to_binary(string:trim(binary_to_list(Bin))).

%% @doc Join a list of binaries into a `separator'-separated string. Abstracts
%% the complexities of converting to/from string lists, as Erlang only provides
%% a `binary:join` function as of OTP/28.
join(Binaries, Separator) ->
    hb_util:bin(
        string:join(
            lists:map(fun hb_util:list/1, Binaries),
            hb_util:list(Separator)
        )
    ).

%% @doc Split a binary by a separator type (`pair', `lines', or `attributes').
%% Separator types that are plural return a list of all parts. Singular types
%% return a single part and the remainder of the binary.
split(pair, Bin) ->
    [Key, ValueRest] = binary:split(Bin, <<"=">>),
    {_, Value, Rest} = hb_util:split_depth_string_aware_single($;, ValueRest),
    {[Key, unquote(Value)], trim_leading(Rest)};
split(lines, Bin) ->
    lists:map(fun trim_leading/1, hb_util:split_depth_string_aware($,, Bin));
split(semicolon, Bin) ->
    lists:map(fun trim_leading/1, hb_util:split_depth_string_aware($;, Bin)).

%% @doc Remove leading whitespace from a binary, if present.
trim_leading(Line) when not is_binary(Line) ->
    trim_leading(hb_util:bin(Line));
trim_leading(<<>>) -> <<>>;
trim_leading(<<" ", Rest/binary>>) -> trim_leading(Rest);
trim_leading(Line) -> Line.

%% @doc Unquote a binary if it is quoted. If it is not quoted, we return the
%% binary as is.
unquote(<< $\", Rest/binary>>) ->
    {Unquoted, _} = hb_util:split_escaped_single($\", Rest),
    Unquoted;
unquote(Bin) -> Bin.

%%% Tests

%% @doc returns a map of tuples of the form `testset_name => {[before], after}'.
%% These sets are used to test the correctness of the parsing and serialization
%% of cookie messages. The `before` is a list of inputs for which all of the
%% outputs are expected to match the `after' value.
test_data() ->
    #{
        from_string_raw_value =>
            {
                [<<"k1=v1">>, <<"k1=\"v1\"">>],
                #{ <<"k1">> => <<"v1">> }
            },
        from_string_attributes =>
            {
                [<<"k1=v1; k2=v2">>, <<"k1=\"v1\"; k2=\"v2\"">>],
                #{
                    <<"k1">> =>
                        #{
                            <<"value">> => <<"v1">>,
                            <<"attributes">> => #{ <<"k2">> => <<"v2">> }
                        }
                }
            },
        from_string_flags =>
            {
                [<<"k1=v1; k2=v2; f1; f2">>, <<"k1=\"v1\"; k2=\"v2\"; f1; f2">>],
                #{
                    <<"k1">> =>
                        #{
                            <<"value">> => <<"v1">>,
                            <<"attributes">> => #{ <<"k2">> => <<"v2">> },
                            <<"flags">> => [<<"f1">>, <<"f2">>]
                        }
                }
            },
        to_string_raw_value =>
            {
                [
                    #{ <<"k1">> => <<"v1">> },
                    #{ <<"k1">> => #{ <<"value">> => <<"v1">> } },
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{},
                                <<"flags">> => []
                            }
                    }
                ],
                [<<"k1=\"v1\"">>]
            },
        to_string_attributes =>
            {
                [
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{ <<"k2">> => <<"v2">> }
                            }
                    },
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{ <<"k2">> => <<"v2">> },
                                <<"flags">> => []
                            }
                    }
                ],
                [<<"k1=\"v1\"; k2=v2">>]
            },
        to_string_flags =>
            {
                [
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"flags">> => [<<"f1">>, <<"f2">>]
                            }
                    },
                    #{
                        <<"k1">> =>
                            #{
                                <<"value">> => <<"v1">>,
                                <<"attributes">> => #{},
                                <<"flags">> => [<<"f1">>, <<"f2">>]
                            }
                    }
                ],
                [<<"k1=\"v1\"; f1; f2">>]
            },
        parse_realworld_1 =>
            {
                [
                    [
                        <<"cart=110045_77895_53420; SameSite=Strict">>,
                        <<"affiliate=e4rt45dw; SameSite=Lax">>
                    ]
                ],
                #{
                    <<"cart">> =>
                        #{
                            <<"value">> => <<"110045_77895_53420">>,
                            <<"attributes">> => #{ <<"SameSite">> => <<"Strict">> }
                        },
                    <<"affiliate">> =>
                        #{
                            <<"value">> => <<"e4rt45dw">>,
                            <<"attributes">> => #{ <<"SameSite">> => <<"Lax">> }
                        }
                }
            },
        parse_user_settings_and_permissions =>
            {
                [
                    [
                        <<"user_settings=notifications=true,privacy=strict,layout=grid; Path=/; HttpOnly; Secure">>,
                        <<"user_permissions=\"read;write;delete\"; Path=/; SameSite=None; Secure">>
                    ]
                ],
                #{
                    <<"user_settings">> =>
                        #{
                            <<"value">> => <<"notifications=true,privacy=strict,layout=grid">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                        },
                    <<"user_permissions">> =>
                        #{
                            <<"value">> => <<"read;write;delete">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">>, <<"SameSite">> => <<"None">> },
                            <<"flags">> => [<<"Secure">>]
                        }
                }
            },
        parse_session_and_temp_data =>
            {
                [
                    [
                        <<"SESSION_ID=abc123xyz ; path= /dashboard ; samesite=Strict ; Secure">>,
                        <<"temp_data=cleanup_me; Max-Age=-1; Path=/">>
                    ]
                ],
                #{
                    <<"SESSION_ID">> =>
                        #{
                            <<"value">> => <<"abc123xyz ">>,
                            <<"attributes">> => #{ <<"path">> => <<"/dashboard">>, <<"samesite">> => <<"Strict">> },
                            <<"flags">> => [<<"Secure">>]
                        },
                    <<"temp_data">> =>
                        #{
                            <<"value">> => <<"cleanup_me">>,
                            <<"attributes">> => #{ <<"Max-Age">> => <<"-1">>, <<"Path">> => <<"/">> }
                        }
                }
            },
        parse_empty_and_anonymous =>
            {
                [
                    [
                        <<"user_preference=; Path=/; HttpOnly">>,
                        <<"=anonymous_session_123; Path=/guest">>
                    ]
                ],
                #{
                    <<"user_preference">> =>
                        #{
                            <<"value">> => <<"">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>]
                        },
                    <<>> =>
                        #{
                            <<"value">> => <<"anonymous_session_123">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/guest">> }
                        }
                }
            },
        parse_app_config_and_analytics =>
            {
                [
                    [
                        <<"$app_config$=theme@dark!%20mode; Path=/">>,
                        <<"analytics_session_data_with_very_long_name_for_tracking_purposes=comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more; Path=/">>
                    ]
                ],
                #{
                    <<"$app_config$">> =>
                        #{
                            <<"value">> => <<"theme@dark! mode">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"analytics_session_data_with_very_long_name_for_tracking_purposes">> =>
                        #{
                            <<"value">> => <<"comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        }
                }
            },
        parse_debug_and_tracking =>
            {
                [
                    [
                        <<"debug_info=\\tIndented\\t\\nMultiline\\n; Path=/">>,
                        <<"tracking_id=user_12345; CustomAttr=CustomValue; Analytics=Enabled; Path=/; HttpOnly">>
                    ]
                ],
                #{
                    <<"debug_info">> =>
                        #{
                            <<"value">> => <<"\\tIndented\\t\\nMultiline\\n">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"tracking_id">> =>
                        #{
                            <<"value">> => <<"user_12345">>,
                            <<"attributes">> => #{
                                <<"CustomAttr">> => <<"CustomValue">>,
                                <<"Analytics">> => <<"Enabled">>,
                                <<"Path">> => <<"/">>
                            },
                            <<"flags">> => [<<"HttpOnly">>]
                        }
                }
            },
        parse_cache_and_form_token =>
            {
                [
                    [
                        <<"cache_bust=v1.2.3; Expires=Mon, 99 Feb 2099 25:99:99 GMT; Path=/">>,
                        <<"form_token=form_abc123; SameSite=Strick; Secure">>
                    ]
                ],
                #{
                    <<"cache_bust">> =>
                        #{
                            <<"value">> => <<"v1.2.3">>,
                            <<"attributes">> => #{
                                <<"Expires">> => <<"Mon, 99 Feb 2099 25:99:99 GMT">>,
                                <<"Path">> => <<"/">>
                            }
                        },
                    <<"form_token">> =>
                        #{
                            <<"value">> => <<"form_abc123">>,
                            <<"attributes">> => #{ <<"SameSite">> => <<"Strick">> },
                            <<"flags">> => [<<"Secure">>]
                        }
                }
            },
        parse_token_and_reactions =>
            {
                [
                    [
                        <<"access_token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c; Path=/; HttpOnly; Secure">>,
                        <<"reaction_prefs=👍👎; Path=/; Secure">>
                    ]
                ],
                #{
                    <<"access_token">> =>
                        #{
                            <<"value">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                        },
                    <<"reaction_prefs">> =>
                        #{
                            <<"value">> => <<"👍👎">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"Secure">>]
                        }
                }
            },
        parse_error_log_and_auth_token =>
            {
                [
                    [
                        <<"error_log=\"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed\"; Path=/">>,
                        <<"auth_token=bearer_xyz789; Secure; Path=/api; Secure; HttpOnly">>
                    ]
                ],
                #{
                    <<"error_log">> =>
                        #{
                            <<"value">> => <<"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"auth_token">> =>
                        #{
                            <<"value">> => <<"bearer_xyz789">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/api">> },
                            <<"flags">> => [<<"HttpOnly">>,<<"Secure">>, <<"Secure">>]
                        }
                }
            },
        parse_csrf_and_quick_setting =>
            {
                [
                    [
                        <<"csrf_token=abc123; \"HttpOnly\"; Path=/">>,
                        <<"quick_setting=\"enabled\"">>
                    ]
                ],
                #{
                    <<"csrf_token">> =>
                        #{
                            <<"value">> => <<"abc123">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> },
                            <<"flags">> => [<<"HttpOnly">>]
                        },
                    <<"quick_setting">> => <<"enabled">>
                }
            },
        parse_admin_and_upload =>
            {
                [
                    [
                        <<"secret_key=confidential; Path=%2Fadmin">>,
                        <<"admin_flag=true; Path=/">>
                        
                    ]
                ],
                #{
                    <<"secret_key">> =>
                        #{
                            <<"value">> => <<"confidential">>,
                            <<"attributes">> => #{ <<"Path">> => <<"%2Fadmin">> }
                        },
                    <<"admin_flag">> =>
                        #{
                            <<"value">> => <<"true">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        }
                }
            },
        parse_search_and_tags =>
            {
                [
                    [
                        <<"search_history=\"query,results\"; Path=/">>,
                        <<"user_tags=\"work,personal\"; Path=/">>
                    ]
                ],
                #{
                    <<"search_history">> =>
                        #{
                            <<"value">> => <<"query,results">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        },
                    <<"user_tags">> =>
                        #{
                            <<"value">> => <<"work,personal">>,
                            <<"attributes">> => #{ <<"Path">> => <<"/">> }
                        }
                }
            },
        to_string_realworld_1 =>
            {
                [
                    #{
                        <<"cart">> =>
                            #{
                                <<"value">> => <<"110045_77895_53420">>,
                                <<"attributes">> => #{ <<"SameSite">> => <<"Strict">> }
                            },
                        <<"affiliate">> =>
                            #{
                                <<"value">> => <<"e4rt45dw">>,
                                <<"attributes">> => #{ <<"SameSite">> => <<"Lax">> }
                            }
                    }
                ],
                [
                    <<"affiliate=\"e4rt45dw\"; SameSite=Lax">>,
                    <<"cart=\"110045_77895_53420\"; SameSite=Strict">>
                ]
            },
        to_string_user_settings_and_permissions =>
            {
                [
                    #{
                        <<"user_settings">> =>
                            #{
                                <<"value">> => <<"notifications=true,privacy=strict,layout=grid">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                            },
                        <<"user_permissions">> =>
                            #{
                                <<"value">> => <<"read;write;delete">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">>, <<"SameSite">> => <<"None">> },
                                <<"flags">> => [<<"Secure">>]
                            }
                    }
                ],
                [
                    <<"user_permissions=\"read;write;delete\"; Path=/; SameSite=None; Secure">>,
                    <<"user_settings=\"notifications=true,privacy=strict,layout=grid\"; Path=/; HttpOnly; Secure">>
                ]
            },
        to_string_session_and_temp_data =>
            {
                [
                    #{
                        <<"SESSION_ID">> =>
                            #{
                                <<"value">> => <<"abc123xyz ">>,
                                <<"attributes">> => #{ <<"path">> => <<"/dashboard">>, <<"samesite">> => <<"Strict">> },
                                <<"flags">> => [<<"Secure">>]
                            },
                        <<"temp_data">> =>
                            #{
                                <<"value">> => <<"cleanup_me">>,
                                <<"attributes">> => #{ <<"Max-Age">> => <<"-1">>, <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"SESSION_ID=\"abc123xyz \"; path=/dashboard; samesite=Strict; Secure">>,
                    <<"temp_data=\"cleanup_me\"; Max-Age=-1; Path=/">>
                ]
            },
        to_string_empty_and_anonymous =>
            {
                [
                    #{
                        <<"user_preference">> =>
                            #{
                                <<"value">> => <<"">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>]
                            },
                        <<>> =>
                            #{
                                <<"value">> => <<"anonymous_session_123">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/guest">> }
                            }
                    }
                ],
                [
                    <<"=\"anonymous_session_123\"; Path=/guest">>,
                    <<"user_preference=\"\"; Path=/; HttpOnly">>
                ]
            },
        to_string_app_config_and_analytics =>
            {
                [
                    #{
                        <<"$app_config$">> =>
                            #{
                                <<"value">> => <<"theme@dark!%20mode">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"analytics_session_data_with_very_long_name_for_tracking_purposes">> =>
                            #{
                                <<"value">> => <<"comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"$app_config$=\"theme@dark!%20mode\"; Path=/">>,
                    <<"analytics_session_data_with_very_long_name_for_tracking_purposes=\"comprehensive_user_behavior_analytics_data_including_page_views_click_events_scroll_depth_time_spent_geographic_location_device_info_browser_details_and_more\"; Path=/">>
                ]
            },
        to_string_debug_and_tracking =>
            {
                [
                    #{
                        <<"debug_info">> =>
                            #{
                                <<"value">> => <<"\\tIndented\\t\\nMultiline\\n">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"tracking_id">> =>
                            #{
                                <<"value">> => <<"user_12345">>,
                                <<"attributes">> => #{
                                    <<"CustomAttr">> => <<"CustomValue">>,
                                    <<"Analytics">> => <<"Enabled">>,
                                    <<"Path">> => <<"/">>
                                },
                                <<"flags">> => [<<"HttpOnly">>]
                            }
                    }
                ],
                [
                    <<"debug_info=\"\\tIndented\\t\\nMultiline\\n\"; Path=/">>,
                    <<"tracking_id=\"user_12345\"; Analytics=Enabled; CustomAttr=CustomValue; Path=/; HttpOnly">>
                ]
            },
        to_string_cache_and_form_token =>
            {
                [
                    #{
                        <<"cache_bust">> =>
                            #{
                                <<"value">> => <<"v1.2.3">>,
                                <<"attributes">> => #{
                                    <<"Expires">> => <<"Mon, 99 Feb 2099 25:99:99 GMT">>,
                                    <<"Path">> => <<"/">>
                                }
                            },
                        <<"form_token">> =>
                            #{
                                <<"value">> => <<"form_abc123">>,
                                <<"attributes">> => #{ <<"SameSite">> => <<"Strick">> },
                                <<"flags">> => [<<"Secure">>]
                            }
                    }
                ],
                [
                    <<"cache_bust=\"v1.2.3\"; Expires=Mon, 99 Feb 2099 25:99:99 GMT; Path=/">>,
                    <<"form_token=\"form_abc123\"; SameSite=Strick; Secure">>
                ]
            },
        to_string_token_and_reactions =>
            {
                [
                    #{
                        <<"access_token">> =>
                            #{
                                <<"value">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>, <<"Secure">>]
                            },
                        <<"reaction_prefs">> =>
                            #{
                                <<"value">> => <<"👍👎">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"Secure">>]
                            }
                    }
                ],
                [
                    <<"access_token=\"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c\"; Path=/; HttpOnly; Secure">>,
                    <<"reaction_prefs=\"👍👎\"; Path=/; Secure">>
                ]
            },
        to_string_error_log_and_auth_token =>
            {
                [
                    #{
                        <<"error_log">> =>
                            #{
                                <<"value">> => <<"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"auth_token">> =>
                            #{
                                <<"value">> => <<"bearer_xyz789">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/api">> },
                                <<"flags">> => [<<"HttpOnly">>, <<"Secure">>, <<"Secure">>]
                            }
                    }
                ],
                [
                    <<"auth_token=\"bearer_xyz789\"; Path=/api; HttpOnly; Secure; Secure">>,
                    <<"error_log=\"timestamp=2024-01-15 10:30:00\\nlevel=ERROR\\tmessage=Database connection failed\"; Path=/">>
                ]
            },
        to_string_csrf_and_quick_setting =>
            {
                [
                    #{
                        <<"csrf_token">> =>
                            #{
                                <<"value">> => <<"abc123">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> },
                                <<"flags">> => [<<"HttpOnly">>]
                            },
                        <<"quick_setting">> => <<"enabled">>
                    }
                ],
                [
                    <<"csrf_token=\"abc123\"; Path=/; HttpOnly">>,
                    <<"quick_setting=\"enabled\"">>
                ]
            },
        to_string_admin_and_upload =>
            {
                [
                    #{
                        <<"secret_key">> =>
                            #{
                                <<"value">> => <<"confidential">>,
                                <<"attributes">> => #{ <<"Path">> => <<"%2Fadmin">> }
                            },
                        <<"admin_flag">> =>
                            #{
                                <<"value">> => <<"true">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"admin_flag=\"true\"; Path=/">>,
                    <<"secret_key=\"confidential\"; Path=%2Fadmin">>
                ]
            },
        to_string_search_and_tags =>
            {
                [
                    #{
                        <<"search_history">> =>
                            #{
                                <<"value">> => <<"query,results">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            },
                        <<"user_tags">> =>
                            #{
                                <<"value">> => <<"work,personal">>,
                                <<"attributes">> => #{ <<"Path">> => <<"/">> }
                            }
                    }
                ],
                [
                    <<"search_history=\"query,results\"; Path=/">>,
                    <<"user_tags=\"work,personal\"; Path=/">>
                ]
            }
    }.

from_string_basic_test() ->
    assert_set(from_string_raw_value, fun from_string/1).

from_string_attributes_test() ->
    assert_set(from_string_attributes, fun from_string/1).

from_string_flags_test() ->
    assert_set(from_string_flags, fun from_string/1).

to_string_basic_test() ->
    assert_set(to_string_raw_value, fun to_string/1).

to_string_attributes_test() ->
    assert_set(to_string_attributes, fun to_string/1).

to_string_flags_test() ->
    assert_set(to_string_flags, fun to_string/1).

parse_realworld_test() ->
    assert_set(parse_realworld_1, fun from_string/1).

parse_user_settings_and_permissions_test() ->
    assert_set(parse_user_settings_and_permissions, fun from_string/1).

parse_session_and_temp_data_test() ->
    assert_set(parse_session_and_temp_data, fun from_string/1).

parse_empty_and_anonymous_test() ->
    assert_set(parse_empty_and_anonymous, fun from_string/1).

parse_app_config_and_analytics_test() ->
    assert_set(parse_app_config_and_analytics, fun from_string/1).

parse_debug_and_tracking_test() ->
    assert_set(parse_debug_and_tracking, fun from_string/1).

parse_cache_and_form_token_test() ->
    assert_set(parse_cache_and_form_token, fun from_string/1).

parse_token_and_reactions_test() ->
    assert_set(parse_token_and_reactions, fun from_string/1).

parse_error_log_and_auth_token_test() ->
    assert_set(parse_error_log_and_auth_token, fun from_string/1).

parse_csrf_and_quick_setting_test() ->
    assert_set(parse_csrf_and_quick_setting, fun from_string/1).

parse_admin_and_upload_test() ->
    assert_set(parse_admin_and_upload, fun from_string/1).

parse_search_and_tags_test() ->
    assert_set(parse_search_and_tags, fun from_string/1).

to_string_realworld_1_test() ->
    assert_set(to_string_realworld_1, fun to_string/1).

to_string_user_settings_and_permissions_test() ->
    assert_set(to_string_user_settings_and_permissions, fun to_string/1).

to_string_session_and_temp_data_test() ->
    assert_set(to_string_session_and_temp_data, fun to_string/1).

to_string_empty_and_anonymous_test() ->
    assert_set(to_string_empty_and_anonymous, fun to_string/1).

to_string_app_config_and_analytics_test() ->
    assert_set(to_string_app_config_and_analytics, fun to_string/1).

to_string_debug_and_tracking_test() ->
    assert_set(to_string_debug_and_tracking, fun to_string/1).

to_string_cache_and_form_token_test() ->
    assert_set(to_string_cache_and_form_token, fun to_string/1).

to_string_token_and_reactions_test() ->
    assert_set(to_string_token_and_reactions, fun to_string/1).

to_string_error_log_and_auth_token_test() ->
    assert_set(to_string_error_log_and_auth_token, fun to_string/1).

to_string_csrf_and_quick_setting_test() ->
    assert_set(to_string_csrf_and_quick_setting, fun to_string/1).

to_string_admin_and_upload_test() ->
    assert_set(to_string_admin_and_upload, fun to_string/1).

to_string_search_and_tags_test() ->
    assert_set(to_string_search_and_tags, fun to_string/1).

%%% Test Helpers

%% @doc Convert a cookie message to a string.
to_string(CookieMsg) ->
    {ok, BaseMsg} = store(#{}, CookieMsg, #{}),
    {ok, Msg} = to(BaseMsg, #{ <<"format">> => <<"set-cookie">> }, #{}),
    hb_maps:get(<<"set-cookie">>, Msg, [], #{}).

%% @doc Convert a string to a cookie message.
from_string(String) ->
    {ok, BaseMsg} = from(#{ <<"set-cookie">> => String }, #{}, #{}),
    {ok, Cookie} = extract(BaseMsg, #{}, #{}),
    Cookie.

%% @doc Assert that when given the inputs in the test set, the outputs are
%% all equal to the expected value when the function is applied to them.
assert_set(TestSet, Fun) ->
    {Inputs, Expected} = maps:get(TestSet, test_data()),
    ?event(match_cookie, {starting_group_match, {inputs, {explicit, Inputs}}}),
    lists:foreach(
        fun(Input) ->
            Res = Fun(Input),
            ?event(
                match_cookie,
                {matching,
                    {expected, {explicit, Expected}, {output, {explicit, Res}}}
                }
            ),
            ?assertEqual(Expected, Res)
        end,
        Inputs
    ).