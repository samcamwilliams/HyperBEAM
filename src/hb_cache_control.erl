%%% @doc Cache control logic for the Converge resolver. It derives cache settings
%%% from request, response, execution-local node Opts, as well as the global
%%% node Opts. It applies these settings when asked to maybe store/lookup in 
%%% response to a request.
-module(hb_cache_control).
-export([maybe_store/4, maybe_lookup/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Write a resulting M3 message to the cache if requested. The precidence
%% order of cache control sources is as follows:
%% 1. The `Opts` map (letting the node operator have the final say).
%% 2. The `Msg3` results message (granted by Msg1's device).
%% 3. The `Msg2` message (the user's request).
%% Msg1 is not used, such that it can specify cache control information about 
%% itself, without affecting its outputs.
maybe_store(Msg1, Msg2, Msg3, Opts) ->
    case derive_cache_settings([Msg3, Msg2], Opts) of
        #{ store := true } ->
            ?event({caching_result, {msg2, Msg2}, {msg3, Msg3}}),
            dispatch_cache_write(Msg1, Msg2, Msg3, Opts);
        _ -> not_caching
    end.

%% @doc Handles cache lookup, modulated by the caching options requested by
%% the user. Honors the following `Opts` cache keys: 
%%      `only_if_cached`: If set and we do not find a result in the cache,
%%                        return an error with a `Cache-Status` of `miss` and
%%                        a 504 `Status`.
%%      `no_cache`:       If set, the cached values are never used. Returns
%%                        `continue` to the caller.
maybe_lookup(Msg1, Msg2, Opts) ->
    case derive_cache_settings([Msg1, Msg2], Opts) of
        #{ lookup := false } -> {continue, Msg1, Msg2};
        Settings = #{ lookup := true } ->
            case hb_cache:read_output(Msg1, Msg2, Opts) of
                {ok, Msg3} ->
                    ?event(
                        {compute_cache_hit,
                            {msg1, Msg1},
                            {msg2, Msg2},
                            {msg3, Msg3}
                        }
                    ),
                    {ok, Msg3};
                not_found ->
                    case Settings of
                            #{ only_if_cached := true } ->
                                only_if_cached_not_found_error(Msg1, Msg2, Opts);
                            _ ->
                                case ?IS_ID(Msg1) of
                                    false -> {continue, Msg1, Msg2};
                                    true ->
                                        case hb_cache:read(Msg1, Opts) of
                                            {ok, FullMsg1} ->
                                                ?event(
                                                    {message_cache_hit,
                                                        {msg1, Msg1},
                                                        {msg2, Msg2},
                                                        {msg3, FullMsg1}
                                                    }
                                                ),
                                                {continue, FullMsg1, Msg2};
                                            not_found ->
                                                necessary_messages_not_found_error(
                                                    Msg1,
                                                    Msg2,
                                                    Opts
                                                )
                                        end
                                end
                        end
            end
    end.

%% @doc Generate a message to return when `only_if_cached` was specified, and
%% we don't have a cached result.
only_if_cached_not_found_error(Msg1, Msg2, Opts) ->
    ?event(
        cache,
        {only_if_cached_execution_failed, {msg1, Msg1}, {msg2, Msg2}},
        Opts
    ),
    {error,
        #{
            <<"Status">> => <<"Gateway Timeout">>,
            <<"Status-Code">> => 504,
            <<"Cache-Status">> => <<"miss">>,
            <<"body">> =>
                <<"Computed result not available in cache.">>
        }
    }.

%% @doc Generate a message to return when the necessary messages to execute a 
%% cache lookup are not found in the cache.
necessary_messages_not_found_error(Msg1, Msg2, Opts) ->
    ?event(
        cache,
        {necessary_messages_not_found, {msg1, Msg1}, {msg2, Msg2}},
        Opts
    ),
    {error,
        #{
            <<"Status">> => <<"Not Found">>,
            <<"Status-Code">> => 404,
            <<"body">> =>
                <<"Necessary messages not found in cache.">>
        }
    }.

%% @doc Derive cache settings from a series of option sources and the opts,
%% honoring precidence order. The Opts is used as the first source. Returns a
%% map with `store' and `lookup' keys, each of which is a boolean.
%% 
%% For example, if the last source has a `no_store', the first expresses no
%% preference, but the Opts has `cache_control => [always]`, then the result 
%% will contain a `store => true' entry.
derive_cache_settings(SourceList, Opts) ->
    lists:foldr(
        fun(Source, Acc) ->
            maybe_set(Acc, cache_source_to_cache_settings(Source))
        end,
        #{ store => true, lookup => true },
        [{opts, Opts}|lists:filter(fun erlang:is_map/1, SourceList)]
    ).

%% @doc Takes a key and two maps, returning the first map with the key set to
%% the value of the second map _if_ the value is not undefined.
maybe_set(Map1, Map2) ->
    lists:foldl(
        fun(Key, AccMap) ->
            case maps:get(Key, Map2) of
                undefined -> AccMap;
                Value -> maps:put(Key, Value, AccMap)
            end
        end,
        Map1,
        maps:keys(Map2)
    ).

%% @doc Convert a cache source to a cache setting. The setting _must_ always be
%% directly in the source, not a Converge-derivable value. The 
%% `to_cache_control_map` function is used as the source of settings in all
%% cases, except where an `Opts` specifies that hashpaths should not be updated,
%% which leads to the result not being cached (as it may be stored with an 
%% incorrect hashpath).
cache_source_to_cache_settings({opts, Opts}) ->
    CCMap = specifiers_to_cache_settings(hb_opts:get(cache_control, [], Opts)),
    case hb_opts:get(hashpath, update, Opts) of
        ignore -> CCMap#{ store => false };
        _ -> CCMap
    end;
cache_source_to_cache_settings(Msg) ->
    case dev_message:get(<<"Cache-Control">>, Msg) of
        {ok, CC} -> specifiers_to_cache_settings(CC);
        {error, not_found} -> #{}
    end.

%% @doc Convert a cache control list as received via HTTP headers into a 
%% normalized map of simply whether we should store and/or lookup the result.
specifiers_to_cache_settings(CCSpecifier) when not is_list(CCSpecifier) ->
    specifiers_to_cache_settings([CCSpecifier]);
specifiers_to_cache_settings(CCList) ->
    #{
        store =>
            case lists:member(<<"always">>, CCList) of
                true -> true;
                false ->
                    case lists:member(<<"no-store">>, CCList) of
                        true -> false;
                        false -> undefined
                    end
            end,
        lookup =>
            case lists:member(<<"always">>, CCList) of
                true -> true;
                false ->
                    case lists:member(<<"no-cache">>, CCList) of
                        true -> false;
                        false -> undefined
                    end
            end,
        only_if_cached =>
            case lists:member(<<"only-if-cached">>, CCList) of
                true -> true;
                false -> undefined
            end
    }.

%% @doc Dispatch the cache write to a worker process if requested.
%% Invoke the appropriate cache write function based on the type of the message.
dispatch_cache_write(Msg1, Msg2, Msg3, Opts) ->
    Dispatch =
        fun() ->
            case is_binary(Msg3) of
                true ->
                    hb_cache:write_binary(
                        hb_path:hashpath(Msg1, Msg2, Opts),
                        Msg3,
                        Opts
                    );
                false -> hb_cache:write(Msg3, Opts)
            end
        end,
    case hb_opts:get(async_cache, false, Opts) of
        true -> spawn(Dispatch);
        false -> Dispatch()
    end.

%%% Tests

%% Helpers to create a message with Cache-Control header
msg_with_cc(CC) -> #{ <<"Cache-Control">> => CC }.
opts_with_cc(CC) -> #{ cache_control => CC }.

%% Test precedence order (Opts > Msg3 > Msg2)
opts_override_message_settings_test() ->
    Msg2 = msg_with_cc([<<"no-store">>]),
    Msg3 = msg_with_cc([<<"no-cache">>]),
    Opts = opts_with_cc([<<"always">>]),
    Result = derive_cache_settings([Msg3, Msg2], Opts),
    ?assertEqual(#{store => true, lookup => true}, Result).

msg_precidence_overrides_test() ->
    Msg2 = msg_with_cc([<<"always">>]),
    Msg3 = msg_with_cc([<<"no-store">>]),  % No restrictions
    Result = derive_cache_settings([Msg3, Msg2], opts_with_cc([])),
    ?assertEqual(#{store => false, lookup => true}, Result).

%% Test specific directives
no_store_directive_test() ->
    Msg = msg_with_cc([<<"no-store">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(#{store => false, lookup => true}, Result).

no_cache_directive_test() ->
    Msg = msg_with_cc([<<"no-cache">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(#{store => true, lookup => false}, Result).

only_if_cached_directive_test() ->
    Msg = msg_with_cc([<<"only-if-cached">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(
        #{store => true, lookup => true, only_if_cached => true},
        Result
    ).

%% Test hashpath settings
hashpath_ignore_prevents_storage_test() ->
    Opts = (opts_with_cc([]))#{hashpath => ignore},
    Result = derive_cache_settings([], Opts),
    ?assertEqual(#{store => false, lookup => true}, Result).

%% Test multiple directives
multiple_directives_test() ->
    Msg = msg_with_cc([<<"no-store">>, <<"no-cache">>, <<"only-if-cached">>]),
    Result = derive_cache_settings([Msg], opts_with_cc([])),
    ?assertEqual(
        #{store => false, lookup => false, only_if_cached => true},
        Result
    ).

%% Test empty/missing cases
empty_message_list_test() ->
    Result = derive_cache_settings([], opts_with_cc([])),
    ?assertEqual(#{store => true, lookup => true}, Result).

message_without_cache_control_test() ->
    Result = derive_cache_settings([#{}], opts_with_cc([])),
    ?assertEqual(#{store => true, lookup => true}, Result).

%% Test the cache_source_to_cache_setting function directly
opts_source_cache_control_test() ->
    Result =
        cache_source_to_cache_settings(
            {opts, opts_with_cc([<<"no-store">>])}
        ),
    ?assertEqual(#{
        store => false,
        lookup => undefined,
        only_if_cached => undefined
    }, Result).

message_source_cache_control_test() ->
    Msg = msg_with_cc([<<"no-cache">>]),
    Result = cache_source_to_cache_settings(Msg),
    ?assertEqual(#{
        store => undefined,
        lookup => false,
        only_if_cached => undefined
    }, Result).

%%% Basic cached Converge resolution tests

cache_message_result_test() ->
    CachedMsg = #{ <<"Purpose">> => <<"Test-Message">> },
    Msg1 = #{ <<"Test-Key">> => CachedMsg },
    Msg2 = <<"Test-Key">>,
    {ok, Res} =
        hb_converge:resolve(
            Msg1,
            Msg2,
            #{
                cache_control => [<<"always">>]
            }
        ),
    ?event(debug, {res1, Res}),
    {ok, Res2} = hb_converge:resolve(Msg1, Msg2, #{ cache_control => [<<"only-if-cached">>] }),
    ?event(debug, {res2, Res2}),
    ?assertMatch(#{ <<"Purpose">> := <<"Test-Message">> }, Res2).

cache_binary_result_test() ->
    CachedMsg = <<"Test-Message">>,
    Msg1 = #{ <<"Test-Key">> => CachedMsg },
    Msg2 = <<"Test-Key">>,
    {ok, Res} = hb_converge:resolve(Msg1, Msg2, #{ cache_control => [<<"always">>] }),
    ?assertEqual(CachedMsg, Res),
    {ok, Res2} = hb_converge:resolve(Msg1, Msg2, #{ cache_control => [<<"only-if-cached">>] }),
    ?assertEqual(CachedMsg, Res2).