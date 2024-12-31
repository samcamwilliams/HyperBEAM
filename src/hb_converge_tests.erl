%%% @doc Tests for the core Converge resolution engine.
-module(hb_converge_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

test_opts() ->
    #{
        topic => converge_internal,
        hashpath => ignore,
        cache => none,
        spawn_worker => false
    }.

resolve_simple_test() ->
    Res = hb_converge:resolve(#{ a => 1 }, a, test_opts()),
    ?assertEqual({ok, 1}, Res).

resolve_key_twice_test() ->
    % Ensure that the same message can be resolved again.
    % This is not as trivial as it may seem, because resolutions are cached and
    % de-duplicated.
    Opts = test_opts(),
    ?assertEqual({ok, 1}, hb_converge:resolve(#{ a => 1 }, a, Opts)),
    ?assertEqual({ok, 1}, hb_converge:resolve(#{ a => 1 }, a, Opts)).

resolve_from_multiple_keys_test() ->
    ?assertEqual(
        {ok, [a]},
        hb_converge:resolve(#{ a => 1, "priv_a" => 2 }, keys, test_opts())
    ).

resolve_path_element_test() ->
    ?assertEqual(
        {ok, [test_path]},
        hb_converge:resolve(#{ path => [test_path] }, path, test_opts())
    ),
    ?assertEqual(
        {ok, [a]},
        hb_converge:resolve(#{ <<"Path">> => [a] }, <<"Path">>, test_opts())
    ).

key_to_binary_test() ->
    ?assertEqual(<<"a">>, hb_converge:key_to_binary(a)),
    ?assertEqual(<<"a">>, hb_converge:key_to_binary(<<"a">>)),
    ?assertEqual(<<"a">>, hb_converge:key_to_binary("a")).

resolve_binary_key_test() ->
    ?assertEqual(
        {ok, 1},
        hb_converge:resolve(#{ a => 1 }, <<"a">>, test_opts())
    ),
    ?assertEqual(
        {ok, 1},
        hb_converge:resolve(
            #{
                <<"Test-Header">> => 1 },
                <<"Test-Header">>,
            test_opts()
        )
    ).

%% @doc Generates a test device with three keys, each of which uses
%% progressively more of the arguments that can be passed to a device key.
generate_device_with_keys_using_args() ->
    #{
        key_using_only_state =>
            fun(State) ->
                {ok,
                    <<(maps:get(state_key, State))/binary>>
                }
            end,
        key_using_state_and_msg =>
            fun(State, Msg) ->
                {ok,
                    <<
                        (maps:get(state_key, State))/binary,
                        (maps:get(msg_key, Msg))/binary
                    >>
                }
            end,
        key_using_all =>
            fun(State, Msg, Opts) ->
                {ok,
                    <<
                        (maps:get(state_key, State))/binary,
                        (maps:get(msg_key, Msg))/binary,
                        (maps:get(opts_key, Opts))/binary
                    >>
                }
            end
    }.

%% @doc Create a simple test device that implements the default handler.
gen_default_device() ->
    #{
        info =>
            fun() ->
                #{
                    default =>
                        fun(_, _State) ->
                            {ok, <<"DEFAULT">>}
                        end
                }
            end,
        state_key =>
            fun(_) ->
                {ok, <<"STATE">>}
            end
    }.

%% @doc Create a simple test device that implements the handler key.
gen_handler_device() ->
    #{
        info =>
            fun() ->
                #{
                    handler =>
                        fun(set, M1, M2, Opts) ->
                            dev_message:set(M1, M2, Opts);
                        (_, _, _, _) ->
                            {ok, <<"HANDLER VALUE">>}
                        end
                }
            end
    }.

%% @doc Test that arguments are passed to a device key as expected.
%% Particularly, we need to ensure that the key function in the device can
%% specify any arity (1 through 3) and the call is handled correctly.
key_from_id_device_with_args_test() ->
    Msg =
        #{
            device => generate_device_with_keys_using_args(),
            state_key => <<"1">>
        },
    ?assertEqual(
        {ok, <<"1">>},
        hb_converge:resolve(
            Msg,
            #{
                path => key_using_only_state,
                msg_key => <<"2">> % Param message, which is ignored
            },
            test_opts()
        )
    ),
    ?assertEqual(
        {ok, <<"13">>},
        hb_converge:resolve(
            Msg,
            #{
                path => key_using_state_and_msg,
                msg_key => <<"3">> % Param message, with value to add
            },
            test_opts()
        )
    ),
    ?assertEqual(
        {ok, <<"1337">>},
        hb_converge:resolve(
            Msg,
            #{
                path => key_using_all,
                msg_key => <<"3">> % Param message
            },
            #{
                opts_key => <<"37">> % Opts
            }
        )
    ).

device_with_handler_function_test() ->
    Msg =
        #{
            device => gen_handler_device(),
            test_key => <<"BAD">>
        },
    ?assertEqual(
        {ok, <<"HANDLER VALUE">>},
        hb_converge:resolve(Msg, test_key, test_opts())
    ).

device_with_default_handler_function_test() ->
    Msg =
        #{
            device => gen_default_device()
        },
    ?assertEqual(
        {ok, <<"STATE">>},
        hb_converge:resolve(Msg, state_key, test_opts())
    ),
    ?assertEqual(
        {ok, <<"DEFAULT">>},
        hb_converge:resolve(Msg, any_random_key, test_opts())
    ).

basic_get_test() ->
    Msg = #{ key1 => <<"value1">>, key2 => <<"value2">> },
    ?assertEqual(<<"value1">>, hb_converge:get(key1, Msg)),
    ?assertEqual(<<"value2">>, hb_converge:get(key2, Msg)),
    ?assertEqual(<<"value2">>, hb_converge:get(<<"key2">>, Msg)),
    ?assertEqual(<<"value2">>, hb_converge:get([<<"key2">>], Msg)).

recursive_get_test() ->
    Msg = #{ key1 => <<"value1">>, key2 => #{ key3 => <<"value3">> } },
    ?assertEqual(
        {ok, <<"value1">>},
        hb_converge:resolve(Msg, #{ path => key1 }, test_opts())
    ),
    ?assertEqual(<<"value1">>, hb_converge:get(key1, Msg)),
    ?assertEqual(
        {ok, <<"value3">>},
        hb_converge:resolve(Msg, #{ path => [key2, key3] }, test_opts())
    ),
    ?assertEqual(<<"value3">>, hb_converge:get([key2, key3], Msg, test_opts())),
    ?assertEqual(<<"value3">>, hb_converge:get(<<"key2/key3">>, Msg, test_opts())).

basic_set_test() ->
    Msg = #{ key1 => <<"value1">>, key2 => <<"value2">> },
    UpdatedMsg = hb_converge:set(Msg, #{ key1 => <<"new_value1">> }),
    ?event({set_key_complete, {key, key1}, {value, <<"new_value1">>}}),
    ?assertEqual(<<"new_value1">>, hb_converge:get(key1, UpdatedMsg)),
    ?assertEqual(<<"value2">>, hb_converge:get(key2, UpdatedMsg)).

get_with_device_test() ->
    Msg =
        #{
            device => generate_device_with_keys_using_args(),
            state_key => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_converge:get(state_key, Msg)),
    ?assertEqual(<<"STATE">>, hb_converge:get(key_using_only_state, Msg)).

get_as_with_device_test() ->
    Msg =
        #{
            device => gen_handler_device(),
            test_key => <<"ACTUAL VALUE">>
        },
    ?assertEqual(
        <<"HANDLER VALUE">>,
        hb_converge:get(test_key, Msg)
    ),
    ?assertEqual(
        <<"ACTUAL VALUE">>,
        hb_converge:get(test_key, {as, dev_message, Msg})
    ).

set_with_device_test() ->
    Msg =
        #{
            device =>
                #{
                    set =>
                        fun(State, _Msg) ->
                            {ok,
                                State#{
                                    set_count =>
                                        1 + maps:get(set_count, State, 0)
                                }
                            }
                        end
                },
            state_key => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_converge:get(state_key, Msg)),
    SetOnce = hb_converge:set(Msg, #{ state_key => <<"SET_ONCE">> }),
    ?assertEqual(1, hb_converge:get(set_count, SetOnce)),
    SetTwice = hb_converge:set(SetOnce, #{ state_key => <<"SET_TWICE">> }),
    ?assertEqual(2, hb_converge:get(set_count, SetTwice)),
    ?assertEqual(<<"STATE">>, hb_converge:get(state_key, SetTwice)).

deep_set_test() ->
    % First validate second layer changes are handled correctly.
    Msg0 = #{ a => #{ b => 1 } },
    ?assertMatch(#{ a := #{ b := 2 } },
        hb_converge:set(Msg0, [a, b], 2, test_opts())),
    % Now validate deeper layer changes are handled correctly.
    Msg = #{ a => #{ b => #{ c => 1 } } },
    ?assertMatch(#{ a := #{ b := #{ c := 2 } } },
        hb_converge:set(Msg, [a, b, c], 2, test_opts())).

deep_set_new_messages_test() ->
    % Test that new messages are created when the path does not exist.
    Msg0 = #{ a => #{ b => #{ c => 1 } } },
    Msg1 = hb_converge:set(Msg0, <<"d/e">>, 3, test_opts()),
    Msg2 = hb_converge:set(Msg1, <<"d/f">>, 4, test_opts()),
    ?assert(
        hb_message:match(
            Msg2,
            #{ a => #{ b => #{ c => 1 } }, d => #{ e => 3, f => 4 } }
        )
    ),
    Msg3 = hb_converge:set(
        Msg2,
        #{ 
            <<"z/a">> => 0,
            <<"z/b">> => 1,
            <<"z/y/x">> => 2
         },
         test_opts()
    ),
    ?assert(
        hb_message:match(
            Msg3,
            #{
                a => #{ b => #{ c => 1 } },
                d => #{ e => 3, f => 4 },
                z => #{ a => 0, b => 1, y => #{ x => 2 } }
            }
        )
    ).

deep_set_with_device_test() ->
    Device = #{
        set =>
            fun(Msg1, Msg2) ->
                % A device where the set function modifies the key
                % and adds a modified flag.
                {Key, Val} =
                    hd(maps:to_list(maps:without([path, priv], Msg2))),
                {ok, Msg1#{ Key => Val, modified => true }}
            end
    },
    % A message with an interspersed custom device: A and C have it,
    % B does not. A and C will have the modified flag set to true.
    Msg = #{
        device => Device,
        a =>
            #{
                b =>
                    #{
                        device => Device,
                        c => 1,
                        modified => false
                    },
                modified => false
            },
        modified => false
    },
    Outer = hb_converge:deep_set(Msg, [a, b, c], 2, test_opts()),
    A = hb_converge:get(a, Outer, test_opts()),
    B = hb_converge:get(b, A, test_opts()),
    C = hb_converge:get(c, B, test_opts()),
    ?assertEqual(2, C),
    ?assertEqual(true, hb_converge:get(modified, Outer)),
    ?assertEqual(false, hb_converge:get(modified, A)),
    ?assertEqual(true, hb_converge:get(modified, B)).

device_exports_test() ->
	Msg = #{ device => dev_message },
	?assert(hb_converge:is_exported(Msg, dev_message, info, test_opts())),
	?assert(hb_converge:is_exported(Msg, dev_message, set, test_opts())),
	?assert(
        hb_converge:is_exported(
            Msg,
            dev_message,
            not_explicitly_exported,
            test_opts()
        )
    ),
	Dev = #{
		info => fun() -> #{ exports => [set] } end,
		set => fun(_, _) -> {ok, <<"SET">>} end
	},
	Msg2 = #{ device => Dev },
	?assert(hb_converge:is_exported(Msg2, Dev, info, test_opts())),
	?assert(hb_converge:is_exported(Msg2, Dev, set, test_opts())),
	?assert(not hb_converge:is_exported(Msg2, Dev, not_exported, test_opts())),
    Dev2 = #{
        info =>
            fun() ->
                #{
                    exports => [test1, <<"Test2">>],
                    handler =>
                        fun() ->
                            {ok, <<"Handler-Value">>}
                        end
                }
            end
    },
    Msg3 = #{ device => Dev2, <<"Test1">> => <<"BAD1">>, test3 => <<"GOOD3">> },
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(test1, Msg3)),
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(test2, Msg3)),
    ?assertEqual(<<"GOOD3">>, hb_converge:get(test3, Msg3)),
    ?assertEqual(<<"GOOD4">>,
        hb_converge:get(
            <<"Test4">>,
            hb_converge:set(Msg3, <<"Test4">>, <<"GOOD4">>, test_opts())
        )
    ),
    ?assertEqual(not_found, hb_converge:get(test5, Msg3)).

device_excludes_test() ->
    % Create a device that returns an identifiable message for any key, but also
    % sets excludes to [set], such that the message can be modified using the 
    % default handler.
    Dev = #{
        info =>
            fun() ->
                #{
                    excludes => [set],
                    handler => fun() -> {ok, <<"Handler-Value">>} end
                }
            end
    },
    Msg = #{ device => Dev, <<"Test-Key">> => <<"Test-Value">> },
    ?assert(hb_converge:is_exported(Msg, Dev, <<"Test-Key2">>, test_opts())),
    ?assert(not hb_converge:is_exported(Msg, Dev, set, test_opts())),
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(<<"Test-Key2">>, Msg)),
    ?assertMatch(#{ <<"Test-Key2">> := 2 },
        hb_converge:set(Msg, <<"Test-Key2">>, 2, test_opts())).

denormalized_device_key_test() ->
	Msg = #{ <<"Device">> => dev_test },
	?assertEqual(dev_test, hb_converge:get(device, Msg, test_opts())),
	?assertEqual(dev_test, hb_converge:get(<<"Device">>, Msg, test_opts())),
	?assertEqual({module, dev_test},
		erlang:fun_info(
            element(3, hb_converge:message_to_fun(Msg, test_func, test_opts())),
            module
        )
    ).

list_transform_test() ->
    Msg = [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>],
    ?assertEqual(<<"A">>, hb_converge:get(1, Msg)),
    ?assertEqual(<<"B">>, hb_converge:get(2, Msg)),
    ?assertEqual(<<"C">>, hb_converge:get(3, Msg)),
    ?assertEqual(<<"D">>, hb_converge:get(4, Msg)),
    ?assertEqual(<<"E">>, hb_converge:get(5, Msg)).

cache_execution_test() ->
    Msg1 = #{ device => <<"Message/1.0">>, <<"A">> => #{ <<"B">> => <<"C">> } },
    Msg2 = #{ path => <<"A/B">> },
    {ok, Res} = hb_converge:resolve(Msg1, Msg2, #{ cache => always }),
    ?assertEqual(<<"B">>, Res),
    {ok, Res2} = hb_converge:resolve(Msg1, Msg2, #{ cache => always }),
    ?assertEqual(<<"B">>, Res2).

