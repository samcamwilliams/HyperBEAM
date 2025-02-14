%%% @doc Tests for the core Converge resolution engine.
%%% Uses a series of different `Opts' values to test the resolution engine's 
%%% execution under different circumstances.
-module(hb_converge_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% Easy hook to make a test executable via the command line:
%% `rebar3 eunit --test hb_converge_test_vectors:run_test'
%% Comment/uncomment out as necessary.
%% run_test() ->
%%     hb_test_utils:run(normal, as, test_suite(), test_opts()).

%% @doc Run each test in the file with each set of options. Start and reset
%% the store for each test.
run_all_test_() ->
    hb_test_utils:suite_with_opts(test_suite(), test_opts()).

test_suite() ->
    [
        {resolve_simple, "resolve simple",
            fun resolve_simple_test/1},
        {resolve_id, "resolve id",
            fun resolve_id_test/1},
        {as, "as",
            fun as_test/1},
        {resolve_key_twice, "resolve key twice",
            fun resolve_key_twice_test/1},
        {resolve_from_multiple_keys, "resolve from multiple keys",
            fun resolve_from_multiple_keys_test/1},
        {resolve_path_element, "resolve path element",
            fun resolve_path_element_test/1},
        {resolve_binary_key, "resolve binary key",
            fun resolve_binary_key_test/1},
        {key_to_binary, "key to binary",
            fun key_to_binary_test/1},
        {key_from_id_device_with_args, "key from id device with args",
            fun key_from_id_device_with_args_test/1},
        {device_with_handler_function, "device with handler function",
            fun device_with_handler_function_test/1},
        {device_with_default_handler_function,
            "device with default handler function",
            fun device_with_default_handler_function_test/1},
        {basic_get, "basic get",
            fun basic_get_test/1},
        {recursive_get, "recursive get",
            fun recursive_get_test/1},
        {deep_recursive_get, "deep recursive get",
            fun deep_recursive_get_test/1},
        {basic_set, "basic set",
            fun basic_set_test/1},
        {get_with_device, "get with device",
            fun get_with_device_test/1},
        {get_as_with_device, "get as with device",
            fun get_as_with_device_test/1},
        {set_with_device, "set with device",
            fun set_with_device_test/1},
        {deep_set, "deep set",
            fun deep_set_test/1},
        {deep_set_with_device, "deep set with device",
            fun deep_set_with_device_test/1},
        {device_exports, "device exports",
            fun device_exports_test/1},
        {device_excludes, "device excludes",
            fun device_excludes_test/1},
        {denormalized_device_key, "denormalized device key",
            fun denormalized_device_key_test/1},
        {list_transform, "list transform",
            fun list_transform_test/1}
    ].

test_opts() ->
    [
        #{
            name => no_cache,
            desc => "No cache read or write",
            opts => #{
                hashpath => ignore,
                cache_control => [<<"no-cache">>, <<"no-store">>],
                spawn_worker => false,
                store => {hb_store_fs, #{ prefix => "TEST-cache-fs" }}
            },
            skip => []
        },
        #{
            name => only_store,
            desc => "Store, don't read",
            opts => #{
                hashpath => update,
                cache_control => [<<"no-cache">>],
                spawn_worker => false,
                store => {hb_store_fs, #{ prefix => "TEST-cache-fs" }}
            },
            skip => [
                denormalized_device_key,
                deep_set_with_device
            ],
            reset => false
        },
        #{
            name => only_if_cached,
            desc => "Only read, don't exec",
            opts => #{
                hashpath => ignore,
                cache_control => [<<"only-if-cached">>],
                spawn_worker => false,
                store => {hb_store_fs, #{ prefix => "TEST-cache-fs" }}
            },
            skip => [
                % Exclude tests that return a list on its own for now, as raw 
                % lists cannot be cached yet.
                set_new_messages,
                resolve_from_multiple_keys,
                resolve_path_element,
                denormalized_device_key,
                % Skip test with locally defined device
                deep_set_with_device,
                as
                % Skip tests that call hb_converge utils (which have their own 
                % cache settings).
            ]
        },
        #{
            name => normal,
            desc => "Default opts",
            opts => #{},
            skip => []
        }
    ].

%%% Test vector suite

resolve_simple_test(Opts) ->
    Res = hb_converge:resolve(#{ <<"a">> => <<"RESULT">> }, <<"a">>, Opts),
    ?assertEqual({ok, <<"RESULT">>}, Res).

resolve_id_test(Opts) ->
    ?assertMatch(
        ID when byte_size(ID) == 43,
        hb_converge:get(id, #{ test_key => <<"1">> }, Opts)
    ).

resolve_key_twice_test(Opts) ->
    % Ensure that the same message can be resolved again.
    % This is not as trivial as it may seem, because resolutions are cached and
    % de-duplicated.
    ?assertEqual({ok, <<"1">>}, hb_converge:resolve(#{ <<"a">> => <<"1">> }, <<"a">>, Opts)),
    ?assertEqual({ok, <<"1">>}, hb_converge:resolve(#{ <<"a">> => <<"1">> }, <<"a">>, Opts)).

resolve_from_multiple_keys_test(Opts) ->
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_converge:resolve(#{ <<"a">> => <<"1">>, <<"priv_a">> => <<"2">> }, <<"keys">>, Opts)
    ).

resolve_path_element_test(Opts) ->
    ?assertEqual(
        {ok, [<<"test_path">>]},
        hb_converge:resolve(#{ <<"path">> => [<<"test_path">>] }, <<"path">>, Opts)
    ),
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_converge:resolve(#{ <<"Path">> => [<<"a">>] }, <<"Path">>, Opts)
    ).

key_to_binary_test(Opts) ->
    ?assertEqual(<<"a">>, hb_converge:normalize_key(a, Opts)),
    ?assertEqual(<<"a">>, hb_converge:normalize_key(<<"a">>, Opts)),
    ?assertEqual(<<"a">>, hb_converge:normalize_key("a", Opts)).

resolve_binary_key_test(Opts) ->
    ?assertEqual(
        {ok, <<"RESULT">>},
        hb_converge:resolve(#{ a => <<"RESULT">> }, <<"a">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"1">>},
        hb_converge:resolve(
            #{
                <<"Test-Header">> => <<"1">> },
                <<"Test-Header">>,
            Opts
        )
    ).

%% @doc Generates a test device with three keys, each of which uses
%% progressively more of the arguments that can be passed to a device key.
generate_device_with_keys_using_args() ->
    #{
        key_using_only_state =>
            fun(State) ->
                {ok,
                    <<(maps:get(<<"state_key">>, State))/binary>>
                }
            end,
        key_using_state_and_msg =>
            fun(State, Msg) ->
                {ok,
                    <<
                        (maps:get(<<"state_key">>, State))/binary,
                        (maps:get(<<"msg_key">>, Msg))/binary
                    >>
                }
            end,
        key_using_all =>
            fun(State, Msg, Opts) ->
                {ok,
                    <<
                        (maps:get(<<"state_key">>, State))/binary,
                        (maps:get(<<"msg_key">>, Msg))/binary,
                        (maps:get(<<"opts_key">>, Opts))/binary
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
        <<"state_key">> =>
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
                        fun(<<"set">>, M1, M2, Opts) ->
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
key_from_id_device_with_args_test(Opts) ->
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
                <<"path">> => <<"key_using_only_state">>,
                <<"msg_key">> => <<"2">> % Param message, which is ignored
            },
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"13">>},
        hb_converge:resolve(
            Msg,
            #{
                <<"path">> => <<"key_using_state_and_msg">>,
                <<"msg_key">> => <<"3">> % Param message, with value to add
            },
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"1337">>},
        hb_converge:resolve(
            Msg,
            #{
                <<"path">> => <<"key_using_all">>,
                <<"msg_key">> => <<"3">> % Param message
            },
            Opts#{
                <<"opts_key">> => <<"37">>,
                <<"cache_control">> => [<<"no-cache">>, <<"no-store">>]
            }
        )
    ).

device_with_handler_function_test(Opts) ->
    Msg =
        #{
            device => gen_handler_device(),
            test_key => <<"BAD">>
        },
    ?assertEqual(
        {ok, <<"HANDLER VALUE">>},
        hb_converge:resolve(Msg, <<"test_key">>, Opts)
    ).

device_with_default_handler_function_test(Opts) ->
    Msg =
        #{
            device => gen_default_device()
        },
    ?assertEqual(
        {ok, <<"STATE">>},
        hb_converge:resolve(Msg, <<"state_key">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"DEFAULT">>},
        hb_converge:resolve(Msg, <<"any_random_key">>, Opts)
    ).

basic_get_test(Opts) ->
    Msg = #{ <<"key1">> => <<"value1">>, <<"key2">> => <<"value2">> },
    ?assertEqual(<<"value1">>, hb_converge:get(<<"key1">>, Msg, Opts)),
    ?assertEqual(<<"value2">>, hb_converge:get(<<"key2">>, Msg, Opts)),
    ?assertEqual(<<"value2">>, hb_converge:get(<<"key2">>, Msg, Opts)),
    ?assertEqual(<<"value2">>, hb_converge:get([<<"key2">>], Msg, Opts)).

recursive_get_test(Opts) ->
    Msg = #{
        <<"key1">> => <<"value1">>,
        <<"key2">> => #{
            <<"key3">> => <<"value3">>,
            <<"key4">> => #{
                <<"key5">> => <<"value5">>,
                <<"key6">> => #{
                    <<"key7">> => <<"value7">>
                }
            }
        }
    },
    ?assertEqual(
        {ok, <<"value1">>},
        hb_converge:resolve(Msg, #{ <<"path">> => <<"key1">> }, Opts)
    ),
    ?assertEqual(<<"value1">>, hb_converge:get(<<"key1">>, Msg, Opts)),
    ?assertEqual(
        {ok, <<"value3">>},
        hb_converge:resolve(Msg, #{ <<"path">> => [<<"key2">>, <<"key3">>] }, Opts)
    ),
    ?assertEqual(<<"value3">>, hb_converge:get([<<"key2">>, <<"key3">>], Msg, Opts)),
    ?assertEqual(<<"value3">>, hb_converge:get(<<"key2/key3">>, Msg, Opts)).

deep_recursive_get_test(Opts) ->
    Msg = #{
        <<"key1">> => <<"value1">>,
        <<"key2">> => #{
            <<"key3">> => <<"value3">>,
            <<"key4">> => #{
                <<"key5">> => <<"value5">>,
                <<"key6">> => #{
                    <<"key7">> => <<"value7">>
                }
            }
        }
    },
    ?assertEqual(<<"value7">>, hb_converge:get(<<"key2/key4/key6/key7">>, Msg, Opts)).

basic_set_test(Opts) ->
    Msg = #{ <<"key1">> => <<"value1">>, <<"key2">> => <<"value2">> },
    UpdatedMsg = hb_converge:set(Msg, #{ <<"key1">> => <<"new_value1">> }, Opts),
    ?event({set_key_complete, {key, <<"key1">>}, {value, <<"new_value1">>}}),
    ?assertEqual(<<"new_value1">>, hb_converge:get(<<"key1">>, UpdatedMsg, Opts)),
    ?assertEqual(<<"value2">>, hb_converge:get(<<"key2">>, UpdatedMsg, Opts)).

get_with_device_test(Opts) ->
    Msg =
        #{
            <<"device">> => generate_device_with_keys_using_args(),
            <<"state_key">> => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_converge:get(<<"state_key">>, Msg, Opts)),
    ?assertEqual(<<"STATE">>, hb_converge:get(<<"key_using_only_state">>, Msg, Opts)).

get_as_with_device_test(Opts) ->
    Msg =
        #{
            <<"device">> => gen_handler_device(),
            <<"test_key">> => <<"ACTUAL VALUE">>
        },
    ?assertEqual(
        <<"HANDLER VALUE">>,
        hb_converge:get(test_key, Msg, Opts)
    ),
    ?assertEqual(
        <<"ACTUAL VALUE">>,
        hb_converge:get(test_key, {as, dev_message, Msg}, Opts)
    ).

set_with_device_test(Opts) ->
    Msg =
        #{
            <<"device">> =>
                #{
                    <<"set">> =>
                        fun(State, _Msg) ->
                            Acc = maps:get(<<"set_count">>, State, <<"">>),
                            {ok,
                                State#{
                                    <<"set_count">> => << Acc/binary, "." >>
                                }
                            }
                        end
                },
            <<"state_key">> => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_converge:get(<<"state_key">>, Msg, Opts)),
    SetOnce = hb_converge:set(Msg, #{ <<"state_key">> => <<"SET_ONCE">> }, Opts),
    ?assertEqual(<<".">>, hb_converge:get(<<"set_count">>, SetOnce, Opts)),
    SetTwice = hb_converge:set(SetOnce, #{ <<"state_key">> => <<"SET_TWICE">> }, Opts),
    ?assertEqual(<<"..">>, hb_converge:get(<<"set_count">>, SetTwice, Opts)),
    ?assertEqual(<<"STATE">>, hb_converge:get(<<"state_key">>, SetTwice, Opts)).

deep_set_test(Opts) ->
    % First validate second layer changes are handled correctly.
    Msg0 = #{ <<"a">> => #{ <<"b">> => <<"RESULT">> } },
    ?assertMatch(#{ <<"a">> := #{ <<"b">> := <<"RESULT2">> } },
        hb_converge:set(Msg0, <<"a/b">>, <<"RESULT2">>, Opts)),
    ?assertMatch(#{ <<"a">> := #{ <<"b">> := <<"RESULT2">> } },
        hb_converge:set(Msg0, [<<"a">>, <<"b">>], <<"RESULT2">>, Opts)),
    % Now validate deeper layer changes are handled correctly.
    Msg = #{ <<"a">> => #{ <<"b">> => #{ <<"c">> => <<"1">> } } },
    ?assertMatch(#{ <<"a">> := #{ <<"b">> := #{ <<"c">> := <<"2">> } } },
        hb_converge:set(Msg, [<<"a">>, <<"b">>, <<"c">>], <<"2">>, Opts)).

deep_set_new_messages_test() ->
    Opts = maps:get(opts, hd(test_opts())),
    % Test that new messages are created when the path does not exist.
    Msg0 = #{ <<"a">> => #{ <<"b">> => #{ <<"c">> => <<"1">> } } },
    Msg1 = hb_converge:set(Msg0, <<"d/e">>, <<"3">>, Opts),
    Msg2 = hb_converge:set(Msg1, <<"d/f">>, <<"4">>, Opts),
    ?assert(
        hb_message:match(
            Msg2,
            #{ 
                <<"a">> =>
                    #{
                        <<"b">> =>
                            #{ <<"c">> => <<"1">> }
                    },
                <<"d">> =>
                    #{
                        <<"e">> => <<"3">>,
                        <<"f">> => <<"4">> }
            }
        )
    ),
    Msg3 = hb_converge:set(
        Msg2,
        #{ 
            <<"z/a">> => <<"0">>,
            <<"z/b">> => <<"1">>,
            <<"z/y/x">> => <<"2">>
         },
         Opts
    ),
    ?assert(
        hb_message:match(
            Msg3,
            #{
                <<"a">> => #{ <<"b">> => #{ <<"c">> => <<"1">> } },
                <<"d">> => #{ <<"e">> => <<"3">>, <<"f">> => <<"4">> },
                <<"z">> =>
                    #{
                        <<"a">> => <<"0">>,
                        <<"b">> => <<"1">>,
                        <<"y">> => #{ <<"x">> => <<"2">> }
                    }
            }
        )
    ).

deep_set_with_device_test(Opts) ->
    Device = #{
        set =>
            fun(Msg1, Msg2) ->
                % A device where the set function modifies the key
                % and adds a modified flag.
                {Key, Val} =
                    hd(maps:to_list(maps:without([<<"path">>, <<"priv">>], Msg2))),
                {ok, Msg1#{ Key => Val, <<"modified">> => true }}
            end
    },
    % A message with an interspersed custom device: A and C have it,
    % B does not. A and C will have the modified flag set to true.
    Msg = #{
        <<"device">> => Device,
        <<"a">> =>
            #{
                <<"b">> =>
                    #{
                        <<"device">> => Device,
                        <<"c">> => <<"1">>,
                        <<"modified">> => false
                    },
                <<"modified">> => false
            },
        <<"modified">> => false
    },
    Outer = hb_converge:set(Msg, <<"a/b/c">>, <<"2">>, Opts),
    A = hb_converge:get(<<"a">>, Outer, Opts),
    B = hb_converge:get(<<"b">>, A, Opts),
    C = hb_converge:get(<<"c">>, B, Opts),
    ?assertEqual(<<"2">>, C),
    ?assertEqual(true, hb_converge:get(<<"modified">>, Outer)),
    ?assertEqual(false, hb_converge:get(<<"modified">>, A)),
    ?assertEqual(true, hb_converge:get(<<"modified">>, B)).

device_exports_test(Opts) ->
	Msg = #{ <<"device">> => dev_message },
	?assert(hb_converge:is_exported(Msg, dev_message, info, Opts)),
	?assert(hb_converge:is_exported(Msg, dev_message, set, Opts)),
	?assert(
        hb_converge:is_exported(
            Msg,
            dev_message,
            not_explicitly_exported,
            Opts
        )
    ),
	Dev = #{
		info => fun() -> #{ exports => [set] } end,
		set => fun(_, _) -> {ok, <<"SET">>} end
	},
	Msg2 = #{ <<"device">> => Dev },
	?assert(hb_converge:is_exported(Msg2, Dev, info, Opts)),
	?assert(hb_converge:is_exported(Msg2, Dev, set, Opts)),
	?assert(not hb_converge:is_exported(Msg2, Dev, not_exported, Opts)),
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
    Msg3 = #{ <<"device">> => Dev2, <<"Test1">> => <<"BAD1">>, <<"test3">> => <<"GOOD3">> },
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(<<"test1">>, Msg3, Opts)),
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(<<"test2">>, Msg3, Opts)),
    ?assertEqual(<<"GOOD3">>, hb_converge:get(<<"test3">>, Msg3, Opts)),
    ?assertEqual(<<"GOOD4">>,
        hb_converge:get(
            <<"Test4">>,
            hb_converge:set(Msg3, <<"Test4">>, <<"GOOD4">>, Opts)
        )
    ),
    ?assertEqual(not_found, hb_converge:get(<<"test5">>, Msg3, Opts)).

device_excludes_test(Opts) ->
    % Create a device that returns an identifiable message for any key, but also
    % sets excludes to [set], such that the message can be modified using the 
    % default handler.
    Dev = #{
        <<"info">> =>
            fun() ->
                #{
                    excludes => [set],
                    handler => fun() -> {ok, <<"Handler-Value">>} end
                }
            end
    },
    Msg = #{ <<"device">> => Dev, <<"Test-Key">> => <<"Test-Value">> },
    ?assert(hb_converge:is_exported(Msg, Dev, <<"test-key2">>, Opts)),
    ?assert(not hb_converge:is_exported(Msg, Dev, set, Opts)),
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(<<"test-key2">>, Msg, Opts)),
    ?assertMatch(#{ <<"test-key2">> := <<"2">> },
        hb_converge:set(Msg, <<"test-key2">>, <<"2">>, Opts)).

denormalized_device_key_test(Opts) ->
	Msg = #{ <<"Device">> => dev_test },
	?assertEqual(dev_test, hb_converge:get(device, Msg, Opts)),
	?assertEqual(dev_test, hb_converge:get(<<"device">>, Msg, Opts)),
	?assertEqual({module, dev_test},
		erlang:fun_info(
            element(3, hb_converge:message_to_fun(Msg, test_func, Opts)),
            module
        )
    ).

list_transform_test(Opts) ->
    Msg = [<<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>],
    ?assertEqual(<<"A">>, hb_converge:get(1, Msg, Opts)),
    ?assertEqual(<<"B">>, hb_converge:get(2, Msg, Opts)),
    ?assertEqual(<<"C">>, hb_converge:get(3, Msg, Opts)),
    ?assertEqual(<<"D">>, hb_converge:get(4, Msg, Opts)),
    ?assertEqual(<<"E">>, hb_converge:get(5, Msg, Opts)).

as_test(Opts) ->
    % Create a message with the test device, which implements the test_func
    % function. It normally returns `GOOD_FUNCTION'.
    Msg = #{
        <<"device">> => <<"test-device@1.0">>,
        <<"test_func">> => #{ <<"test_key">> => <<"MESSAGE">> }
    },
    ?assertEqual(<<"GOOD_FUNCTION">>, hb_converge:get(<<"test_func">>, Msg, Opts)),
    % Now use the `as' keyword to subresolve a key with the message device.
    ?assertMatch(
        {ok, #{ <<"test_key">> := <<"MESSAGE">> }},
        hb_converge:resolve(
            Msg,
            {as, <<"message@1.0">>, #{ <<"path">> => <<"test_func">> }},
            Opts
        )
    ),
    % Resolve a list of messages in sequence, swapping the device in the middle.
    ?assertEqual(
        {ok, <<"MESSAGE">>},
        hb_converge:resolve_many(
            [
                Msg,
                {as, <<"message@1.0">>, <<>>},
                #{ <<"path">> => <<"test_func">> },
                #{ <<"path">> => <<"test_key">> }
            ],
            Opts
        )
    ).