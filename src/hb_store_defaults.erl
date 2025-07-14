%% @doc Module responsible for applying default configuration to store options.
%% Takes store options and store defaults and returns a new list of stores
%% with default properties applied.
-module(hb_store_defaults).
-export([apply/2]).
-compile({no_auto_import,[apply/2]}).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Apply store defaults to store options.
%% Takes StoreOpts (list of store configuration maps) and Defaults (map of defaults)
%% and returns a new list with defaults applied where appropriate.
apply(StoreOpts, Defaults) when is_list(StoreOpts), is_map(Defaults) ->
    lists:map(
        fun(StoreOpt) ->
            apply_defaults_to_store(StoreOpt, Defaults)
        end,
        StoreOpts
    ).

%% @doc Apply defaults to a single store configuration.
apply_defaults_to_store(StoreOpt, Defaults) when is_map(StoreOpt), is_map(Defaults) ->
    %% First, apply defaults to the main store
    UpdatedStore = apply_defaults_by_module_type(StoreOpt, Defaults),
    %% Then, recursively apply defaults to any sub-stores
    apply_defaults_to_substores(UpdatedStore, Defaults).

%% @doc Apply defaults based on store-module.
apply_defaults_by_module_type(StoreOpt, Defaults) ->
    case maps:get(<<"store-module">>, StoreOpt, undefined) of
        hb_store_lmdb ->
            apply_type_defaults(StoreOpt, <<"lmdb">>, Defaults);
        hb_store_fs ->
            apply_type_defaults(StoreOpt, <<"fs">>, Defaults);
        hb_store_rocksdb ->
            apply_type_defaults(StoreOpt, <<"rocksdb">>, Defaults);
        hb_store_gateway ->
            apply_type_defaults(StoreOpt, <<"gateway">>, Defaults);
        _ ->
            StoreOpt
    end.

%% @doc Apply type-specific defaults to a store.
apply_type_defaults(StoreOpt, TypeKey, Defaults) ->
    case maps:get(TypeKey, Defaults, #{}) of
        TypeDefaults when is_map(TypeDefaults) ->
            maps:merge(TypeDefaults, StoreOpt);
        _ ->
            StoreOpt
    end.

%% @doc Apply defaults to sub-stores recursively.
apply_defaults_to_substores(StoreOpt, Defaults) ->
    %% Handle 'store' key (list of sub-stores)
    UpdatedWithStore =
        case maps:get(<<"store">>, StoreOpt, undefined) of
            SubStores when is_list(SubStores) ->
                UpdatedSubStores = 
                    lists:map(
                        fun(SubStore) ->
                            apply_defaults_to_store(SubStore, Defaults)
                        end,
                        SubStores
                    ),
                maps:put(<<"store">>, UpdatedSubStores, StoreOpt);
            _ ->
                StoreOpt
        end,
    
    %% Handle 'subindex' key (list of sub-stores)
    case maps:get(<<"subindex">>, UpdatedWithStore, undefined) of
        SubIndex when is_list(SubIndex) ->
            UpdatedSubIndex =
                lists:map(
                    fun(SubStore) ->
                        apply_defaults_to_store(SubStore, Defaults)
                    end,
                    SubIndex
                ),
            maps:put(<<"subindex">>, UpdatedSubIndex, UpdatedWithStore);
        _ ->
            UpdatedWithStore
    end.

%% @doc Test function to verify the apply/2 function works correctly.
test() ->
    %% Test data from hb_store_defaults.md
    StoreOpts = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb
            },
            #{
                <<"name">> => <<"cache-mainent">>,
                <<"store-module">> => hb_store_fs
            },
            #{
                <<"store-module">> => hb_store_gateway,
                <<"subindex">> => [
                    #{
                        <<"Data-Protocol">> => <<"ao">>
                    }
                ],
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb
                    }
                ]
            },
            #{
                <<"store-module">> => hb_store_gateway,
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb
                    }
                ]
            }
        ],
    
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => 1073741824
            }
        },
    
    Expected = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb,
                <<"capacity">> => 1073741824
            },
            #{
                <<"name">> => <<"cache-mainent">>,
                <<"store-module">> => hb_store_fs
            },
            #{
                <<"store-module">> => hb_store_gateway,
                <<"subindex">> => [
                    #{
                        <<"Data-Protocol">> => <<"ao">>
                    }
                ],
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb,
                        <<"capacity">> => 1073741824
                    }
                ]
            },
            #{
                <<"store-module">> => hb_store_gateway,
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb,
                        <<"capacity">> => 1073741824
                    }
                ]
            }
        ],
    
    Result = apply(StoreOpts, Defaults),
    
    %% Verify the result matches expected
    case Result =:= Expected of
        true ->
            ?event({test_passed, apply_function_works_correctly}),
            ok;
        false ->
            ?event({test_failed, {expected, Expected}, {got, Result}}),
            error
    end.

%% EUnit tests
apply_test_() ->
    [
        ?_test(test_basic_apply()),
        ?_test(test_empty_defaults()),
        ?_test(test_empty_store_opts()),
        ?_test(test_nested_stores()),
        ?_test(test_lmdb_capacity_integration()),
        ?_test(test_full_integration_flow())
    ].

test_basic_apply() ->
    StoreOpts = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => 1073741824
            }
        },
    
    Expected = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb,
                <<"capacity">> => 1073741824
            }
        ],
    
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

test_empty_defaults() ->
    StoreOpts = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    
    Defaults = #{},
    
    Expected = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

test_empty_store_opts() ->
    StoreOpts = [],
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => 1073741824
            }
        },
    
    Expected = [],
    
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

test_nested_stores() ->
    StoreOpts = 
        [
            #{
                <<"store-module">> => hb_store_gateway,
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb
                    }
                ]
            }
        ],
    
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => 1073741824
            }
        },
    
    Expected = 
        [
            #{
                <<"store-module">> => hb_store_gateway,
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb,
                        <<"capacity">> => 1073741824
                    }
                ]
            }
        ],
    
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

%% @doc Integration test to verify that capacity is properly set for hb_store_lmdb
%% This test verifies that the capacity value is correctly applied and accessible
%% to the hb_store_lmdb module before environment creation.
test_lmdb_capacity_integration() ->
    %% Test with a custom capacity value
    CustomCapacity = 5000,
    
    StoreOpts = 
        [
            #{
                <<"name">> => <<"test-lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => CustomCapacity
            }
        },
    
    %% Apply defaults
    [UpdatedStoreOpt] = apply(StoreOpts, Defaults),
    
    %% Verify capacity is set in the store options
    ?assertEqual(CustomCapacity, maps:get(<<"capacity">>, UpdatedStoreOpt)),
    
    %% Verify that the updated store option would be usable by hb_store_lmdb
    ?assertEqual(<<"test-lmdb">>, maps:get(<<"name">>, UpdatedStoreOpt)),
    ?assertEqual(hb_store_lmdb, maps:get(<<"store-module">>, UpdatedStoreOpt)),
    
    %% Test with default capacity (should use our custom value, not the module default)
    ?assertNotEqual(16 * 1024 * 1024 * 1024, maps:get(<<"capacity">>, UpdatedStoreOpt)),
    
    %% Test that multiple lmdb stores all get the capacity applied
    MultipleStoreOpts = 
        [
            #{
                <<"name">> => <<"test-lmdb-1">>,
                <<"store-module">> => hb_store_lmdb
            },
            #{
                <<"name">> => <<"test-lmdb-2">>,
                <<"store-module">> => hb_store_lmdb
            },
            #{
                <<"name">> => <<"test-fs">>,
                <<"store-module">> => hb_store_fs
            }
        ],
    
    UpdatedMultipleStoreOpts = apply(MultipleStoreOpts, Defaults),
    
    %% Verify both lmdb stores have capacity set
    [LmdbStore1, LmdbStore2, FsStore] = UpdatedMultipleStoreOpts,
    ?assertEqual(CustomCapacity, maps:get(<<"capacity">>, LmdbStore1)),
    ?assertEqual(CustomCapacity, maps:get(<<"capacity">>, LmdbStore2)),
    
    %% Verify fs store does not have capacity (not applicable)
    ?assertEqual(false, maps:is_key(<<"capacity">>, FsStore)),
    
    ?event({integration_test_passed, {lmdb_capacity, CustomCapacity}, {note, "correctly applied to store options"}}).

%% @doc Full integration test simulating the hb_http_server flow
%% This test verifies that the complete flow from config loading to store defaults
%% application works correctly, simulating what happens in hb_http_server:start/0
test_full_integration_flow() ->
    %% Simulate loaded configuration with store_defaults
    LoadedConfig = #{
        <<"store_defaults">> => #{
            <<"lmdb">> => #{
                <<"capacity">> => 5000
            }
        }
    },
    
    %% Simulate the default store configuration from hb_opts:default_message/0
    DefaultStoreOpts = [
        #{
            <<"name">> => <<"cache-mainnet/lmdb">>,
            <<"store-module">> => hb_store_lmdb
        },
        #{
            <<"store-module">> => hb_store_fs,
            <<"name">> => <<"cache-mainnet">>
        },
        #{
            <<"store-module">> => hb_store_gateway,
            <<"subindex">> => [
                #{
                    <<"name">> => <<"Data-Protocol">>,
                    <<"value">> => <<"ao">>
                }
            ],
            <<"store">> => [
                #{
                    <<"store-module">> => hb_store_lmdb,
                    <<"name">> => <<"cache-mainnet/lmdb">>
                }
            ]
        }
    ],
    
    %% Simulate merging with defaults (simplified)
    MergedConfig = maps:merge(
        #{<<"store">> => DefaultStoreOpts},
        LoadedConfig
    ),
    
    %% Extract store options and defaults (simulating hb_http_server logic)
    StoreOpts = maps:get(<<"store">>, MergedConfig),
    StoreDefaults = maps:get(<<"store_defaults">>, MergedConfig, #{}),
    
    %% Apply store defaults
    UpdatedStoreOpts = apply(StoreOpts, StoreDefaults),
    
    %% Verify that LMDB stores have capacity applied
    [LmdbStore, FsStore, GatewayStore] = UpdatedStoreOpts,
    
    %% Check main LMDB store
    ?assertEqual(5000, maps:get(<<"capacity">>, LmdbStore)),
    ?assertEqual(<<"cache-mainnet/lmdb">>, maps:get(<<"name">>, LmdbStore)),
    ?assertEqual(hb_store_lmdb, maps:get(<<"store-module">>, LmdbStore)),
    
    %% Check FS store (no capacity should be added)
    ?assertEqual(false, maps:is_key(<<"capacity">>, FsStore)),
    ?assertEqual(hb_store_fs, maps:get(<<"store-module">>, FsStore)),
    
    %% Check gateway store and its nested LMDB store
    ?assertEqual(hb_store_gateway, maps:get(<<"store-module">>, GatewayStore)),
    NestedStores = maps:get(<<"store">>, GatewayStore),
    [NestedLmdbStore] = NestedStores,
    ?assertEqual(5000, maps:get(<<"capacity">>, NestedLmdbStore)),
    ?assertEqual(hb_store_lmdb, maps:get(<<"store-module">>, NestedLmdbStore)),
    
    %% Verify the updated store options are ready for hb_store:start/1
    ?assertEqual(3, length(UpdatedStoreOpts)),
    
    ?event({full_integration_test_passed, store_defaults_correctly_applied_through_complete_flow}).