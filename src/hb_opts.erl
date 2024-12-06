-module(hb_opts).
-export([get/1, get/2, get/3]).
-include_lib("eunit/include/eunit.hrl").

%%% @moduledoc A module for interacting with local and global options inside
%%% HyperBEAM. Options are set globally, but can also be overridden using an
%%% an optional local `Opts` map argument. Many functions across the HyperBEAM 
%%% environment accept an `Opts` argument, which can be used to customize 
%%% behavior.
%%% 
%%% Options set in an `Opts` map must _never_ change the behavior of a function
%%% that should otherwise be deterministic. Doing so may lead to loss of funds
%%% by the HyperBEAM node operator, as the results of their executions will be
%%% different than those of other node operators. If they are economically 
%%% staked on the correctness of these results, they may experience punishments
%%% for non-verifiable behavior. Instead, if a local node setting makes 
%%% deterministic behavior impossible, the caller should fail the execution 
%%% with a refusal to execute.

%% @doc The default configuration options of the hyperbeam node.
config() ->
    #{
        %%%%%%%% Functional options %%%%%%%%
        %% Scheduling mode: Determines when the SU should inform the recipient
        %% that an assignment has been scheduled for a message.
        %% Options: aggressive(!), local_confirmation, remote_confirmation
        scheduling_mode => local_confirmation,
        %% Compute mode: Determines whether the CU should attempt to execute
        %% more messages on a process after it has returned a result.
        %% Options: aggressive, lazy
        compute_mode => lazy,
        %% Choice of remote nodes for tasks that are not local to hyperbeam.
        http_host => "localhost",
        gateway => "https://arweave.net",
        bundler => "https://up.arweave.net",
        %% Choice of nodes for remote tasks, in the form of a map between
        %% node addresses and HTTP URLs.
        %% `_` is a wildcard for any other address that is not specified.
        nodes => #{
            compute =>
                #{
                    <<"ggltHF0Cnv9ylH3vM1p7amR2vXLMoPLQIUQmAEwLP-k">> =>
                        "http://localhost:8734",
                    <<"J-j0jyZ1YWhMBXtJMWHz-dl-mDcksoJSQo_Fq5loHUs">> =>
                        "http://localhost:8736",
                    '_' => "http://localhost:8734"
                },
            message => #{
                hb:address() => "http://localhost:8734",
                '_' => "http://localhost:8734"
            },
            schedule => #{
                hb:address() => "http://localhost:8734",
                '_' => "http://localhost:8734"
            }
        },
        %% Location of the wallet keyfile on disk that this node will use.
        key_location => "hyperbeam-key.json",
        %% Default page limit for pagination of results from the APIs.
        %% Currently used in the SU devices.
        default_page_limit => 5,
        %% The time-to-live that should be specified when we register
        %% ourselves as a scheduler on the network.
        scheduler_location_ttl => 60 * 60 * 24 * 30,
        %% Preloaded devices for the node to use. These names override
        %% resolution of devices via ID to the default implementations.
        preloaded_devices =>
            #{
                <<"Stack">> => dev_stack,
                <<"Scheduler">> => dev_scheduler,
                <<"Cron">> => dev_cron,
                <<"Deduplicate">> => dev_dedup,
                <<"JSON-Interface">> => dev_json_iface,
                <<"VFS">> => dev_vfs,
                <<"PODA">> => dev_poda,
                <<"Monitor">> => dev_monitor,
                <<"WASM64-pure">> => dev_wasm,
                <<"Multipass">> => dev_multipass,
                <<"Push">> => dev_mu,
                <<"Compute">> => dev_cu,
                <<"P4">> => dev_p4
            },
        %% The stacks of devices that the node should expose by default.
        %% These represent the core flows of functionality of the node.
        default_device_stacks => [
            {<<"data">>, {<<"read">>, [dev_p4, dev_lookup]}},
            {<<"su">>, {<<"schedule">>, [dev_p4, dev_scheduler]}},
            {<<"cu">>, {<<"execute">>, [dev_p4, dev_cu]}}
        ],
        %% Should the node attempt to access data from remote caches for
        %% client requests?
        access_remote_cache_for_client => false,
        %% Should the node attempt to load devices from remote signers?
        load_remote_devices => false,
        %% The list of device signers that the node should trust.
        trusted_device_signers => [],
        %% What should the node do if a client error occurs?
        client_error_strategy => throw,
        % Dev options
        local_store =>
            [{hb_store_fs, #{ prefix => "TEST-data" }}],
        mode => debug,
        debug_stack_depth => 20,
        debug_print_map_line_threshold => 30,
        debug_print_binary_max => 15,
        debug_print_indent => 4,
        debug_print => false,
        cache_results => false,
        stack_print_prefixes => ["hb", "dev", "ar"]
    }.

%% @doc Get an option from the global options, optionally overriding with a
%% local `Opts` map if `prefer` or `only` is set to `local`. If the `only` 
%% option is provided in the `local` map, only keys found in the corresponding
%% (`local` or `global`) map will be returned. This function also offers users
%% a way to specify a default value to return if the option is not set.
%% 
%% `prefer` defaults to `local`.
get(Key) -> ?MODULE:get(Key, undefined).
get(Key, Default) -> ?MODULE:get(Key, Default, #{}).
get(Key, Default, Opts = #{ only := local }) ->
    case maps:find(Key, Opts) of
        {ok, Value} -> Value;
        error -> 
            Default
    end;
get(Key, Default, #{ only := global }) ->
    case global_get(Key, hb_opts_not_found) of
        hb_opts_not_found -> Default;
        Value -> Value
    end;
get(Key, Default, Opts = #{ prefer := global }) ->
    case ?MODULE:get(Key, hb_opts_not_found, #{ only => global }) of
        hb_opts_not_found -> ?MODULE:get(Key, Default, Opts#{ only => local });
        Value -> Value
    end;
get(Key, Default, Opts = #{ prefer := local }) ->
    case ?MODULE:get(Key, hb_opts_not_found, Opts#{ only => local }) of
        hb_opts_not_found ->
            ?MODULE:get(Key, Default, Opts#{ only => global });
        Value -> Value
    end;
get(Key, Default, Opts) ->
    % No preference was set in Opts, so we default to local.
    ?MODULE:get(Key, Default, Opts#{ prefer => local }).

-define(ENV_KEYS,
    #{
        key_location => {"HB_KEY", "hyperbeam-key.json"},
        http_port => {"HB_PORT", fun erlang:list_to_integer/1, "8734"},
        store =>
            {"HB_STORE",
                fun(Dir) ->
                    [
                        {
                            hb_store_fs,
                            #{ prefix => Dir }
                        }
                    ]
                end,
                "TEST-data"
            },
        mode =>
            {"HB_MODE", fun list_to_existing_atom/1},
        debug_print =>
            {"HB_PRINT",
                fun
                    (Str) when Str == "1" -> true;
                    (Str) when Str == "true" -> true;
                    (Str) -> string:tokens(Str, ",")
                end
            }
    }
).

%% @doc Get an environment variable or configuration key.
global_get(Key, Default) ->
    case maps:get(Key, ?ENV_KEYS, Default) of
        Default -> config_lookup(Key, Default);
        {EnvKey, ValParser, DefaultValue} when is_function(ValParser) ->
            ValParser(os:getenv(EnvKey, DefaultValue));
        {EnvKey, ValParser} when is_function(ValParser) ->
            case os:getenv(EnvKey, not_found) of
                not_found -> config_lookup(Key, Default);
                Value -> ValParser(Value)
            end;
        {EnvKey, DefaultValue} ->
            os:getenv(EnvKey, DefaultValue)
    end.

%% @doc An abstraction for looking up configuration variables. In the future,
%% this is the function that we will want to change to support a more dynamic
%% configuration system.
config_lookup(Key, Default) -> maps:get(Key, config(), Default).

%%% Tests

global_get_test() ->
    ?assertEqual(debug, ?MODULE:get(mode)),
    ?assertEqual(debug, ?MODULE:get(mode, production)),
    ?assertEqual(undefined, ?MODULE:get(unset_global_key)),
    ?assertEqual(1234, ?MODULE:get(unset_global_key, 1234)).

local_get_test() ->
    Local = #{ only => local },
    ?assertEqual(undefined, 
        ?MODULE:get(test_key, undefined, Local)),
    ?assertEqual(correct,
        ?MODULE:get(test_key, undefined, Local#{ test_key => correct })).

local_preference_test() ->
    Local = #{ prefer => local },
    ?assertEqual(correct,
        ?MODULE:get(test_key, undefined, Local#{ test_key => correct })),
    ?assertEqual(correct,
        ?MODULE:get(mode, undefined, Local#{ mode => correct })),
    ?assertNotEqual(undefined,
        ?MODULE:get(mode, undefined, Local)).

global_preference_test() ->
    Global = #{ prefer => global },
    ?assertEqual(undefined, ?MODULE:get(test_key, undefined, Global)),
    ?assertNotEqual(incorrect,
        ?MODULE:get(mode, undefined, Global#{ mode => incorrect })),
    ?assertNotEqual(undefined, ?MODULE:get(mode, undefined, Global)).