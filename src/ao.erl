-module(ao).
%%% Configuration and environment:
-export([config/0, now/0, get/1, get/2, build/0]).
%%% Debugging tools:
-export([event/1, event/2, event/3, no_prod/3]).
-export([read/1, read/2, debug_wait/3, profile/1]).
%%% Node wallet and address management:
-export([address/0, wallet/0, wallet/1]).
-include("include/ar.hrl").

%%% Hyperbeam is a decentralized node implementation implementing a protocol built
%%% on top of the Arweave protocol. This protocol offers a computation layer for
%%% executing arbitrary logic on top of data inside the Arweave network.
%%% 
%%% Arweave is built to offer a robust, permanent storage layer for static data
%%% over time. One can see it essentially as a globally distributed key-value
%%% store that allows users to lookup IDs to retrieve data at any point in time:
%%% 
%%% 	`Arweave(ID) => Message`
%%% 
%%% Hyperbeam adds another layer of functionality on top of Arweave's protocol:
%%% Allowing users to store and retrieve not only arbitrary bytes, but also to
%%% perform execution of computation upon that data:
%%% 
%%% 	`Hyperbeam(Message0) => Message1`
%%% 
%%% When Hyperbeam executes a message, it will return a new message containing
%%% the result of that execution, as well as signed attestations of its
%%% correctness. If the computation that is executed is deterministic, recipients
%%% of the new message are able to verify that the computation was performed
%%% correctly. The new message may be stored back to Arweave if desired,
%%% forming a permanent, verifiable, and decentralized log of computation.
%%% 
%%% The mechanisms described above form the basis of a decentralized and
%%% verifiable compute engine without any relevant protocol-enforced
%%% scalability limits. It is an implementation of a global, shared
%%% supercomputer.
%%% 
%%% Hyperbeam can be used for an extremely large variety of applications, from
%%% serving static Arweave data with signed commitments of correctness, to
%%% executing smart contracts that have _built-in_ HTTP APIs. The Hyperbeam
%%% node implementation implements AO, an Actor-Oriented process-based
%%% environment for orchestrating computation over Arweave messages in order to
%%% facilitate the execution of more traditional, consensus-based smart
%%% contracts.
%%% 
%%% The core abstractions of the Hyperbeam node are broadly as follows:
%%% 
%%% 1. The `ao` module manages the node's configuration, environment variables,
%%%    and debugging tools.
%%% 2. The `ao_http` and `ao_http_router` modules manage all HTTP-related
%%%    functionality. `ao_http_router` handles turning received HTTP requests
%%%    into messages and applying those messages with the appropriate devices.
%%%    `ao_http` handles making requests and responding with messages. `cowboy`
%%%    is used to implement the actual HTTP server.
%%% 3. `ao_device` implements the computation logic of the node: A mechanism
%%%    for resolving messages to other messages, via the application of logic
%%%    implemented in devices. `ao_device_loader` manages the loading of Erlang
%%%    modules for each device into the node's environment. There are many
%%%    different default devices implemented in the hyperbeam node, using the
%%%    namespace `dev_*`. Some of the critical components are:
%%%    - `dev_meta`: The device responsible for managing all requests to the
%%%      node. This device takes a message and loads the appropriate devices to
%%%      execute it. The `ao_http_router` uses this device to resolve incoming
%%%      HTTP requests.
%%%    - `dev_stack`: The device responsible for creating and executing stacks
%%%      of other devices on messages that request it. There are many uses for
%%%      this device, one of which is the resolution of AO processes, which
%%%      typically require it.
%%%    - `dev_p4`: The device responsible for managing payments for the services
%%%      provided by the node.
%%% 4. `ao_store`, `ao_cache` and the store implementations forms a layered
%%%    system for managing the node's access to persistent storage. `ao_cache`
%%%    is used as a resolution mechanism for reading and writing messages, while
%%%    `ao_store` provides an abstraction over the underlying persistent key-value
%%%    byte storage mechanisms. Example `ao_store` mechanisms can be found in
%%%    `ao_fs_store` and `ao_remote_node_store`.
%%% 5. `ar_*` modules implement functionality related to the base-layer Arweave
%%%    protocol and are largely unchanged from their counterparts in the Arweave
%%%    node codebase presently maintained by the Digital History Association
%%%    (@dha-team/Arweave).
%%% 
%%% You can find documentation of a similar form to this note in each of the core
%%% modules of the hyperbeam node.

%%% @doc This module implements the environment and configuration functionality
%%% for the hyperbeam node. It manages all global components of the node,
%%% including the node's wallet, address, configuration, and environment
%%% variables.

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
                        "http://localhost:8734/cu",
                    <<"J-j0jyZ1YWhMBXtJMWHz-dl-mDcksoJSQo_Fq5loHUs">> =>
                        "http://localhost:8736/cu",
                    '_' => "http://localhost:8734/cu"
                },
            message => #{
                address() => "http://localhost:8734/mu",
                '_' => "http://localhost:8734/mu"
            },
            schedule => #{
                address() => "http://localhost:8734/su",
                '_' => "http://localhost:8734/su"
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
                <<"Stack">> => {dev_stack, execute},
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
            {<<"cu">>, {<<"execute">>, [dev_p4, dev_cu]}},
            {<<"mu">>, {<<"push">>, [dev_p4, dev_mu]}}
        ],
		%% Should the node attempt to access data from remote caches for
		%% client requests?
		access_remote_cache_for_client => false,
		%% What should the node do if a client error occurs?
		client_error_strategy => throw,
        % Dev options
        local_store =>
            [{ao_fs_store, #{ prefix => "TEST-data" }}],
        mode => debug,
        debug_print => true
    }.

-define(ENV_KEYS,
    #{
        key_location => {"AO_KEY", "hyperbeam-key.json"},
        http_port => {"AO_PORT", fun erlang:list_to_integer/1, "8734"},
        store =>
            {"AO_STORE",
                fun(Dir) ->
                    [
                        {
                            ao_fs_store,
                            #{ prefix => Dir }
                        },
                        {
                            ao_remote_node_store,
                            #{ node => "http://localhost:8734" }
                        }
                    ]
                end,
                "TEST-data"
            }
    }
).

wallet() ->
    wallet(ao:get(key_location)).
wallet(Location) ->
    case file:read_file_info(Location) of
        {ok, _} -> ar_wallet:load_keyfile(Location);
        {error, _} -> ar_wallet:new_keyfile(?DEFAULT_KEY_TYPE, ao:get(key_location))
    end.

address() ->
    ar_util:encode(ar_wallet:to_address(wallet())).

%% @doc Get an environment variable or configuration key.
get(Key) -> get(Key, undefined).
get(Key, Default) ->
    case maps:get(Key, ?ENV_KEYS, false) of
        false -> config_lookup(Key, Default);
        {EnvKey, ValParser, DefaultValue} when is_function(ValParser) ->
            ValParser(os:getenv(EnvKey, DefaultValue));
        {EnvKey, DefaultValue} ->
            os:getenv(EnvKey, DefaultValue)
    end.

%% @doc An abstraction for looking up configuration variables. In the future,
%% this is the function that we will want to change to support a more dynamic
%% configuration system.
config_lookup(Key, Default) -> maps:get(Key, config(), Default).

%% @doc Debugging event logging function. For now, it just prints to standard
%% error.
event(X) -> event(X, "").
event(X, Mod) -> event(X, Mod, undefined).
event(X, ModStr, undefined) -> event(X, ModStr, "");
event(X, ModAtom, Line) when is_atom(ModAtom) ->
    case lists:member({ao_debug, [print]}, ModAtom:module_info(attributes)) of
        true -> debug_print(X, atom_to_list(ModAtom), Line);
        false -> 
            case lists:keyfind(ao_debug, 1, ModAtom:module_info(attributes)) of
                {ao_debug, [no_print]} -> X;
                _ -> event(X, atom_to_list(ModAtom), Line)
            end
    end;
event(X, ModStr, Line) ->
    case ao:get(debug_print) of
        true -> debug_print(X, ModStr, Line);
        false -> X
    end.

%% @doc Print a message to the standard error stream, prefixed by the amount
%% of time that has elapsed since the last call to this function.
debug_print(X, ModStr, LineNum) ->
    Now = erlang:system_time(millisecond),
    Last = erlang:put(last_debug_print, Now),
    TSDiff = case Last of undefined -> 0; _ -> Now - Last end,
    io:format(standard_error, "=== HB DEBUG ===[~pms in ~p @ ~s:~w]==> ~s~n",
        [
			TSDiff, self(), ModStr, LineNum,
			lists:flatten(debug_fmt(X))
		]),
    X.

%% @doc Convert a term to a string for debugging print purposes.
debug_fmt({X, Y}) when is_atom(X) and is_atom(Y) ->
    io_lib:format("~p: ~p", [X, Y]);
debug_fmt({X, Y}) when is_record(Y, tx) ->
    io_lib:format("~p => Message:~n~s",
        [X, lists:flatten(ar_bundles:format(Y, 1))]);
debug_fmt(Tuple) when is_tuple(Tuple) ->
    format_tuple(Tuple);
debug_fmt(Str = [X | _]) when is_integer(X) andalso X >= 32 andalso X < 127 ->
    lists:flatten(io_lib:format("~s", [Str]));
debug_fmt(X) ->
    lists:flatten(io_lib:format("~120p", [X])).

%% @doc Helper function to format tuples with arity greater than 2.
format_tuple(Tuple) ->
    Elements = tuple_to_list(Tuple),
    FormattedElements = lists:map(fun debug_fmt/1, Elements),
    io_lib:format("~s", [string:join(FormattedElements, ", ")]).

%% @doc Debugging function to read a message from the cache.
%% Specify either a scope atom (local or remote) or a store tuple
%% as the second argument.
read(ID) -> read(ID, local).
read(ID, ScopeAtom) when is_atom(ScopeAtom) ->
    read(ID, ao_store:scope(ao:get(store), ScopeAtom));
read(ID, Store) ->
    ao_cache:read_message(Store, ao_message:id(ID)).

%% @doc Utility function to throw an error if the current mode is prod and
%% non-prod ready code is being executed. You can find these in the codebase
%% by looking for ?NO_PROD calls.
no_prod(X, Mod, Line) ->
    case ao:get(mode) of
        prod ->
            io:format(standard_error,
                "=== DANGER: NON-PROD READY CODE INVOKED IN PROD ===~n", []),
            io:format(standard_error, "~w:~w:       ~p~n", [Mod, Line, X]),
			case ao:get(exit_on_no_prod) of
				true -> init:stop();
				false -> throw(X)
			end;
        _ -> X
    end.

%% @doc Utility function to get the current time in milliseconds.
now() ->
    erlang:system_time(millisecond).

%% @doc Utility function to hot-recompile and load the hyperbeam environment.
build() ->
    r3:do(compile, [{dir, "src"}]).

%% @doc Utility function to start a profiling session and run a function,
%% then analyze the results. Obviously -- do not use in production.
profile(Fun) ->
    eprof:start_profiling([self()]),
    try
        Fun()
    after
        eprof:stop_profiling()
    end,
    eprof:analyze(total).

%% @doc Utility function to wait for a given amount of time, printing a debug
%% message to the console first.
debug_wait(T, Mod, Line) ->
    debug_print({debug_wait, T}, Mod, Line),
    receive after T -> ok end.
