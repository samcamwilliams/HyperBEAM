%%% @doc Hyperbeam is a decentralized node implementating the Converge Protocol
%%% on top of Arweave.
%%% 
%%% This protocol offers a computation layer for executing arbitrary logic on 
%%% top of the network's data.
%%% 
%%% Arweave is built to offer a robust, permanent storage layer for static data
%%% over time. It can be seen as a globally distributed key-value store that
%%% allows users to lookup IDs to retrieve data at any point in time:
%%% 
%%% 	`Arweave(ID) => Message'
%%% 
%%% Hyperbeam adds another layer of functionality on top of Arweave's protocol:
%%% Allowing users to store and retrieve not only arbitrary bytes, but also to
%%% perform execution of computation upon that data:
%%% 
%%% 	`Hyperbeam(Message1, Message2) => Message3'
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
%%% 1. The `hb' and `hb_opts' modules manage the node's configuration, 
%%%    environment variables, and debugging tools.
%%% 
%%% 2. The `hb_http' and `hb_http_server' modules manage all HTTP-related
%%%    functionality. `hb_http_server' handles turning received HTTP requests
%%%    into messages and applying those messages with the appropriate devices.
%%%    `hb_http' handles making requests and responding with messages. `cowboy'
%%%    is used to implement the underlying HTTP server.
%%% 
%%% 3. `hb_converge' implements the computation logic of the node: A mechanism
%%%    for resolving messages to other messages, via the application of logic
%%%    implemented in `devices'. `hb_converge' also manages the loading of Erlang
%%%    modules for each device into the node's environment. There are many
%%%    different default devices implemented in the hyperbeam node, using the
%%%    namespace `dev_*'. Some of the critical components are:
%%% 
%%%     - `dev_message': The default handler for all messages that do not 
%%%      specify their own device. The message device is also used to resolve
%%%      keys that are not implemented by the device specified in a message,
%%%      unless otherwise signalled.
%%% 
%%%    - `dev_stack': The device responsible for creating and executing stacks
%%%      of other devices on messages that request it. There are many uses for
%%%      this device, one of which is the resolution of AO processes.
%%% 
%%%    - `dev_p4': The device responsible for managing payments for the services
%%%      provided by the node.
%%% 
%%% 4. `hb_store', `hb_cache' and the store implementations forms a layered
%%%    system for managing the node's access to persistent storage. `hb_cache'
%%%    is used as a resolution mechanism for reading and writing messages, while
%%%    `hb_store' provides an abstraction over the underlying persistent key-value
%%%    byte storage mechanisms. Example `hb_store' mechanisms can be found in
%%%    `hb_store_fs' and `hb_store_remote_node'.
%%% 
%%% 5. `ar_*' modules implement functionality related to the base-layer Arweave
%%%    protocol and are largely unchanged from their counterparts in the Arweave
%%%    node codebase presently maintained by the Digital History Association
%%%    (@dha-team/Arweave).
%%% 
%%% You can find documentation of a similar form to this note in each of the core
%%% modules of the hyperbeam node.
-module(hb).
%%% Configuration and environment:
-export([init/0, now/0, build/0]).
%%% Debugging tools:
-export([event/1, event/2, event/3, event/4, event/5, event/6, no_prod/3]).
-export([read/1, read/2, debug_wait/4, profile/1, benchmark/2, benchmark/3]).
%%% Node wallet and address management:
-export([address/0, wallet/0, wallet/1]).
-include("include/hb.hrl").

%% @doc Initialize system-wide settings for the hyperbeam node.
init() ->
    pg:start(pg),
    ?event({setting_debug_stack_depth, hb_opts:get(debug_stack_depth)}),
    Old = erlang:system_flag(backtrace_depth, hb_opts:get(debug_stack_depth)),
    ?event({old_system_stack_depth, Old}),
    ok.

wallet() ->
    wallet(hb_opts:get(key_location)).
wallet(Location) ->
    case file:read_file_info(Location) of
        {ok, _} ->
            ar_wallet:load_keyfile(Location);
        {error, _} -> 
            Res = ar_wallet:new_keyfile(?DEFAULT_KEY_TYPE, Location),
            ?event({created_new_keyfile, Location, address(Res)}),
            Res
    end.

%% @doc Get the address of a wallet. Defaults to the address of the wallet
%% specified by the `key_location' configuration key. It can also take a
%% wallet tuple as an argument.
address() -> address(wallet()).
address(Wallet) when is_tuple(Wallet) ->
    hb_util:encode(ar_wallet:to_address(Wallet));
address(Location) -> address(wallet(Location)).

%% @doc Debugging event logging function. For now, it just prints to standard
%% error.
event(X) -> event(global, X).
event(Topic, X) -> event(Topic, X, "").
event(Topic, X, Mod) -> event(Topic, X, Mod, undefined).
event(Topic, X, Mod, Func) -> event(Topic, X, Mod, Func, undefined).
event(Topic, X, Mod, Func, Line) -> event(Topic, X, Mod, Func, Line, #{}).
event(Topic, X, Mod, undefined, Line, Opts) -> event(Topic, X, Mod, "", Line, Opts);
event(Topic, X, Mod, Func, undefined, Opts) -> event(Topic, X, Mod, Func, "", Opts);
event(Topic, X, ModAtom, Func, Line, Opts) when is_atom(ModAtom) ->
    % Check if the module has the `hb_debug` attribute set to `print`.
    case lists:member({hb_debug, [print]}, ModAtom:module_info(attributes)) of
        true -> hb_util:debug_print(X, atom_to_list(ModAtom), Func, Line);
        false -> 
            % Check if the module has the `hb_debug` attribute set to `no_print`.
            case lists:keyfind(hb_debug, 1, ModAtom:module_info(attributes)) of
                {hb_debug, [no_print]} -> X;
                _ -> event(Topic, X, atom_to_list(ModAtom), Func, Line, Opts)
            end
    end;
event(Topic, X, ModStr, Func, Line, Opts) ->
    % Check if the debug_print option has the topic in it if set.
    case hb_opts:get(debug_print, false, Opts) of
        ModList when is_list(ModList) ->
            (lists:member(ModStr, ModList)
                orelse lists:member(atom_to_list(Topic), ModList))
                andalso hb_util:debug_print(X, ModStr, Func, Line);
        true -> hb_util:debug_print(X, ModStr, Func, Line);
        false -> X
    end.

%% @doc Debugging function to read a message from the cache.
%% Specify either a scope atom (local or remote) or a store tuple
%% as the second argument.
read(ID) -> read(ID, local).
read(ID, ScopeAtom) when is_atom(ScopeAtom) ->
    read(ID, hb_store:scope(hb_opts:get(store), ScopeAtom));
read(ID, Store) ->
    hb_cache:read(Store, hb_util:id(ID)).

%% @doc Utility function to throw an error if the current mode is prod and
%% non-prod ready code is being executed. You can find these in the codebase
%% by looking for ?NO_PROD calls.
no_prod(X, Mod, Line) ->
    case hb_opts:get(mode) of
        prod ->
            io:format(standard_error,
                "=== DANGER: NON-PROD READY CODE INVOKED IN PROD ===~n", []),
            io:format(standard_error, "~w:~w:       ~p~n", [Mod, Line, X]),
            case hb_opts:get(exit_on_no_prod) of
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
debug_wait(T, Mod, Func, Line) ->
    ?event(wait, {debug_wait, {T, Mod, Func, Line}}),
    receive after T -> ok end.

%% @doc Run a function as many times as possible in a given amount of time.
benchmark(Fun, TLen) ->
    T0 = erlang:system_time(millisecond),
    until(
        fun() -> erlang:system_time(millisecond) - T0 > (TLen * 1000) end,
        Fun,
        0
    ).

%% @doc Run multiple instances of a function in parallel for a given amount of time.
benchmark(Fun, TLen, Procs) ->
    Parent = self(),
    StartWorker =
        fun(_) ->
            Ref = make_ref(),
            link(spawn(fun() ->
                Count = benchmark(Fun, TLen),
                Parent ! {work_complete, Ref, Count}
            end)),
            Ref
        end,
    CollectRes =
        fun(R) ->
            receive
                {work_complete, R, Count} ->
                    Count
            end
        end,
    Refs = lists:map(StartWorker, lists:seq(1, Procs)),
    lists:sum(lists:map(CollectRes, Refs)).

until(Condition, Fun, Count) ->
    case Condition() of
        false ->
            case apply(Fun, hb_converge:truncate_args(Fun, [Count])) of
                {count, AddToCount} ->
                    until(Condition, Fun, Count + AddToCount);
                _ ->
                    until(Condition, Fun, Count + 1)
            end;
        true -> Count
    end.
