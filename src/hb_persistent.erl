-module(hb_persistent).
-export([find_or_register/3, unregister_notify/4]).
-export([notify/4, await/4, start_worker/2]).
-export([find/2, find/3]).
-export([register/2, register/3, unregister/2, unregister/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc Creates and manages long-lived Converge resolution processes.
%%% These can be useful for situations where a message is large and expensive
%%% to serialize and deserialize, or when executions should be deliberately
%%% serialized to avoid parallel executions of the same computation. This 
%%% module is called during the core `hb_converge` execution process, so care
%%% must be taken to avoid recursive spawns/loops.
%%% 
%%% Built using the `pg` module, which is a distributed Erlang process group
%%% manager.

%% @doc Ensure that the `pg` module is started.
start() -> pg:start(pg).

%% @doc Register the process to lead an execution if none is found, otherwise
%% signal that we should await resolution.
find_or_register(Msg1, Msg2, Opts) ->
    GroupName = group(Msg1, Msg2, Opts),
    case find_groupname(GroupName, Opts) of
        not_found ->
            register_groupname(GroupName, Opts),
            leader;
        {ok, [Leader|_]} -> {wait, Leader}
    end.

%% @doc Unregister as the leader for an execution and notify waiting processes.
unregister_notify(Msg1, Msg2, Msg3, Opts) ->
    GroupName = group(Msg1, Msg2, Opts),
    unregister_groupname(GroupName, Opts),
    notify(Msg1, Msg2, Msg3, Opts).

%% @doc Find a process that is already managing a specific Converge resolution.
find(Msg1, Opts) -> find(Msg1, undefined, Opts).
find(Msg1, Msg2, Opts) ->
    case find_groupname(group(Msg1, Msg2, Opts), Opts) of
        [] -> not_found;
        Procs -> {ok, Procs}
    end.

%% @doc Find a group with the given name.
find_groupname(Groupname, _Opts) ->
    start(),
    case pg:get_local_members(Groupname) of
        [] -> not_found;
        Procs -> {ok, Procs}
    end.

%% @doc Calculate the group name for a Msg1 and Msg2 pair. Uses the Msg1's
%% `group' function if it is found in the `info', otherwise uses the default.
group(Msg1, Msg2, Opts) ->
    Grouper = maps:get(group, hb_converge:info(Msg1, Opts), fun default_group/2),
    apply(Grouper, hb_converge:truncate_args(Grouper, [Msg1, Msg2, Opts])).

%% @doc Create a group name from a Msg1 and Msg2 pair as a tuple.
default_group(Msg1, Msg2) ->
    {Msg1, Msg2}.

%% @doc Register for performing a Converge resolution.
register(Msg1, Opts) -> register(Msg1, undefined, Opts).
register(Msg1, Msg2, Opts) ->
    ?event({register_resolver, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    register_groupname(group(Msg1, Msg2, Opts), Opts).
register_groupname(Groupname, _Opts) ->
    pg:join(Groupname, self()).

%% @doc Unregister for being the leader on a Converge resolution.
unregister(Msg1, Opts) -> unregister(Msg1, undefined, Opts).
unregister(Msg1, Msg2, Opts) ->
    start(),
    ?event({unregister_resolver, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    unregister_groupname(group(Msg1, Msg2, Opts), Opts).
unregister_groupname(Groupname, _Opts) ->
    pg:leave(Groupname, self()).

%% @doc If there was already an Erlang process handling this execution,
%% we should register with them and wait for them to notify us of
%% completion.
await(Worker, Msg1, Msg2, Opts) ->
    % Calculate the compute path that we will wait upon resolution of.
    % Register with the process.
    ?no_prod("We should find a more effective way to represent the "
        "requested execution. This may cause memory issues."),
    Worker ! {resolve, self(), Msg1, Msg2, Opts},
    ?event({await_resolution, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    % Wait for the result.
    receive
        {resolved, Worker, Msg1, Msg2, Msg3} ->
            ?event({await_resolution_received, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
            ?no_prod("Should we handle response matching in a more "
            " fine-grained manner?"),
            Msg3
    end.

%% @doc Check our inbox for processes that are waiting for the resolution
%% of this execution.
notify(Msg1, Msg2, Msg3, Opts) ->
    ?event({notify_waiting, {msg1, Msg1}, {msg2, Msg2}, {msg3, Msg3}}),
    receive
        {resolve, Listener, Msg1, Msg2, _ListenerOpts} ->
            send_response(Listener, Msg1, Msg2, Msg3),
            notify(Msg1, Msg2, Msg3, Opts)
    after 0 ->
        ?event(finished_notify),
        ok
    end.

%% @doc Helper function that wraps responding with a new Msg3.
send_response(Listener, Msg1, Msg2, Msg3) ->
    ?event({send_response, {msg1, Msg1}, {msg2, Msg2}, {msg3, Msg3}}),
    Listener ! {resolved, self(), Msg1, Msg2, Msg3}.

%% @doc Start a worker process that will hold a message in memory for
%% future executions.
start_worker(Msg, Opts) ->
    WorkerPID = spawn(
        fun() ->
            % If the device's info contains a `worker` function we
            % use that instead of the default implementation.
            WorkerFun =
                maps:get(
                    worker,
                    hb_converge:info(Msg, Opts),
                    fun default_worker/2
                ),
            % Call the worker function, unsetting the option
            % to avoid recursive spawns.
            apply(
                WorkerFun,
                hb_converge:truncate_args(
                    WorkerFun,
                    [Msg, Opts#{ spawn_worker := false }])
            )
        end
    ),
    ?MODULE:register(Msg, WorkerPID).

%% @doc A server function for handling persistent executions. 
default_worker(Msg1, Opts) ->
    Timeout = hb_opts:get(worker_timeout, infinity, Opts),
    receive
        {resolve, Listener, Msg1, Msg2, _ListenerOpts} ->
            Msg3 = hb_converge:resolve(Msg1, Msg2, Opts),
            send_response(Listener, Msg1, Msg2, Msg3),
            notify(Msg1, Msg2, Msg3, Opts),
            % In this (default) worker implementation we do not advance the
            % process to monitor resolution of `Msg3`, staying instead with
            % Msg1 indefinitely.
            default_worker(Msg1, Opts)
    after Timeout ->
        % We have hit the in-memory persistence timeout. Check whether the
        % device has shutdown procedures (for example, writing in-memory
        % state to the cache).
        unregister(Msg1, undefined, Opts),
        hb_converge:resolve(Msg1, terminate, Opts#{ hashpath := ignore })
    end.

%%% Tests

%% @doc Test merging and returning a value with a persistent worker.
merged_execution_test() ->
    Device =
        #{
            info =>
                fun() ->
                    #{
                        group => fun(Msg) -> Msg end
                    }
                end,
            slow_key =>
                fun(_, #{ wait := Wait }) ->
                    receive after Wait ->
                        {ok,
                            #{
                                waited => Wait,
                                pid => self(),
                                random_bytes =>
                                    hb_util:encode(crypto:strong_rand_bytes(4))
                            }
                        }
                    end
                end
        },
    TestTime = 200,
    Msg1 = #{ device => Device },
    Msg2 = #{ path => [slow_key], wait => TestTime },
    TestParent = self(),
    ParalellTestFun =
        fun(Ref) ->
            ?event({starting_test_worker, {time, TestTime}}),
            Res = hb_converge:resolve(Msg1, Msg2, #{}),
            ?event({test_worker_got_result, {time, TestTime}, {result, Res}}),
            TestParent ! {result, Ref, Res}
        end,
    GatherResult = fun(Ref) -> receive {result, Ref, Res} -> Res end end,
    T0 = hb:now(),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    spawn_link(fun() -> ParalellTestFun(Ref1) end),
    receive after 100 -> ok end,
    spawn_link(fun() -> ParalellTestFun(Ref2) end),
    Res1 = GatherResult(Ref1),
    Res2 = GatherResult(Ref2),
    T1 = hb:now(),
    % Check the result is the same.
    ?assertEqual(Res1, Res2),
    % Check the time it took is less than the sum of the two test times.
    ?assert(T1 - T0 < (2*TestTime)).
