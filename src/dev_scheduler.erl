%%% @doc A simple scheduler scheme for AO.
%%% This device expects a message of the form:
%%%     Process: #{ id, Scheduler: #{ Authority } }
%%% ```
%%% It exposes the following keys for scheduling:
%%%     #{ method: GET, path: <<"/info">> } ->
%%%         Returns information about the scheduler.
%%%     #{ method: GET, path: <<"/slot">> } -> slot(Msg1, Msg2, Opts)
%%%         Returns the current slot for a process.
%%%     #{ method: GET, path: <<"/schedule">> } -> get_schedule(Msg1, Msg2, Opts)
%%%         Returns the schedule for a process in a cursor-traversable format.
%%%     #{ method: POST, path: <<"/schedule">> } -> post_schedule(Msg1, Msg2, Opts)
%%%         Schedules a new message for a process.'''

-module(dev_scheduler).
%%% Converge API functions:
-export([info/0]).
%%% Local scheduling functions:
-export([schedule/3, router/4]).
%%% CU-flow functions:
-export([slot/3, status/3, next/3]).
-export([start/0, checkpoint/1]).
%%% Test helper exports:
-export([test_process/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

%% @doc Helper to ensure that the environment is started.
start() ->
    % We need the rocksdb backend to run for hb_cache module to work
    application:ensure_all_started(hb),
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>>
        = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}),
    ok.

%% @doc This device uses a default_handler to route requests to the correct
%% function.
info() -> 
    #{
        exports =>
            [
                status,
                next,
                schedule,
                slot,
                init,
                checkpoint
            ],
        excludes => [set, keys],
        default => fun router/4
    }.

%% @doc The default handler for the scheduler device.
router(_, Msg1, Msg2, Opts) ->
    ?event({scheduler_router_called, {msg2, Msg2}, {opts, Opts}}),
    schedule(Msg1, Msg2, Opts).

%% @doc Load the schedule for a process into the cache, then return the next
%% assignment. Assumes that Msg1 is a `dev_process` or similar message, having
%% a `Current-Slot' key. It stores a local cache of the schedule in the
%% `priv/To-Process' key.
next(Msg1, Msg2, Opts) ->
    ?event({scheduler_next_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    Schedule =
        hb_private:get(
            <<"priv/scheduler/assignments">>,
            Msg1,
            Opts
        ),
    LastProcessed = hb_converge:get(<<"current-slot">>, Msg1, Opts),
    ?event({local_schedule_cache, {schedule, Schedule}}),
    Assignments =
        case Schedule of
            X when is_map(X) and map_size(X) > 0 -> X;
            _ ->
                {ok, RecvdAssignments} =
                    hb_converge:resolve(
                        Msg1,
                        #{
                            <<"method">> => <<"GET">>,
                            <<"path">> => <<"schedule/assignments">>,
                            <<"from">> => LastProcessed
                        },
                        Opts
                    ),
                RecvdAssignments
        end,
    ValidKeys =
        lists:filter(
            fun(Slot) -> Slot > LastProcessed end,
            maps:keys(Assignments)
        ),
    % Remove assignments that are below the last processed slot.
    FilteredAssignments = maps:with(ValidKeys, Assignments),
    ?event({filtered_assignments, FilteredAssignments}),
    Slot =
        case ValidKeys of
            [] -> LastProcessed;
            Slots -> lists:min(Slots)
        end,
    ?event({next_slot_to_process, Slot, {last_processed, LastProcessed}}),
    case (LastProcessed + 1) == Slot of
        true ->
            NextMessage =
                hb_converge:get(
                    Slot,
                    FilteredAssignments,
                    Opts
                ),
            NextState =
                hb_private:set(
                    Msg1,
                    <<"schedule/assignments">>,
                    hb_converge:remove(FilteredAssignments, Slot),
                    Opts
                ),
            ?event(
                {next_returning, {slot, Slot}, {message, NextMessage}}),
            {ok, #{ <<"body">> => NextMessage, <<"state">> => NextState }};
        false ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"body">> => <<"No assignment found for next slot.">>
                }
            }
    end.

%% @doc Returns information about the entire scheduler.
status(_M1, _M2, _Opts) ->
    ?event(getting_scheduler_status),
    Wallet = dev_scheduler_registry:get_wallet(),
    {ok,
        #{
            <<"address">> => hb_util:id(ar_wallet:to_address(Wallet)),
            <<"processes">> =>
                lists:map(
                    fun hb_util:id/1,
                    dev_scheduler_registry:get_processes()
                ),
            <<"cache-control">> => <<"no-store">>
        }
    }.

%% @doc A router for choosing between getting the existing schedule, or
%% scheduling a new message.
schedule(Msg1, Msg2, Opts) ->
    ?event({resolving_schedule_request, {msg2, Msg2}, {state_msg, Msg1}}),
    case hb_converge:get(<<"method">>, Msg2, <<"GET">>, Opts) of
        <<"POST">> -> post_schedule(Msg1, Msg2, Opts);
        <<"GET">> -> get_schedule(Msg1, Msg2, Opts)
    end.

%% @doc Schedules a new message on the SU. Searches Msg1 for the appropriate ID,
%% then uses the wallet address of the scheduler to determine if the message is
%% for this scheduler. If so, it schedules the message and returns the assignment.
post_schedule(Msg1, Msg2, Opts) ->
    ?event(scheduling_message),
    ToSched = hb_converge:get(
        <<"body">>, Msg2, Msg2, Opts#{ hashpath => ignore }),
    ProcID = find_id(Msg1, Msg2, Opts),
    PID =
        case dev_scheduler_registry:find(ProcID, false, Opts) of
            not_found ->
                % Check if we are the scheduler for this process.
                ?no_prod("Once we have GQL, get the scheduler location record. "
                    "For now, we'll just use the address of the wallet."),
                Address = hb_util:human_id(ar_wallet:to_address(
                    hb_opts:get(priv_wallet, hb:wallet(), Opts))),
                Proc = find_process(Msg1, Opts),
                case hb_converge:get(<<"scheduler-location">>,
                        Proc, Opts#{ hashpath => ignore }) of
                    Address ->
                        % Start the scheduler process if we are the scheduler.
                        dev_scheduler_registry:find(ProcID, true, Opts);
                    not_found ->
                        throw(
                            {scheduler_location_not_found,
                                {proc_id, ProcID}
                            }
                        );
                    ProcScheduler ->
                        throw(
                            {scheduler_location_mismatch,
                                {local, Address},
                                {required, ProcScheduler}
                            }
                        )
                end;
            Proc -> Proc
        end,
    ?event(
        {post_schedule,
            {schedule_id, ProcID},
            {scheduler_pid, PID},
            {message, ToSched}
        }
    ),
    Verified =
        case hb_opts:get(verify_assignments, true, Opts) of
            true -> hb_message:verify(ToSched);
            false -> true
        end,
    case {Verified, hb_converge:get(type, ToSched)} of
        {false, _} ->
            {ok,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Message is not valid.">>
                }
            };
        {true, <<"Process">>} ->
            hb_cache:write(ToSched, Opts),
            spawn(fun() -> hb_client:upload(ToSched) end),
            ?event(
                {registering_new_process,
                    {proc_id, ProcID},
                    {pid, PID},
                    {is_alive, is_process_alive(PID)}
                }
            ),
            {ok,
                #{
                    <<"status">> => 200,
                    <<"assignment">> => <<"0">>,
                    <<"process">> => ProcID
                }
            };
        {true, _} ->
            % If Message2 is not a process, use the ID of Message1 as the PID
            {ok, dev_scheduler_server:schedule(PID, ToSched)}
    end.

%% @doc Returns information about the current slot for a process.
slot(M1, M2, Opts) ->
    ?event({getting_current_slot, {msg, M1}}),
    ProcID = find_id(M1, M2, Opts),
    ?event({getting_current_slot, {proc_id, ProcID}}),
    {Timestamp, Hash, Height} = ar_timestamp:get(),
    #{ current := CurrentSlot, wallet := Wallet } =
        dev_scheduler_server:info(
            dev_scheduler_registry:find(ProcID)
        ),
    {ok, #{
        <<"process">> => ProcID,
        <<"current-slot">> => CurrentSlot,
        <<"timestamp">> => Timestamp,
        <<"block-height">> => Height,
        <<"block-hash">> => Hash,
        <<"cache-control">> => <<"no-store">>,
        <<"wallet-address">> => hb_util:human_id(ar_wallet:to_address(Wallet))
    }}.

get_schedule(Msg1, Msg2, Opts) ->
    ProcID = find_id(Msg1, Msg2, Opts),
    From =
        case hb_converge:get(<<"from">>, Msg2, not_found, Opts) of
            not_found -> 0;
            X when X < 0 -> 0;
            FromRes -> FromRes
        end,
    To =
        case hb_converge:get(<<"to">>, Msg2, not_found, Opts) of
            not_found ->
                ?event({getting_current_slot, {proc_id, ProcID}}),
                maps:get(current,
                    dev_scheduler_server:info(
                        dev_scheduler_registry:find(ProcID)
                    )
                );
            ToRes -> ToRes
        end,
    Format = hb_converge:get(<<"accept">>, Msg2, <<"application/http">>, Opts),
    gen_schedule(Format, ProcID, From, To, Opts).

%%% Private methods

%% @doc Find the ID from a given request. The precidence order for search is as
%% follows:
%% 1. `Msg2/target`
%% 2. `Msg1/process/id`
%% 3. `Msg1/id`
find_id(Msg1, Msg2, Opts) ->
    TempOpts = Opts#{ hashpath => ignore },
    Res = case hb_converge:get(<<"target">>, Msg2, TempOpts) of
        not_found ->
            case hb_converge:get(<<"process/id">>, Msg1, TempOpts) of
                not_found -> hb_converge:get(<<"id">>, Msg1, TempOpts);
                ID -> ID
            end;
        Target -> Target
    end,
    ?event({found_id, {id, Res}}),
    Res.

%% @doc Find the process from a given request. Check if it has a `process`
%% field, and if so, return that. Otherwise, return the full message.
find_process(Msg, Opts) ->
    hb_converge:get(<<"process">>, Msg, Msg, Opts#{ hashpath => ignore }).

%% @doc Generate a `GET /schedule' response for a process.
gen_schedule(Format, ProcID, From, To, Opts) ->
    ?event(
        {servicing_request_for_assignments,
            {proc_id, ProcID},
            {from, From},
            {to, To}
        }
    ),
    {Assignments, More} = get_assignments(ProcID, From, To, Opts),
    ?event({got_assignments, length(Assignments), {more, More}}),
    % Determine and apply the formatting function to use for generation of the
    % response, based on the `Accept' header.
    FormatterFun =
        case Format of
            <<"application/json">> ->
                fun dev_scheduler_formats:assignments_to_json/4;
            <<"application/http">> ->
                fun dev_scheduler_formats:assignments_to_bundle/4
        end,
    Res = FormatterFun(ProcID, Assignments, More, Opts),
    ?event({assignments_bundle_outbound, {format, Format}, {res, Res}}),
    Res.

%% @doc Get the assignments for a process, and whether the request was truncated.
get_assignments(ProcID, From, RequestedTo, Opts) ->
    ?event({handling_req_to_get_assignments, ProcID, From, RequestedTo}),
    ComputedTo =
        case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
            true -> RequestedTo + ?MAX_ASSIGNMENT_QUERY_LEN;
            false -> RequestedTo
        end,
    {do_get_assignments(ProcID, From, ComputedTo, Opts), ComputedTo =/= RequestedTo }.

%% @doc Get the assignments for a process.
do_get_assignments(_ProcID, From, To, _Opts) when From > To ->
    [];
do_get_assignments(ProcID, From, To, Opts) ->
    case dev_scheduler_cache:read(ProcID, From, Opts) of
        not_found ->
            [];
        {ok, Assignment} ->
            [
                Assignment
                | do_get_assignments(
                    ProcID,
                    From + 1,
                    To,
                    Opts
                )
            ]
    end.

%% @doc Returns the current state of the scheduler.
checkpoint(State) -> {ok, State}.

%%% Tests

%% @doc Generate a _transformed_ process message, not as they are generated 
%% by users. See `dev_process' for examples of AO process messages.
test_process() -> test_process(hb:wallet()).
test_process(Wallet) ->
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    #{
        <<"device">> => ?MODULE,
        <<"device-stack">> =>
            [<<"Cron@1.0">>, <<"WASM-64@1.0">>, <<"PODA@1.0">>],
        <<"image">> => <<"wasm-image-id">>,
        <<"type">> => <<"process">>,
        <<"scheduler-location">> => Address,
        <<"test-random-seed">> => rand:uniform(1337)
    }.

status_test() ->
    start(),
    ?assertMatch(
        #{<<"processes">> := Processes,
            <<"address">> := Address}
            when is_list(Processes) and is_binary(Address),
        hb_converge:get(status, test_process())
    ).

register_new_process_test() ->
    start(),
    Msg1 = test_process(),
    ?event({test_registering_new_process, {msg, Msg1}}),
    ?assertMatch({ok, _},
        hb_converge:resolve(
            Msg1,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Msg1
            },
            #{}
        )
    ),
    ?assert(
        lists:member(
            hb_util:id(Msg1),
            hb_converge:get(<<"processes">>, hb_converge:get(status, Msg1))
        )
    ).

schedule_message_and_get_slot_test() ->
    start(),
    Msg1 = test_process(),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"true">>
            }, hb:wallet())
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    Msg3 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>,
        <<"process">> => hb_util:id(Msg1)
    },
    ?event({pg, dev_scheduler_registry:get_processes()}),
    ?event({getting_schedule, {msg, Msg3}}),
    ?assertMatch({ok, #{ <<"current-slot">> := CurrentSlot }}
            when CurrentSlot > 0,
        hb_converge:resolve(Msg1, Msg3, #{})).

get_schedule_test() ->
    start(),
    Msg1 = test_process(),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"Test-Val">>
            }, hb:wallet())
    },
    Msg3 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:attest(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"Test-Val-2">>
            }, hb:wallet())
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg3, #{})),
    ?assertMatch(
        {ok, _},
        hb_converge:resolve(Msg1, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>,
            <<"target">> => hb_util:id(Msg1)
        },
        #{})
    ).

%%% HTTP tests

http_init() -> http_init(#{}).
http_init(Opts) ->
    start(),
    Wallet = ar_wallet:new(),
    Node = hb_http_server:start_node(Opts#{ priv_wallet => Wallet }),
    {Node, Wallet}.

http_post_schedule_sign(Node, Msg, ProcessMsg, Wallet) ->
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"process">> => ProcessMsg,
        <<"body">> => hb_message:attest(Msg, Wallet)
    }, Wallet),
    hb_http:post(Node, Msg1, #{}).

http_get_slot(N, PMsg) ->
    ID = hb_message:id(PMsg, all),
    Wallet = hb:wallet(),
    {ok, _} = hb_http:get(N, hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/slot">>,
        <<"method">> => <<"GET">>,
        <<"target">> => ID
    }, Wallet), #{}).

http_get_schedule(N, PMsg, From, To) ->
    http_get_schedule(N, PMsg, From, To, <<"application/http">>).

http_get_schedule(N, PMsg, From, To, Format) ->
    ID = hb_message:id(PMsg, all),
    Wallet = hb:wallet(),
    {ok, _} = hb_http:get(N, hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"GET">>,
        <<"target">> => ID,
        <<"from">> => From,
        <<"to">> => To,
        <<"accept">> => Format
    }, Wallet), #{}).

http_post_schedule_test() ->
    {N, W} = http_init(),
    PMsg = hb_message:attest(test_process(W), W),
    {ok, Res} =
        http_post_schedule_sign(
            N,
            #{ <<"inner">> => <<"test-message">> },
            PMsg,
            W
        ),
    ?assertEqual(<<"test-message">>, hb_converge:get(<<"body/inner">>, Res, #{})),
    ?assertMatch({ok, #{ <<"current-slot">> := 0 }}, http_get_slot(N, PMsg)).

http_get_schedule_test() ->
    {Node, Wallet} = http_init(),
    PMsg = hb_message:attest(test_process(Wallet), Wallet),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"process">> => PMsg,
        <<"body">> => hb_message:attest(#{ <<"inner">> => <<"test">> }, Wallet)
    }, Wallet),
    {ok, _} = hb_http:post(Node, Msg1, #{}),
    lists:foreach(
        fun(_) -> {ok, _} = hb_http:post(Node, Msg1, #{}) end,
        lists:seq(1, 10)
    ),
    ?assertMatch({ok, #{ <<"current-slot">> := 10 }}, http_get_slot(Node, PMsg)),
    {ok, Schedule} = http_get_schedule(Node, PMsg, 0, 10),
    Assignments = hb_converge:get(<<"assignments">>, Schedule, #{}),
    ?assertEqual(
        12, % +1 for the hashpath
        length(maps:values(Assignments))
    ).

http_get_json_schedule_test() ->
    {Node, Wallet} = http_init(),
    PMsg = hb_message:attest(test_process(Wallet), Wallet),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"process">> => PMsg,
        <<"body">> => hb_message:attest(#{ <<"inner">> => <<"test">> }, Wallet)
    }, Wallet),
    {ok, _} = hb_http:post(Node, Msg1, #{}),
    lists:foreach(
        fun(_) -> {ok, _} = hb_http:post(Node, Msg1, #{}) end,
        lists:seq(1, 10)
    ),
    ?assertMatch({ok, #{ <<"current-slot">> := 10 }}, http_get_slot(Node, PMsg)),
    {ok, Schedule} = http_get_schedule(Node, PMsg, 0, 10, <<"application/json">>),
    ?event(debug, {schedule, Schedule}),
    JSON = hb_converge:get(<<"body">>, Schedule, #{}),
    Assignments = jiffy:decode(JSON, [return_maps]),
    ?assertEqual(
        11, % +1 for the hashpath
        length(maps:get(<<"edges">>, Assignments))
    ).

%%% Benchmarks

single_converge(Opts) ->
    start(),
    BenchTime = 1,
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Msg1 = test_process(Wallet),
    ?event({benchmark_start, ?MODULE}),
    MsgToSchedule = hb_message:attest(#{
        <<"type">> => <<"Message">>,
        <<"test-key">> => <<"test-val">>
    }, Wallet),
    Iterations = hb:benchmark(
        fun(_) ->
            MsgX = #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => MsgToSchedule
            },
            ?assertMatch({ok, _}, hb_converge:resolve(Msg1, MsgX, Opts))
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    Msg3 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>,
        <<"process">> => hb_util:id(Msg1)
    },
    ?assertMatch({ok, #{ <<"current-slot">> := CurrentSlot }}
            when CurrentSlot == Iterations - 1,
        hb_converge:resolve(Msg1, Msg3, Opts)),
    ?event(bench, {res, Iterations - 1}),
    hb_util:eunit_print(
        "Scheduled ~p messages through Converge in ~p seconds (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 3).

many_clients(Opts) ->
    BenchTime = 1,
    Processes = hb_opts:get(workers, 25, Opts),
    {Node, Wallet} = http_init(Opts),
    PMsg = hb_message:attest(test_process(Wallet), Wallet),
    Msg1 = hb_message:attest(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"process">> => PMsg,
        <<"body">> => hb_message:attest(#{ <<"inner">> => <<"test">> }, Wallet)
    }, Wallet),
    {ok, _} = hb_http:post(Node, Msg1, Opts),
    Iterations = hb:benchmark(
        fun(X) ->
            {ok, _} = hb_http:post(Node, Msg1, Opts),
            ?event(bench, {iteration, X, self()})
        end,
        BenchTime,
        Processes
    ),
    ?event({iterations, Iterations}),
    hb_util:eunit_print(
        "Scheduled ~p messages with ~p workers through HTTP in ~ps (~.2f msg/s)",
        [Iterations, Processes, BenchTime, Iterations / BenchTime]
    ),
    {ok, Res} = http_get_slot(Node, PMsg),
    ?event(bench, {res, Res}),
    ?assert(Iterations > 10).

benchmark_suite_test_() ->
    rand:seed(exsplus, erlang:timestamp()),
    Port = 30000 + rand:uniform(10000),
    Bench = [
        {benchmark, "benchmark", fun single_converge/1},
        {multihttp_benchmark, "multihttp_benchmark", fun many_clients/1}
    ],
    OptSpecs = [
        #{
            name => fs,
            opts => #{
                store => {hb_store_fs, #{
                    prefix => <<"TEST-cache/fs-",
                        (integer_to_binary(Port))/binary>>
                }},
                scheduling_mode => local_confirmation,
                port => Port
            },
            desc => "FS store, local conf."
        },
        #{
            name => fs_aggressive,
            opts => #{
                store => {hb_store_fs, #{
                    prefix => <<"TEST-cache/fs-",
                        (integer_to_binary(Port))/binary>>
                }},
                scheduling_mode => aggressive,
                port => Port + 1
            },
            desc => "FS store, aggressive conf."
        },
        #{
            name => rocksdb,
            opts => #{
                store => {hb_store_rocksdb, #{
                    prefix => <<"TEST-cache-rocksdb-",
                        (integer_to_binary(Port+1))/binary>>
                }},
                scheduling_mode => local_confirmation,
                port => Port + 2
            },
            desc => "RocksDB store, local conf."
        },
        #{
            name => rocksdb_aggressive,
            opts => #{
                store => {hb_store_rocksdb, #{
                    prefix => <<"TEST-cache-rocksdb-",
                        (integer_to_binary(Port+2))/binary>>
                }},
                scheduling_mode => aggressive,
                port => Port + 3
            },
            desc => "RocksDB store, aggressive conf."
        }
        % #{
        %     name => rocksdb_extreme_aggressive_h3,
        %     opts => #{
        %         store => {hb_store_rocksdb, #{
        %             prefix => <<"TEST-cache-rocksdb-",
        %                 (integer_to_binary(Port+3))/binary>>
        %         }},
        %         scheduling_mode => aggressive,
        %         protocol => http3,
        %         workers => 100
        %     },
        %     desc => "100xRocksDB store, aggressive conf, http/3."
        % }
    ],
    hb_test_utils:suite_with_opts(Bench, OptSpecs).
