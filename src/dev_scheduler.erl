-module(dev_scheduler).
%%% Converge API functions:
-export([set/3, keys/1, info/0]).
%%% Local scheduling functions:
-export([schedule/3]).
%%% CU-flow functions:
-export([slot/3, status/3, next/3]).
-export([start/0, init/3, end_of_schedule/3, checkpoint/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A simple scheduler scheme for AO.
%%% This device expects a message of the form:
%%%     Process: #{ id, Scheduler: #{ Authority } }
%%% 
%%% It exposes the following keys for scheduling:
%%%     #{ method: GET, path: <<"/info">> } ->
%%%         Returns information about the scheduler.
%%%     #{ method: GET, path: <<"/slot">> } -> slot(Msg1, Msg2, Opts)
%%%         Returns the current slot for a process.
%%%     #{ method: GET, path: <<"/schedule">> } -> get_schedule(Msg1, Msg2, Opts)
%%%         Returns the schedule for a process in a cursor-traversable format.
%%%     #{ method: POST, path: <<"/schedule">> } -> post_schedule(Msg1, Msg2, Opts)
%%%         Schedules a new message for a process.

-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

%% @doc Helper to ensure that the environment is started.
start() ->
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
                set,
                status,
                next,
                schedule,
                slot,
                init,
                end_of_schedule,
                checkpoint
            ]
    }.

%%% Default Converge handlers.

set(Msg1, Msg2, Opts) ->
    ?event({scheduler_set_called, {msg2, Msg2}}),
    dev_message:set(Msg1, Msg2, Opts).

keys(Msg) ->
    ?event({scheduler_keys_called, {msg, Msg}}),
    dev_message:keys(Msg).

%% @doc Load the schedule for a process into the cache, then return the next
%% assignment. Assumes that Msg1 is a `dev_process` or similar message, having
%% a `Current-Slot` key. It stores a local cache of the schedule in the
%% `priv/To-Process` key.
next(Msg1, Msg2, Opts) ->
    ?event({scheduler_next_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    Schedule =
        hb_private:get(
            <<"priv/Scheduler/Assignments">>,
            Msg1,
            Opts
        ),
    LastProcessed = hb_converge:get(<<"Current-Slot">>, Msg1, Opts),
    ?event({local_schedule_cache, {schedule, Schedule}}),
    Assignments =
        case Schedule of
            X when is_map(X) and map_size(X) > 0 -> X;
            _ ->
                {ok, RecvdAssignments} =
                    hb_converge:resolve(
                        Msg1,
                        #{
                            <<"Method">> => <<"GET">>,
                            path => <<"Schedule/Assignments">>,
                            <<"From">> => LastProcessed
                        },
                        Opts
                    ),
                ?event({next_assignments, {assignments, RecvdAssignments}}),
                RecvdAssignments
        end,
    ?event({assignments, Assignments}),
    ValidKeys =
        lists:filter(
            fun(Slot) ->
                try 
                    binary_to_integer(Slot) >= LastProcessed
                catch
                    _:_ -> false
                end
            end,
            maps:keys(Assignments)
        ),
    % Remove assignments that are below the last processed slot.
    FilteredAssignments = maps:with(ValidKeys, Assignments),
    ?event({filtered_assignments, FilteredAssignments}),
    NextSlot = lists:min([ binary_to_integer(Slot) || Slot <- ValidKeys ]),
    ?event({next_slot, NextSlot}),
    NextMessage =
        hb_converge:get(
            integer_to_binary(NextSlot),
            FilteredAssignments,
            Opts
        ),
    ?event({next_message, NextMessage}),
    StateCache =
        hb_private:set(
            Msg1,
            <<"Schedule/Assignments">>,
            hb_converge:remove(FilteredAssignments, NextSlot),
            Opts
        ),
    NextState =
        hb_converge:set(StateCache, <<"Current-Slot">>, LastProcessed+1, Opts),
    ?event({next_state, NextState}),
    {ok, #{ <<"Message">> => NextMessage, <<"State">> => NextState }}.

%% @doc Returns information about the entire scheduler.
status(_M1, _M2, _Opts) ->
    ?event(getting_scheduler_status),
    Wallet = dev_scheduler_registry:get_wallet(),
    {ok,
        #{
            <<"Address">> => hb_util:id(ar_wallet:to_address(Wallet)),
            <<"Processes">> =>
                lists:map(
                    fun hb_util:id/1,
                    dev_scheduler_registry:get_processes()
                ),
            <<"Cache-Control">> => <<"no-store">>
        }
    }.

%% @doc A router for choosing between getting the existing schedule, or
%% scheduling a new message.
schedule(Msg1, Msg2, Opts) ->
    ?event({resolving_schedule_request, {msg2, Msg2}, {state_msg, Msg1}}),
    case hb_converge:get(<<"Method">>, Msg2, <<"GET">>, Opts) of
        <<"POST">> -> post_schedule(Msg1, Msg2, Opts);
        <<"GET">> -> get_schedule(Msg1, Msg2, Opts)
    end.

%% @doc Schedules a new message on the SU.
post_schedule(Msg1, Msg2, Opts) ->
    ?event(scheduling_message),
    ToSched = hb_converge:get(message, Msg2, Opts#{ hashpath => ignore }),
    ToSchedID = hb_converge:get(id, ToSched),
    Proc = hb_converge:get(process, Msg1, Opts#{ hashpath => ignore }),
    ?no_prod("Once we have GQL, get the scheduler location record. "
        "For now, we'll just use the address of the wallet."),
    SchedulerLocation =
        hb_converge:get(<<"Process/Scheduler-Location">>, Msg1, Opts#{ hashpath => ignore }),
    ProcID = hb_converge:get(id, Proc),
    PID = dev_scheduler_registry:find(ProcID, true),
    #{ wallet := Wallet } = dev_scheduler_server:info(PID),
    WalletAddress = hb_util:id(ar_wallet:to_address(Wallet)),
    ?event(
        {post_schedule,
            {process_id, ProcID},
            {process, Proc},
            {message_id, ToSchedID},
            {message, ToSched}
        }
    ),
    ?no_prod("SU does not validate item before writing into stream."),
    %case {ar_bundles:verify_item(ToSched), hb_converge:get(type, ToSched)} of
    case {WalletAddress == SchedulerLocation, true, hb_converge:get(type, ToSched)} of
        {false, _, _} ->
            {ok,
                #{
                    <<"Status">> => <<"Failed">>,
                    <<"Body">> => <<"Scheduler location does not match wallet address.">>
                }
            };
        {true, false, _} ->
            {ok,
                #{
                    <<"Status">> => <<"Failed">>,
                    <<"Body">> => <<"Data item is not valid.">>
                }
            };
        {true, true, <<"Process">>} ->
            ?no_prod("SU does not write to cache or upload to bundler."),
            hb_cache:write(ToSched, Opts),
            hb_client:upload(ToSched),
            ?event(
                {registering_new_process,
                    {proc_id, ProcID},
                    {pid, PID},
                    {is_alive, is_process_alive(PID)}
                }
            ),
            {ok,
                #{
                    <<"Status">> => <<"OK">>,
                    <<"Initial-Assignment">> => <<"0">>,
                    <<"Process">> => ProcID
                }
            };
        {true, true, _} ->
            % If Message2 is not a process, use the ID of Message1 as the PID
            {ok,
                dev_scheduler_server:schedule(
                    dev_scheduler_registry:find(ProcID, true),
                    ToSched
                )
            }
    end.

%% @doc Returns information about the current slot for a process.
slot(M1, _M2, Opts) ->
    ?event({getting_current_slot, {msg, M1}}),
    Proc = hb_converge:get(
        process,
        {as, dev_message, M1},
        Opts#{ hashpath => ignore }
    ),
    ProcID = hb_converge:get(id, Proc),
    ?event({getting_current_slot, {proc_id, ProcID}, {process, Proc}}),
    {Timestamp, Hash, Height} = ar_timestamp:get(),
    #{ current := CurrentSlot, wallet := Wallet } =
        dev_scheduler_server:info(
            dev_scheduler_registry:find(ProcID)
        ),
    {ok, #{
        <<"Process">> => ProcID,
        <<"Current-Slot">> => CurrentSlot,
        <<"Timestamp">> => Timestamp,
        <<"Block-Height">> => Height,
        <<"Block-Hash">> => Hash,
        <<"Cache-Control">> => <<"no-store">>,
        <<"Wallet-Address">> => hb_util:human_id(ar_wallet:to_address(Wallet))
    }}.

get_schedule(Msg1, Msg2, Opts) ->
    Proc = hb_converge:get(
        process,
        {as, dev_message, Msg1},
        Opts#{ hashpath => ignore }
    ),
    ProcID = hb_converge:get(id, Proc),
    From =
        case hb_converge:get(from, Msg2, not_found, Opts) of
            not_found -> 0;
            X when X < 0 -> 0;
            FromRes -> FromRes
        end,
    To =
        case hb_converge:get(to, Msg2, not_found, Opts) of
            not_found ->
                ?event({getting_current_slot, {proc_id, ProcID}}),
                maps:get(current,
                    dev_scheduler_server:info(
                        dev_scheduler_registry:find(ProcID)
                    )
                );
            ToRes -> ToRes
        end,
    gen_schedule(ProcID, From, To, Opts).

%% Private methods

gen_schedule(ProcID, From, To, Opts) ->
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    ?event(
        {servicing_request_for_assignments,
            {proc_id, ProcID},
            {from, From},
            {to, To}
        }
    ),
    {Assignments, More} = get_assignments(
        ProcID,
        From,
        To,
        Opts
    ),
    ?event({got_assignments, length(Assignments), {more, More}}),
    Bundle = #{
        <<"Type">> => <<"Schedule">>,
        <<"Process">> => ProcID,
        <<"Continues">> => atom_to_binary(More, utf8),
        <<"Timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"Block-Height">> => list_to_binary(integer_to_list(Height)),
        <<"Block-Hash">> => Hash,
        <<"Assignments">> => assignment_bundle(Assignments, Opts)
    },
    ?event(assignments_bundle_outbound),
    Signed = hb_message:sign(Bundle, hb:wallet()),
    {ok, Signed}.

%% @doc Get the assignments for a process, and whether the request was truncated.
get_assignments(ProcID, From, RequestedTo, Opts) ->
    ?event({handling_req_to_get_assignments, ProcID, From, RequestedTo}),
    ComputedTo =
        case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
            true -> RequestedTo + ?MAX_ASSIGNMENT_QUERY_LEN;
            false -> RequestedTo
        end,
    {do_get_assignments(ProcID, From, ComputedTo, Opts), ComputedTo =/= RequestedTo }.

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

assignment_bundle(Assignments, Opts) ->
    assignment_bundle(Assignments, Opts, #{}).
assignment_bundle([], Bundle, _Opts) ->
    Bundle;
assignment_bundle([Assignment | Assignments], Bundle, Opts) ->
    Slot = hb_converge:get(<<"Slot">>, Assignment, Opts#{ hashpath => ignore }),
    MessageID =
        hb_converge:get(<<"Message">>, Assignment, Opts#{ hashpath => ignore }),
    {ok, Message} = hb_cache:read(MessageID, Opts),
    ?event(
        {adding_assignment_to_bundle,
            Slot,
            {requested, MessageID},
            hb_util:id(Assignment, signed),
            hb_util:id(Assignment, unsigned)
        }
    ),
    assignment_bundle(
        Assignments,
        Bundle#{
            Slot =>
                hb_message:sign(
                    #{
                        <<"Path">> => <<"Compute">>,
                        <<"Assignment">> => Assignment,
                        <<"Message">> => Message
                    },
                    hb:wallet()
                )
        },
        Opts
    ).

%%% Compute-stack flow functions.
%%% These keys are used during the compute phase for a process to interact with
%%% the scheduler.

%% @doc Initializes the scheduler state.
init(M1, M2, Opts) ->
    update_schedule(M1, M2, Opts).

%% @doc Updates the schedule for a process.
end_of_schedule(M1, M2, Opts) -> update_schedule(M1, M2, Opts).

%% @doc Abstracted function for updating the schedule for a process the current
%% schedule is in the `/priv/Scheduler/*` private map.
update_schedule(M1, M2, Opts) ->
    Proc = hb_converge:get(process, M1, Opts),
    ProcID = hb_util:id(Proc),
    CurrentSlot =
        hb_converge:get(<<"Current-Slot">>, M1, Opts, 0),
    ToSlot =
        hb_converge:get(<<"Slot">>, M2, Opts, undefined),
    ?event({updating_schedule_current, CurrentSlot, to, ToSlot}),
    Assignments =
        hb_client:get_assignments(
            ProcID,
            CurrentSlot,
            ToSlot
        ),
    ?event({got_assignments_from_su,
        [
            {
                element(2, lists:keyfind(<<"Assignment">>, 1, A#tx.tags)),
                hb_util:id(A, signed),
                hb_util:id(A, unsigned)
            }
        ||
            A <- Assignments
        ]}),
    lists:foreach(
        fun(Assignment) ->
            ?event(
                {writing_assignment_to_cache,
                    hb_util:id(Assignment, unsigned)
                }
            ),
            hb_cache:write(Assignment, Opts)
        end,
        Assignments
    ),
    {ok, hb_private:set(M1, <<"priv/Schedule">>, Assignments, Opts)}.

%% @doc Returns the current state of the scheduler.
checkpoint(State) -> {ok, State}.

%%% Tests
%%% These tests assume that the process message has been transformed by the 
%%% dev_process, such that the full process is found in `/process`, but the
%%% scheduler is the device of the primary message.


%% @doc Generate a _transformed_ process message, not as they are generated 
%% by users. See `dev_process` for examples of AO process messages.
test_process() ->
    Wallet = hb:wallet(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    #{
        device => ?MODULE,
        process => #{
            <<"Device-Stack">> => [dev_cron, dev_wasm, dev_poda],
            <<"Image">> => <<"wasm-image-id">>,
            <<"Type">> => <<"Process">>,
            <<"Scheduler-Location">> => Address,
            <<"Test-Random-Seed">> => rand:uniform(1337)
        }
    }.

status_test() ->
    start(),
    ?assertMatch(
        #{<<"Processes">> := Processes,
            <<"Address">> := Address}
            when is_list(Processes) and is_binary(Address),
        hb_converge:get(status, test_process())
    ).

register_new_process_test() ->
    start(),
    Msg1 = test_process(),
    Proc = hb_converge:get(process, Msg1, #{ hashpath => ignore }),
    ProcID = hb_util:id(Proc),
    ?event({test_registering_new_process, {id, ProcID}, {msg, Msg1}}),
    ?assertMatch({ok, _},
        hb_converge:resolve(
            Msg1,
            #{
                <<"Method">> => <<"POST">>,
                path => <<"Schedule">>,
                <<"Message">> => Proc
            },
            #{}
        )
    ),
    ?assert(
        lists:member(
            ProcID,
            hb_converge:get(processes, hb_converge:get(status, Msg1))
        )
    ).

schedule_message_and_get_slot_test() ->
    start(),
    Msg1 = test_process(),
    Proc = hb_converge:get(process, Msg1, #{ hashpath => ignore }),
    ProcID = hb_util:id(Proc),
    Msg2 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Test-Key">> => <<"true">>
            }
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    Msg3 = #{
        path => <<"Slot">>,
        <<"Method">> => <<"GET">>,
        <<"Process">> => ProcID
    },
    ?event({pg, dev_scheduler_registry:get_processes()}),
    ?event({getting_schedule, {msg, Msg3}}),
    ?assertMatch({ok, #{ <<"Current-Slot">> := CurrentSlot }}
            when CurrentSlot > 0,
        hb_converge:resolve(Msg1, Msg3, #{})).

get_schedule_test() ->
    start(),
    Msg1 = test_process(),
    Msg2 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Test-Key">> => <<"Test-Val">>
            }
    },
    Msg3 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Test-Key">> => <<"Test-Val-2">>
            }
    },
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg1, Msg3, #{})),
    ?assertMatch(
        {ok, _},
        hb_converge:resolve(Msg1, #{
            <<"Method">> => <<"GET">>,
            path => <<"Schedule">>
        },
        #{})
    ).