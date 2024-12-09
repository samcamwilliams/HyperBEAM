-module(dev_scheduler).
%%% Local scheduling functions:
-export([schedule/3]).
%%% CU-flow functions:
-export([info/0, slot/3, status/3]).
-export([init/3, end_of_schedule/1, checkpoint/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A simple scheduler scheme for AO.
%%% This device expects a message of the form:
%%%     Process: #{ id, Scheduler: #{ Authority } }
%%% 
%%% It exposes the following keys for scheduling:
%%%     info: Returns information about the scheduler.
%%%     #{ method: GET, path: <<"/">> } -> get_info()
%%%     #{ method: POST, path: <<"/">> } -> post_schedule(Msg1, Msg2, Opts)
%%% 

%% @doc This device uses a default_handler to route requests to the correct
%% function.
info() -> 
    #{
        exports => [status, schedule, slot, init, end_of_schedule, checkpoint]
    }.

%% @doc Returns information about the entire scheduler.
status(_M1, _M2, _Opts) ->
    Wallet = dev_scheduler_registry:get_wallet(),
    {ok,
        #{
            <<"Address">> => hb_util:id(ar_wallet:to_address(Wallet)),
            <<"Processes">> =>
                lists:map(
                    fun hb_util:id/1,
                    dev_scheduler_registry:get_processes()
                )
        }
    }.

%% @doc A router for choosing between getting the existing schedule, or
%% scheduling a new message.
schedule(Msg1, Msg2, Opts) ->
    case hb_pam:get(<<"Method">>, Msg2) of
        <<"POST">> -> post_schedule(Msg1, Msg2, Opts);
        <<"GET">> -> get_schedule(Msg1, Msg2, Opts)
    end.

%% @doc Schedules a new message on the SU.
post_schedule(Msg1, Msg2, Opts) ->
    ?event(scheduling_message),
    ToSched = hb_pam:get(message, Msg2, Opts#{ hashpath => ignore }),
    ToSchedID = hb_pam:get(id, ToSched),
    Proc = hb_pam:get(process, Msg1, Opts#{ hashpath => ignore }),
    ProcID = hb_pam:get(id, Proc),
    ?event(
        {post_schedule,
            {process_id, ProcID},
            {process, Proc},
            {message_id, ToSchedID},
            {message, ToSched}
        }
    ),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?no_prod("SU does not validate item before writing into stream."),
    %case {ar_bundles:verify_item(ToSched), hb_pam:get(type, ToSched)} of
    case {true, hb_pam:get(type, ToSched)} of
        {false, _} ->
            {ok,
                #{
                    <<"Status">> => <<"Failed">>,
                    <<"Body">> => <<"Data item is not valid.">>
                }
            };
        {_, <<"Process">>} ->
            ?no_prod("SU does not write to cache or upload to bundler."),
            hb_cache:write(Store, ToSched),
            hb_client:upload(ToSched),
            PID = dev_scheduler_registry:find(ProcID, true),
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
        {_, _} ->
            % If Message2 is not a process, use the ID of Message1 as the PID
            {ok,
                dev_scheduler_server:schedule(
                    dev_scheduler_registry:find(ProcID, true),
                    ToSched
                )
            }
    end.

%% @doc Returns information about the current slot for a process.
slot(M1, _M2, _Opts) ->
    ?event({getting_current_slot, {msg, M1}}),
    Proc = hb_pam:get_as(dev_message, process, M1, #{ hashpath => ignore }),
    ProcID = hb_pam:get(id, Proc),
    ?event({getting_current_slot, {proc_id, ProcID}, {process, Proc}}),
    {Timestamp, Hash, Height} = ar_timestamp:get(),
    {ok, #{
        <<"Process">> => ProcID,
        <<"Current-Slot">> =>
            dev_scheduler_server:get_current_slot(
                dev_scheduler_registry:find(ProcID)),
        <<"Timestamp">> => Timestamp,
        <<"Block-Height">> => Height,
        <<"Block-Hash">> => Hash,
        <<"Cache-Control">> => <<"no-store">>
    }}.

get_schedule(Msg1, Msg2, Opts) ->
    Proc = hb_pam:get_as(dev_message, process, Msg1, #{ hashpath => ignore }),
    ProcID = hb_pam:get(id, Proc),
    From =
        case hb_pam:get_default(from, Msg2, not_found, Opts) of
            not_found -> 0;
            FromRes -> FromRes
        end,
    To =
        case hb_pam:get_default(to, Msg2, not_found, Opts) of
            not_found ->
                ?event({getting_current_slot, {proc_id, ProcID}}),
                dev_scheduler_server:get_current_slot(
                    dev_scheduler_registry:find(ProcID)
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
    {Assignments, More} = dev_scheduler_server:get_assignments(
        ProcID,
        From,
        To
    ),
    ?event({got_assignments, {first, hd(Assignments)}, {more, More}}),
    Bundle = #{
        <<"Type">> => <<"Schedule">>,
        <<"Process">> => ProcID,
        <<"Continues">> => atom_to_binary(More, utf8),
        <<"Timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"Block-Height">> => list_to_binary(integer_to_list(Height)),
        <<"Block-Hash">> => Hash,
        <<"Assignments">> => assignments_message(Assignments, Opts)
    },
    ?event(assignments_bundle_outbound),
    Signed = hb_message:sign(Bundle, hb:wallet()),
    {ok, Signed}.

assignments_message(Assignments, Opts) ->
    assignments_message(Assignments, Opts, #{}).
assignments_message([], Bundle, _Opts) ->
    Bundle;
assignments_message([Assignment | Assignments], Bundle, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    Slot = hb_pam:get(<<"Slot">>, Assignment, Opts#{ hashpath => ignore }),
    MessageID = hb_pam:get(<<"Message">>, Assignment, Opts#{ hashpath => ignore }),
    {ok, MessageTX} = hb_cache:read_message(Store, MessageID),
    Message = hb_message:tx_to_message(MessageTX),
    ?event(
        {adding_assignment_to_bundle,
            Slot,
            {requested, MessageID},
            hb_util:id(Assignment, signed),
            hb_util:id(Assignment, unsigned)
        }
    ),
    assignments_message(
        Assignments,
        Bundle#{
            Slot =>
                hb_message:sign(
                    #{
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
    {ok, M3} = hb_pam:resolve(
        hb_pam:set(M1, #{ device => dev_message }),
        M2,
        Opts
    ),
    update_schedule(M3#{su_location => M2}).

%% @doc Updates the schedule for a process.
end_of_schedule(State) -> update_schedule(State).

%% @doc Abstracted function for updating the schedule for a process.
update_schedule(State = #{ process := Proc }) ->
    Store = maps:get(store, State, hb_opts:get(store)),
    CurrentSlot = maps:get(slot, State, 0),
    ToSlot = maps:get(to, State),
    ?event({updating_schedule_current, CurrentSlot, to, ToSlot}),
    Assignments = hb_client:get_assignments(
        hb_util:id(Proc, signed),
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
        || A <- Assignments ]}),
    lists:foreach(
        fun(Assignment) ->
            ?event(
                {writing_assignment_to_cache,
                    hb_util:id(Assignment, unsigned)
                }
            ),
            hb_cache:write(Store, Assignment)
        end,
        Assignments
    ),
    State#{schedule => Assignments}.

%% @doc Returns the current state of the scheduler.
checkpoint(State) -> {ok, State}.

%%% Tests
%%% These tests assume that the process message has been transformed by the 
%%% dev_process, such that the full process is found in `/process`, but the
%%% scheduler is the device of the primary message.

%% @doc Helper to ensure that the environment is started before running tests.
init() ->
    application:ensure_all_started(hb),
    ok.

test_process() ->
    #{
        device => ?MODULE,
        process => #{
            <<"Device-Stack">> => [dev_cron, dev_wasm, dev_poda],
            <<"Image">> => <<"wasm-image-id">>,
            <<"Type">> => <<"Process">>,
            <<"Exciting-Random-Number">> => rand:uniform(1337)
        }
    }.

status_test() ->
    init(),
    ?assertMatch(
        #{<<"Processes">> := Processes,
            <<"Address">> := Address}
            when is_list(Processes) and is_binary(Address),
        hb_pam:get(status, test_process())
    ).

register_new_process_test() ->
    init(),
    Msg1 = test_process(),
    Proc = hb_pam:get(process, Msg1, #{ hashpath => ignore }),
    ProcID = hb_util:id(Proc),
    ?event({test_registering_new_process, {id, ProcID}, {msg, Msg1}}),
    ?assertMatch({ok, _},
        hb_pam:resolve(
            Msg1,
            #{
                <<"Method">> => <<"POST">>,
                path => <<"Schedule">>,
                <<"Message">> => Proc
            }
        )
    ),
    ?assertEqual([ProcID], hb_pam:get(processes, hb_pam:get(status, Msg1))).

schedule_message_and_get_slot_test() ->
    init(),
    Msg1 = test_process(),
    Proc = hb_pam:get(process, Msg1, #{ hashpath => ignore }),
    ProcID = hb_util:id(Proc),
    Msg2 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Exciting">> => <<"true">>
            }
    },
    ?assertMatch({ok, _}, hb_pam:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_pam:resolve(Msg1, Msg2, #{})),
    Msg3 = #{
        path => <<"Slot">>,
        <<"Method">> => <<"GET">>,
        <<"Process">> => ProcID
    },
    ?event({pg, dev_scheduler_registry:get_processes()}),
    ?event({getting_schedule, {msg, Msg3}}),
    ?assertMatch({ok, #{ <<"Current-Slot">> := CurrentSlot }}
            when CurrentSlot == 1,
        hb_pam:resolve(Msg1, Msg3, #{})).

get_schedule_test() ->
    init(),
    Msg1 = test_process(),
    Msg2 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Exciting">> => <<"Reasonably">>
            }
    },
    Msg3 = #{
        path => <<"Schedule">>,
        <<"Method">> => <<"POST">>,
        <<"Message">> =>
            #{
                <<"Type">> => <<"Message">>,
                <<"Exciting">> => <<"Getting old.">>
            }
    },
    ?assertMatch({ok, _}, hb_pam:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_pam:resolve(Msg1, Msg3, #{})),
    ?assertMatch(
        {ok, _},
        ?event(hb_pam:resolve(Msg1, #{
            <<"Method">> => <<"GET">>,
            path => <<"Schedule">>
        }))
    ).
