%%% @doc A long-lived server that schedules messages for a process.
%%% It acts as a deliberate 'bottleneck' to prevent the server accidentally
%%% assigning multiple messages to the same slot.
-module(dev_scheduler_server).
-export([start/2, schedule/2]).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").

%% @doc Start a scheduling server for a given computation.
start(ProcID, Opts) ->
    {CurrentSlot, HashChain} = slot_from_cache(ProcID, Opts),
    spawn(
        fun() ->
            ?event(
                {starting_scheduling_server,
                    {proc_id, ProcID},
                    {current, CurrentSlot},
                    {hash_chain, HashChain}
                }
            ),
            server(
                #{
                    id => ProcID,
                    current => CurrentSlot,
                    wallet => hb_opts:get(wallet, hb:wallet(), Opts),
                    hash_chain => HashChain,
                    opts => Opts
                }
            )
        end
    ).

%% @doc Get the current slot from the cache.
slot_from_cache(ProcID, Opts) ->
    case dev_scheduler_cache:list(ProcID, Opts) of
        [] ->
            ?event({no_assignments_in_cache, {proc_id, ProcID}}),
            {-1, <<>>};
        Assignments ->
            AssignmentNum = lists:max(Assignments),
            ?event(
                {found_assignment_from_cache,
                    {proc_id, ProcID},
                    {assignment_num, AssignmentNum}
                }
            ),
            {ok, Assignment} = dev_scheduler_cache:read(
                ProcID,
                AssignmentNum,
                Opts
            ),
            {
                AssignmentNum,
                hb_converge:get(
                    <<"Hash-Chain">>, Assignment, #{ hashpath => ignore })
            }
    end.

%% @doc Call the appropriate scheduling server to assign a message.
schedule(AOProcID, Message) when is_binary(AOProcID) ->
    schedule(dev_scheduler_registry:find(AOProcID), Message);
schedule(ErlangProcID, Message) ->
    ErlangProcID ! {schedule, Message, self()},
    receive
        {scheduled, Message, Assignment} ->
            Assignment
    end.

%% @doc Get the current slot from the scheduling server.
info(ProcID) ->
    ?event({getting_info, {proc_id, ProcID}}),
    ProcID ! {info, self()},
    receive
        {info, Info} ->
            Info
    end.

%% @doc The main loop of the server. Simply waits for messages to assign and
%% returns the current slot.
server(State) ->
    receive
        {schedule, Message, Reply} ->
            server(assign(State, Message, Reply));
        {info, Reply} ->
            Reply ! {info, State},
            server(State)
    end.

%% @doc Assign a message to the next slot.
assign(State, Message, ReplyPID) ->
    try
        do_assign(State, Message, ReplyPID)
    catch
        _Class:Reason:Stack ->
            ?event({error_scheduling, Reason, Stack}),
            {error, State}
    end.

%% @doc Generate and store the actual assignment message.
do_assign(State, Message, ReplyPID) ->
    ?event(
        {assigning_message,
            {id, hb_converge:get(id, Message)},
            {message, Message}
        }
    ),
    HashChain = next_hashchain(maps:get(hash_chain, State), Message),
    NextSlot = maps:get(current, State) + 1,
    % Run the signing of the assignment and writes to the disk in a separate process
    spawn(
        fun() ->
            {Timestamp, Height, Hash} = ar_timestamp:get(),
            Assignment = hb_message:sign(#{
                <<"Data-Protocol">> => <<"ao">>,
                <<"Variant">> => <<"ao.TN.2">>,
                <<"Process">> => hb_util:id(maps:get(id, State)),
                <<"Epoch">> => <<"0">>,
                <<"Slot">> => NextSlot,
                <<"Message">> => hb_converge:get(id, Message),
                <<"Block-Height">> => Height,
                <<"Block-Hash">> => Hash,
                <<"Block-Timestamp">> => Timestamp,
                % Note: Local time on the SU, not Arweave
                <<"Timestamp">> => erlang:system_time(millisecond),
                <<"Hash-Chain">> => hb_util:id(HashChain)
            }, maps:get(wallet, State)),
            maybe_inform_recipient(aggressive, ReplyPID, Message, Assignment),
            ?event(starting_message_write),
            dev_scheduler_cache:write(Assignment, maps:get(opts, State)),
            hb_cache:write(Message, maps:get(opts, State)),
            maybe_inform_recipient(
                local_confirmation,
                ReplyPID,
                Message,
                Assignment
            ),
            ?event(writes_complete),
            ?event(uploading_assignment),
            hb_client:upload(Assignment),
            ?event(uploading_message),
            hb_client:upload(Message),
            ?event(uploads_complete),
            maybe_inform_recipient(
                remote_confirmation,
                ReplyPID,
                Message,
                Assignment
            )
        end
    ),
    State#{
        current := NextSlot,
        hash_chain := HashChain
    }.

maybe_inform_recipient(Mode, ReplyPID, Message, Assignment) ->
    case hb_opts:get(scheduling_mode, remote_confirmation) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.

%% @doc Create the next element in a chain of hashes that links this and prior
%% assignments.
next_hashchain(HashChain, Message) ->
    crypto:hash(
        sha256,
        << HashChain/binary, (hb_util:id(Message, signed))/binary >>
    ).

%% TESTS

%% @doc Test the basic functionality of the server.
new_proc_test() ->
    Wallet = ar_wallet:new(),
    SignedItem = hb_message:sign(
        #{ <<"Data">> => <<"test">>, <<"Random-Key">> => rand:uniform(10000) },
        Wallet
    ),
    SignedItem2 = hb_message:sign(
        #{ <<"Data">> => <<"test2">> },
        Wallet
    ),
    SignedItem3 = hb_message:sign(#{ <<"Data">> => <<"test3">> }, Wallet),
    dev_scheduler_registry:find(hb_converge:get(id, SignedItem), true),
    schedule(ID = hb_converge:get(id, SignedItem), SignedItem),
    schedule(ID, SignedItem2),
    schedule(ID, SignedItem3),
    ?assertMatch(
        #{ current := 2 },
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ).

benchmark_test() ->
    BenchTime = 2500,
    Wallet = ar_wallet:new(),
    SignedItem = hb_message:sign(
        #{ <<"Data">> => <<"test">>, <<"Random-Key">> => rand:uniform(10000) },
        Wallet
    ),
    dev_scheduler_registry:find(ID = hb_converge:get(id, SignedItem), true),
    ?event({benchmark_start, ?MODULE}),
    Iterations = hb:benchmark(
        fun(X) ->
            MsgX = #{
                path => <<"Schedule">>,
                <<"Method">> => <<"POST">>,
                <<"Message">> =>
                    #{
                        <<"Type">> => <<"Message">>,
                        <<"Test-Val">> => X
                    }
            },
            schedule(ID, MsgX)
        end,
        BenchTime
    ),
    ?assert(Iterations > 150),
    hb_util:eunit_print(
        "Scheduled ~p messages in ~p ms (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / (BenchTime / 1000)]
    ),
    ?assertMatch(
        #{ current := X } when X == Iterations - 1,
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ).