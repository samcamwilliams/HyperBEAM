%%% @doc A long-lived server that schedules messages for a process.
%%% It acts as a deliberate 'bottleneck' to prevent the server accidentally
%%% assigning multiple messages to the same slot.
-module(dev_scheduler_server).
-export([start/2, schedule/2, stop/1]).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Start a scheduling server for a given computation.
start(ProcID, Opts) ->
    spawn_link(
        fun() ->
            pg:join({dev_scheduler, ProcID}, self()),
            ?event({starting_scheduling_server, {proc_id, ProcID}}),
            {CurrentSlot, HashChain} = slot_from_cache(ProcID, Opts),
            ?event(
                {scheduler_got_process_info,
                    {proc_id, ProcID},
                    {current, CurrentSlot},
                    {hash_chain, HashChain}
                }
            ),
            server(
                #{
                    id => ProcID,
                    current => CurrentSlot,
                    wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts),
                    hash_chain => HashChain,
                    opts => Opts
                }
            )
        end
    ).

%% @doc Get the current slot from the cache.
slot_from_cache(ProcID, Opts) ->
    ?event({getting_assignments_from_cache, {proc_id, ProcID}, {opts, Opts}}),
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
                    <<"hash-chain">>, Assignment, #{ hashpath => ignore })
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
    receive {info, Info} -> Info end.

stop(ProcID) ->
    ?event({stopping_scheduling_server, {proc_id, ProcID}}),
    ProcID ! stop.

%% @doc The main loop of the server. Simply waits for messages to assign and
%% returns the current slot.
server(State) ->
    receive
        {schedule, Message, Reply} ->
            server(assign(State, Message, Reply));
        {info, Reply} ->
            Reply ! {info, State},
            server(State);
        stop -> ok
    end.

%% @doc Assign a message to the next slot.
assign(State, Message, ReplyPID) ->
    try
        do_assign(State, Message, ReplyPID)
    catch
        _Class:Reason:Stack ->
            ?event({error_scheduling, Reason, Stack}),
            State
    end.

%% @doc Generate and store the actual assignment message.
do_assign(State, Message, ReplyPID) ->
    HashChain = next_hashchain(maps:get(hash_chain, State), Message),
    NextSlot = maps:get(current, State) + 1,
    % Run the signing of the assignment and writes to the disk in a separate
    % process.
    AssignFun =
        fun() ->
            {Timestamp, Height, Hash} = ar_timestamp:get(),
            Assignment = hb_message:attest(#{
                <<"path">> =>
                    case hb_path:from_message(request, Message) of
                        undefined -> <<"compute">>;
                        Path -> Path
                    end,
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"process">> => hb_util:id(maps:get(id, State)),
                <<"epoch">> => <<"0">>,
                <<"slot">> => NextSlot,
                <<"block-height">> => Height,
                <<"block-hash">> => hb_util:human_id(Hash),
                <<"block-timestamp">> => Timestamp,
                % Note: Local time on the SU, not Arweave
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"hash-chain">> => hb_util:id(HashChain),
                <<"body">> => Message
            }, maps:get(wallet, State)),
            maybe_inform_recipient(
                aggressive,
                ReplyPID,
                Message,
                Assignment,
                State
            ),
            ?event(starting_message_write),
            dev_scheduler_cache:write(Assignment, maps:get(opts, State)),
            maybe_inform_recipient(
                local_confirmation,
                ReplyPID,
                Message,
                Assignment,
                State
            ),
            ?event(writes_complete),
            ?event(uploading_assignment),
            hb_client:upload(Assignment),
            ?event(uploads_complete),
            maybe_inform_recipient(
                remote_confirmation,
                ReplyPID,
                Message,
                Assignment,
                State
            )
        end,
    case hb_opts:get(scheduling_mode, sync, maps:get(opts, State)) of
        aggressive ->
            spawn(AssignFun);
        Other ->
            ?event({scheduling_mode, Other}),
            AssignFun()
    end,
    State#{
        current := NextSlot,
        hash_chain := HashChain
    }.

maybe_inform_recipient(Mode, ReplyPID, Message, Assignment, State) ->
    case hb_opts:get(scheduling_mode, remote_confirmation, maps:get(opts, State)) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.

%% @doc Create the next element in a chain of hashes that links this and prior
%% assignments.
next_hashchain(HashChain, Message) ->
    ?event({creating_next_hashchain, {hash_chain, HashChain}, {message, Message}}),
    ID = hb_message:id(Message, all),
    crypto:hash(
        sha256,
        << HashChain/binary, ID/binary >>
    ).

%% TESTS

%% @doc Test the basic functionality of the server.
new_proc_test() ->
    Wallet = ar_wallet:new(),
    SignedItem = hb_message:attest(
        #{ <<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000) },
        Wallet
    ),
    SignedItem2 = hb_message:attest(
        #{ <<"data">> => <<"test2">> },
        Wallet
    ),
    SignedItem3 = hb_message:attest(
        #{
            <<"data">> => <<"test2">>,
            <<"deep-key">> =>
                #{ <<"data">> => <<"test3">> }
        },
        Wallet
    ),
    dev_scheduler_registry:find(hb_converge:get(id, SignedItem), true),
    schedule(ID = hb_converge:get(id, SignedItem), SignedItem),
    schedule(ID, SignedItem2),
    schedule(ID, SignedItem3),
    ?assertMatch(
        #{ current := 2 },
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ).

benchmark_test() ->
    BenchTime = 1,
    Wallet = ar_wallet:new(),
    SignedItem = hb_message:attest(
        #{ <<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000) },
        Wallet
    ),
    dev_scheduler_registry:find(ID = hb_converge:get(id, SignedItem), true),
    ?event({benchmark_start, ?MODULE}),
    Iterations = hb:benchmark(
        fun(X) ->
            MsgX = #{
                path => <<"Schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    #{
                        <<"type">> => <<"Message">>,
                        <<"test-val">> => X
                    }
            },
            schedule(ID, MsgX)
        end,
        BenchTime
    ),
    hb_util:eunit_print(
        "Scheduled ~p messages in ~p seconds (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assertMatch(
        #{ current := X } when X == Iterations - 1,
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ),
    ?assert(Iterations > 30).
