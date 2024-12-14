-module(dev_scheduler_server).
-export([start/2, schedule/2]).
-export([get_current_slot/1]).
-include_lib("eunit/include/eunit.hrl").

%%% @moduledoc A long-lived server that schedules messages for a process.
%%% It acts as a deliberate 'bottleneck' to prevent the server accidentally
%%% assigning multiple messages to the same slot.

-record(state,
    {
        id,
        current,
        wallet,
        hash_chain = <<>>,
        opts = #{}
    }
).

-include("include/hb.hrl").

%% @doc Start a scheduling server for a given computation.
start(ProcID, Opts) ->
    {Current, HashChain} = slot_from_cache(ProcID, Opts),
    spawn(
        fun() ->
            ?event(
                {starting_scheduling_server,
                    {proc_id, ProcID},
                    {current, Current},
                    {hash_chain, HashChain}
                }
            ),
            server(
                #state{
                    id = ProcID,
                    current = Current,
                    hash_chain = HashChain,
                    wallet = hb_opts:get(wallet, hb:wallet(), Opts),
                    opts = Opts
                }
            )
        end
    ).

%% @doc Get the current slot from the cache.
slot_from_cache(ProcID, Opts) ->
    case dev_scheduler:assignments(ProcID, Opts) of
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
            {ok, Assignment} = hb_cache:read_assignment_message(
                hb_opts:get(store, no_viable_store, Opts),
                ProcID,
                AssignmentNum
            ),
            {
                AssignmentNum,
                hb_converge:get(
                    <<"Hash-Chain">>, Assignment, #{ hashpath => ignore })
            }
    end.


%% @doc Call the appropriate scheduling server to assign a message.
schedule(ProcID, Message) when is_binary(ProcID) ->
    schedule(dev_scheduler_registry:find(ProcID), Message);
schedule(ProcID, Message) ->
    ProcID ! {schedule, Message, self()},
    receive
        {scheduled, Message, Assignment} ->
            Assignment
    end.

%% @doc Get the current slot from the scheduling server.
get_current_slot(ProcID) ->
    ?event({getting_current_slot, {proc_id, ProcID}}),
    ProcID ! {get_current_slot, self()},
    receive
        {current_slot, CurrentSlot} ->
            CurrentSlot
    end.

%% @doc The main loop of the server. Simply waits for messages to assign and
%% returns the current slot.
server(State) ->
    receive
        {schedule, Message, Reply} ->
            server(assign(State, Message, Reply));
        {get_current_slot, Reply} ->
            Reply ! {current_slot, State#state.current},
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
    HashChain = next_hashchain(State#state.hash_chain, Message),
    NextNonce = State#state.current + 1,
    % Run the signing of the assignment and writes to the disk in a separate process
    spawn(
        fun() ->
            {Timestamp, Height, Hash} = ar_timestamp:get(),
            Assignment = hb_message:sign(#{
                <<"Data-Protocol">> => <<"ao">>,
                <<"Variant">> => <<"ao.TN.2">>,
                <<"Process">> => hb_util:id(State#state.id),
                <<"Epoch">> => <<"0">>,
                <<"Slot">> => NextNonce,
                % This was causing an error during tag encoding,
                % due to badarg on byte_length. Not sure that accessing
                % Message as a record (like process id from State above)
                % is the correct solution.
                <<"Message">> => hb_converge:get(id, Message),
                <<"Block-Height">> => Height,
                <<"Block-Hash">> => Hash,
                <<"Block-Timestamp">> => Timestamp,
                % Local time on the SU, not Arweave
                <<"Timestamp">> => erlang:system_time(millisecond),
                <<"Hash-Chain">> => hb_util:id(HashChain)
            }, State#state.wallet),
            maybe_inform_recipient(aggressive, ReplyPID, Message, Assignment),
            ?event(starting_message_write),
            dev_scheduler:write_assignment(Assignment, State#state.opts),
            hb_cache:write(Message, State#state.opts),
            maybe_inform_recipient(
                local_confirmation,
                ReplyPID,
                Message,
                Assignment
            ),
            hb_client:upload(Assignment),
            hb_client:upload(Message),
            maybe_inform_recipient(
                remote_confirmation,
                ReplyPID,
                Message,
                Assignment
            )
        end
    ),
    State#state{current = NextNonce, hash_chain = HashChain}.

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
    application:ensure_all_started(hb),
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
    ?assertEqual(2, dev_scheduler_server:get_current_slot(
        dev_scheduler_registry:find(ID))).