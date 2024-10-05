-module(su_process).
-export([start/2, schedule/2, get_location/1]).
-export([get_current_slot/1, get_assignments/3]).
-export([new_proc_test/0]).
-record(state, {id, current, wallet, hash_chain = <<>> }).

-include("include/ar.hrl").
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

start(ProcID, Wallet) ->
    {Current, HashChain} = su_data:get_current_slot(ProcID),
    ao:c({starting, ProcID, Current, HashChain}),
    server(#state{id = ProcID, current = Current, hash_chain = HashChain, wallet = Wallet}).

get_location(_ProcID) ->
    ao:get(su).

schedule(ProcID, Message) when is_list(ProcID) ->
    schedule(su_registry:find(ProcID), Message);
schedule(ProcID, Message) ->
    ProcID ! {schedule, Message, self()},
    receive
        {scheduled, Message, Assignment} ->
            Assignment
    end.

get_current_slot(ProcID) ->
    ProcID ! {get_current_slot, self()},
    receive
        {current_slot, CurrentSlot} ->
            CurrentSlot
    end.

get_assignments(ProcID, From, inf) ->
    get_assignments(ProcID, From, get_current_slot(ProcID));
get_assignments(ProcID, From, RequestedTo) ->
    ComputedTo = case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
        true -> RequestedTo + ?MAX_ASSIGNMENT_QUERY_LEN;
        false -> RequestedTo
    end,
    {do_get_assignments(ProcID, From, ComputedTo), ComputedTo =/= RequestedTo }.

do_get_assignments(_ProcID, From, To) when From > To ->
    [];
do_get_assignments(ProcID, From, To) ->
    case su_data:read_assignment(ProcID, From) of
        not_found ->
            [];
        Assignment ->
            [Assignment | do_get_assignments(ProcID, From + 1, To)]
    end.

server(State) ->
    receive
        {schedule, Message, Reply} ->
            server(assign(State, Message, Reply));
        {get_current_slot, Reply} ->
            Reply ! {current_slot, State#state.current},
            server(State)
    end.

assign(State, Message, ReplyPID) ->
    try
        do_assign(State, Message, ReplyPID)
    catch
        Class:Reason:Stack ->
            {error, State}
    end.

do_assign(State, Message, ReplyPID) ->
    HashChain = next_hashchain(State#state.hash_chain, Message),
    NextNonce = State#state.current + 1,
    % Run the signing of the assignment and writes to the disk in a separate process
    spawn(
        fun() ->
            ok = su_data:write_message(Message),
            {Timestamp, Height, Hash} = su_timestamp:get(),
            Assignment = ar_bundles:sign_item(#tx {
                tags = [
                    {"Data-Protocol", "ao"},
                    {"Variant", "ao.TN.2"},
                    {"Process", State#state.id},
                    {"Epoch", "0"},
                    {"Slot", integer_to_list(NextNonce)},
                    {"Message", binary_to_list(ar_util:encode(Message#tx.id))},
                    {"Block-Height", integer_to_list(Height)},
                    {"Block-Hash", binary_to_list(Hash)},
                    {"Block-Timestamp", integer_to_list(Timestamp)},
                    {"Timestamp", integer_to_list(erlang:system_time(millisecond))}, % Local time on the SU, not Arweave
                    {"Hash-Chain", binary_to_list(ar_util:encode(HashChain))}
                ]
            }, State#state.wallet),
            ok = su_data:write_assignment(State#state.id, Assignment),
            ao_client:upload(Assignment),
            ao_client:upload(Message),
            ReplyPID ! {scheduled, Message, Assignment}
        end
    ),
    State#state{current = NextNonce, hash_chain = HashChain}.

next_hashchain(HashChain, Message) ->
    crypto:hash(sha256, << HashChain/binary, (Message#tx.id)/binary >>).

%% TESTS

new_proc_test() ->
    su_data:reset_data(),
    Wallet = ar_wallet:new(),
    SignedItem = ar_tx:sign(#tx{ data = <<"test">> }, Wallet),
    SignedItem2 = ar_tx:sign(#tx{ data = <<"test2">> }, Wallet),
    SignedItem3 = ar_tx:sign(#tx{ data = <<"test3">> }, Wallet),
    schedule("test", SignedItem),
    schedule("test", SignedItem2),
    schedule("test", SignedItem3),
    {3, _} = su_data:get_current_slot("test").