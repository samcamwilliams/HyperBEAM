-module(su_process).
-export([start/1, schedule/2, get_current_slot/1]).
-export([new_proc_test/0]).
-record(state, {id, current, wallet = ar_wallet:new(), hash_chain = <<>> }).

-include("include/ar.hrl").

start(ProcID) ->
    {Current, HashChain} = su_data:get_current_slot(ProcID),
    server(#state{id = ProcID, current = Current, hash_chain = HashChain}).

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
            su:c({Class, Reason, Stack}),
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
            Assignment = ar_tx:sign(#tx {
                tags = [
                    {"Data-Protocol", "ao"},
                    {"Variant", "ao.TN.1"},
                    {"Block-Height", integer_to_list(Height)},
                    {"Block-Hash", binary_to_list(Hash)},
                    {"Block-Timestamp", integer_to_list(Timestamp)},
                    {"Epoch", "0"},
                    {"Timestamp", integer_to_list(erlang:system_time(millisecond))}, % Local time on the SU, not Arweave
                    {"Nonce", integer_to_list(NextNonce)},
                    {"Message", binary_to_list(ar_util:encode(Message#tx.id))},
                    {"Hash-Chain", binary_to_list(ar_util:encode(HashChain))}
                ]
            }, State#state.wallet),
            ok = su_data:write_assignment(State#state.id, Assignment),
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