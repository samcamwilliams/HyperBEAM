-module(dev_scheduler_server).
-export([start/2, schedule/2]).
-export([get_current_slot/1, get_assignments/3]).
-include_lib("eunit/include/eunit.hrl").

-record(state,
    {
        id,
        current,
        wallet,
        hash_chain = <<>>,
        store = ao:get(store)
    }
).

-include("include/ao.hrl").
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

start(ProcID, Wallet) ->
    start(ProcID, Wallet, ao:get(store)).
start(ProcID, Wallet, Store) ->
    {Current, HashChain} = slot_from_cache(ProcID),
    spawn(
        fun() ->
            server(
                #state{
                    id = ProcID,
                    current = Current,
                    hash_chain = HashChain,
                    wallet = Wallet,
                    store = Store
                }
            )
        end
    ).

slot_from_cache(ProcID) ->
    case ao_cache:assignments(ao:get(store), ProcID) of
        [] ->
            {-1, <<>>};
        Assignments ->
            AssignmentNum = lists:max(Assignments),
            Assignment = ao_cache:read_assignment(ao:get(store), ProcID, AssignmentNum),
            {
                AssignmentNum,
                ar_util:decode(element(2, lists:keyfind(<<"Hash-Chain">>, 1, Assignment#tx.tags)))
            }
    end.

schedule(ProcID, Message) when is_list(ProcID) ->
    schedule(dev_scheduler_registry:find(ProcID), Message);
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

get_assignments(ProcID, From, undefined) ->
    get_assignments(ProcID, From, get_current_slot(ProcID));
get_assignments(ProcID, From, RequestedTo) when is_binary(From) andalso byte_size(From) == 43 ->
    From = ao_cache:read_assignment(ao:get(store), ProcID, From),
    {_, Slot} = lists:keyfind(<<"Slot">>, 1, From#tx.tags),
    get_assignments(ProcID, binary_to_integer(Slot), RequestedTo);
get_assignments(ProcID, From, RequestedTo) when is_binary(RequestedTo) andalso byte_size(RequestedTo) == 43 ->
    Assignment = ao_cache:read_assignment(ao:get(store), ProcID, RequestedTo),
    {_, Slot} = lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags),
    get_assignments(ProcID, From, binary_to_integer(Slot));
get_assignments(ProcID, From, RequestedTo) when is_binary(From) ->
    get_assignments(ProcID, binary_to_integer(From), RequestedTo);
get_assignments(ProcID, From, RequestedTo) when is_binary(RequestedTo) ->
    get_assignments(ProcID, From, binary_to_integer(RequestedTo));
get_assignments(ProcID, From, RequestedTo) ->
    ?event({handling_req_to_get_assignments, ProcID, From, RequestedTo}),
    ComputedTo = case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
        true -> RequestedTo + ?MAX_ASSIGNMENT_QUERY_LEN;
        false -> RequestedTo
    end,
    {do_get_assignments(ProcID, From, ComputedTo), ComputedTo =/= RequestedTo }.

do_get_assignments(_ProcID, From, To) when From > To ->
    [];
do_get_assignments(ProcID, From, To) ->
    case ao_cache:read_assignment(ao:get(store), ProcID, From) of
        not_found ->
            [];
        Assignment ->
            [
                Assignment
                | do_get_assignments(
                    ProcID,
                    From + 1,
                    To
                )
            ]
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
        _Class:Reason:Stack ->
            ?event({error_scheduling, Reason, Stack}),
            {error, State}
    end.

do_assign(State, Message, ReplyPID) ->
    HashChain = next_hashchain(State#state.hash_chain, Message),
    NextNonce = State#state.current + 1,
    % Run the signing of the assignment and writes to the disk in a separate process
    spawn(
        fun() ->
            {Timestamp, Height, Hash} = ar_timestamp:get(),
            Assignment = ar_bundles:sign_item(#tx {
                tags = [
                    {<<"Data-Protocol">>, <<"ao">>},
                    {<<"Variant">>, <<"ao.TN.2">>},
                    {<<"Process">>, ao_message:id(State#state.id)},
                    {<<"Epoch">>, <<"0">>},
                    {<<"Slot">>, list_to_binary(integer_to_list(NextNonce))},
                    {<<"Message">>, ao_message:id(Message, signed)},
                    {<<"Block-Height">>, list_to_binary(integer_to_list(Height))},
                    {<<"Block-Hash">>, Hash},
                    {<<"Block-Timestamp">>, list_to_binary(integer_to_list(Timestamp))},
                    {<<"Timestamp">>, list_to_binary(integer_to_list(erlang:system_time(millisecond)))}, % Local time on the SU, not Arweave
                    {<<"Hash-Chain">>, ao_message:id(HashChain)}
                ]
            }, State#state.wallet),
            maybe_inform_recipient(aggressive, ReplyPID, Message, Assignment),
            ao_cache:write_assignment(State#state.store, Assignment),
            ?event(starting_message_write),
            ao_cache:write(State#state.store, Message),
            % ?event(message_written),
            % ?event(assignment_after_write),
            % ar_bundles:print(Assignment),
            % ?event(message_after_assignment_written),
            % ar_bundles:print(Message),
            % ?event(read_from_disk),
            % ar_bundles:print(ao_cache:read(ao_store:scope(State#state.store, local), ao_message:id(Message, unsigned))),
            maybe_inform_recipient(local_confirmation, ReplyPID, Message, Assignment),
            ao_client:upload(Assignment),
            ao_client:upload(Message),
            maybe_inform_recipient(remote_confirmation, ReplyPID, Message, Assignment)
        end
    ),
    State#state{current = NextNonce, hash_chain = HashChain}.

maybe_inform_recipient(Mode, ReplyPID, Message, Assignment) ->
    case ao:get(scheduling_mode, remote_confirmation) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.

next_hashchain(HashChain, Message) ->
    crypto:hash(sha256, << HashChain/binary, (ao_message:id(Message, signed))/binary >>).

%% TESTS

new_proc() ->
    application:ensure_all_started(ao),
    su_data:reset_data(),
    Wallet = ar_wallet:new(),
    SignedItem = ar_bundles:sign_item(#tx{ data = <<"test">> }, Wallet),
    ?event(1),
    SignedItem2 = ar_bundles:sign_item(#tx{ data = <<"test2">> }, Wallet),
    ?event(2),
    SignedItem3 = ar_bundles:sign_item(#tx{ data = <<"test3">> }, Wallet),
    ?event(3),
    dev_scheduler_registry:find(binary_to_list(ao_message:id(SignedItem, signed)), true),
    ?event(4),
    schedule(ID = binary_to_list(ao_message:id(SignedItem, signed)), SignedItem),
    ?event(5),
    schedule(ID, SignedItem2),
    ?event(6),
    schedule(ID, SignedItem3),
    {2, _} = su_data:get_current_slot(ID),
    true.

new_proc_test_() ->
    {timeout, 30, ?_assert(new_proc())}.
