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
        store = hb_opts:get(store)
    }
).

-include("include/hb.hrl").
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

start(ProcID, Wallet) ->
    start(ProcID, Wallet, hb_opts:get(store)).
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
    case hb_cache:assignments(hb_opts:get(store), ProcID) of
        [] ->
            {-1, <<>>};
        Assignments ->
            AssignmentNum = lists:max(Assignments),
            {ok, Assignment} = hb_cache:read_assignment(hb_opts:get(store), ProcID, AssignmentNum),
            {
                AssignmentNum,
                hb_util:decode(element(2, lists:keyfind(<<"Hash-Chain">>, 1, Assignment#tx.tags)))
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
    {ok, From} = hb_cache:read_assignment(hb_opts:get(store), ProcID, From),
    {_, Slot} = lists:keyfind(<<"Slot">>, 1, From#tx.tags),
    get_assignments(ProcID, binary_to_integer(Slot), RequestedTo);
get_assignments(ProcID, From, RequestedTo) when is_binary(RequestedTo) andalso byte_size(RequestedTo) == 43 ->
    {ok, Assignment} = hb_cache:read_assignment(hb_opts:get(store), ProcID, RequestedTo),
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
    case hb_cache:read_assignment(hb_opts:get(store), ProcID, From) of
        not_found ->
            [];
        {ok, Assignment} ->
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
                    {<<"Process">>, hb_util:id(State#state.id)},
                    {<<"Epoch">>, <<"0">>},
                    {<<"Slot">>, list_to_binary(integer_to_list(NextNonce))},
                    % This was causing an error during tag encoding,
                    % due to badarg on byte_length. Not sure that accessing
                    % Message as a record (like process id from State above)
                    % is the correct solution.
                    {<<"Message">>, hb_util:id(Message, signed)},
                    {<<"Block-Height">>, list_to_binary(integer_to_list(Height))},
                    {<<"Block-Hash">>, Hash},
                    {<<"Block-Timestamp">>, list_to_binary(integer_to_list(Timestamp))},
                    {<<"Timestamp">>, list_to_binary(integer_to_list(erlang:system_time(millisecond)))}, % Local time on the SU, not Arweave
                    {<<"Hash-Chain">>, hb_util:id(HashChain)}
                ]
            }, State#state.wallet),
            maybe_inform_recipient(aggressive, ReplyPID, Message, Assignment),
            hb_cache:write_assignment(State#state.store, Assignment),
            ?event(starting_message_write),
            hb_cache:write(State#state.store, Message),
            % ?event(message_written),
            % ?event(assignment_after_write),
            % ar_bundles:print(Assignment),
            % ?event(message_after_assignment_written),
            % ar_bundles:print(Message),
            % ?event(read_from_disk),
            % ar_bundles:print(hb_cache:read(hb_store:scope(State#state.store, local), hb_util:id(Message, unsigned))),
            maybe_inform_recipient(local_confirmation, ReplyPID, Message, Assignment),
            hb_client:upload(Assignment),
            hb_client:upload(Message),
            maybe_inform_recipient(remote_confirmation, ReplyPID, Message, Assignment)
        end
    ),
    State#state{current = NextNonce, hash_chain = HashChain}.

maybe_inform_recipient(Mode, ReplyPID, Message, Assignment) ->
    case hb_opts:get(scheduling_mode, remote_confirmation) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.

next_hashchain(HashChain, Message) ->
    crypto:hash(sha256, << HashChain/binary, (hb_util:id(Message, signed))/binary >>).

%% TESTS

new_proc_test() ->
    application:ensure_all_started(hb),
    Wallet = ar_wallet:new(),
    SignedItem = ar_bundles:sign_item(#tx{ data = <<"test">> }, Wallet),
    SignedItem2 = ar_bundles:sign_item(#tx{ data = <<"test2">> }, Wallet),
    SignedItem3 = ar_bundles:sign_item(#tx{ data = <<"test3">> }, Wallet),
    dev_scheduler_registry:find(binary_to_list(hb_util:id(SignedItem, signed)), true),
    schedule(ID = binary_to_list(hb_util:id(SignedItem, signed)), SignedItem),
    schedule(ID, SignedItem2),
    schedule(ID, SignedItem3),
    ?assertEqual(2, dev_scheduler_server:get_current_slot(
        dev_scheduler_registry:find(ID))).