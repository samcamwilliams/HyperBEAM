-module(dev_scheduler_interface).
-export([handle/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The SU device's API functions. Enables clients to read/write messages into
%%% the schedule for a process.

handle(M) ->
    (choose_handler(M))(M).

choose_handler(M) ->
    Method = hb_converge:get(<<"method">>, M),
    Action = hb_converge:get(<<"action">>, M),
    case {Method, Action} of
        {{_, <<"GET">>}, {_, <<"info">>}} -> fun info/1;
        {{_, <<"GET">>}, {_, <<"slot">>}} -> fun current_slot/1;
        {{_, <<"GET">>}, {_, <<"schedule">>}} -> fun current_schedule/1;
        {{_, <<"POST">>}, _} -> fun schedule/1
    end.

info(M) ->
    Wallet = dev_scheduler_registry:get_wallet(),
    {ok,
        #{
            <<"unit">> => <<"scheduler">>,
            <<"address">> => hb_util:id(ar_wallet:to_address(Wallet)),
            <<"data">> =>
                jiffy:encode(
                    lists:map(
                        fun hb_util:id/1,
                        dev_scheduler_registry:get_processes()
                    )
                )
        }
    }.

current_slot(M) ->
    {_, ProcID} = lists:keyfind(<<"process">>, 1, M#tx.tags),
    {Timestamp, Hash, Height} = ar_timestamp:get(),
    {ok, #tx{
        tags = [
            {<<"process">>, ProcID},
            {<<"current-slot">>,
                dev_scheduler_server:get_current_slot(
                    dev_scheduler_registry:find(ProcID)
                )
            },
            {<<"timestamp">>, Timestamp},
            {<<"block-height">>, Height},
            {<<"block-hash">>, Hash}
        ]
    }}.

current_schedule(M) ->
    {_, ProcID} = lists:keyfind(<<"process">>, 1, M#tx.tags),
    send_schedule(
        hb_opts:get(store),
        ProcID,
        lists:keyfind(<<"from">>, 1, M#tx.tags),
        lists:keyfind(<<"to">>, 1, M#tx.tags)
    ).

schedule(CarrierM) ->
    ?event(scheduling_message),
    #{ <<"1">> := M } = CarrierM#tx.data,
    %ar_bundles:print(M),
    Store = hb_opts:get(store),
    ?no_prod("SU does not validate item before writing into stream."),
    case {ar_bundles:verify_item(M), lists:keyfind(<<"type">>, 1, M#tx.tags)} of
        % {false, _} ->
        %     {ok,
        %         #tx{
        %             tags = [{<<"status">>, <<"failed">>}],
        %             data = <<"Data item is not valid.">>
        %         }
        %     };
        {_, {<<"type">>, <<"process">>}} ->
            hb_cache:write(Store, M),
            hb_client:upload(M),
            {ok,
                #tx{
                    tags =
                        [
                            {<<"status">>, 200},
                            {<<"initial-assignment">>, <<"0">>},
                            {<<"process">>, hb_util:id(M, signed)}
                        ],
                    data = []
                }
            };
        {_, _} ->
            % If the process-id is not specified, use the target of the message as the process-id
            AOProcID =
                case lists:keyfind(<<"process">>, 1, M#tx.tags) of
                    false -> binary_to_list(hb_util:id(M#tx.target));
                    {_, ProcessID} -> ProcessID
                end,
            {ok, dev_scheduler_server:schedule(dev_scheduler_registry:find(AOProcID, true), M)}
    end.

%% Private methods

send_schedule(Store, ProcID, false, To) ->
    send_schedule(Store, ProcID, 0, To);
send_schedule(Store, ProcID, From, false) ->
    send_schedule(Store, ProcID, From, dev_scheduler_server:get_current_slot(dev_scheduler_registry:find(ProcID)));
send_schedule(Store, ProcID, {<<"from">>, From}, To) ->
    send_schedule(Store, ProcID, binary_to_integer(From), To);
send_schedule(Store, ProcID, From, {<<"to">>, To}) when byte_size(To) == 43 ->
    send_schedule(Store, ProcID, From, To);
send_schedule(Store, ProcID, From, {<<"to">>, To}) ->
    send_schedule(Store, ProcID, From, binary_to_integer(To));
send_schedule(Store, ProcID, From, To) ->
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    ?event({servicing_request_for_assignments, {proc_id, ProcID}, {from, From}, {to, To}}),
    {Assignments, More} = dev_scheduler_server:get_assignments(
        ProcID,
        From,
        To
    ),
    Bundle = #tx{
        tags =
            [
                {<<"type">>, <<"schedule">>},
                {<<"process">>, ProcID},
                {<<"continues">>, atom_to_binary(More, utf8)},
                {<<"timestamp">>, list_to_binary(integer_to_list(Timestamp))},
                {<<"block-height">>, list_to_binary(integer_to_list(Height))},
                {<<"block-hash">>, Hash}
            ] ++
                case Assignments of
                    [] ->
                        [];
                    _ ->
                        {_, FromSlot} = lists:keyfind(
                            <<"slot">>, 1, (hd(Assignments))#tx.tags
                        ),
                        {_, ToSlot} = lists:keyfind(
                            <<"slot">>, 1, (lists:last(Assignments))#tx.tags
                        ),
                        [
                            {<<"from">>, FromSlot},
                            {<<"to">>, ToSlot}
                        ]
                end,
        data = assignments_to_bundle(Store, Assignments)
    },
    ?event(assignments_bundle_outbound),
    %ar_bundles:print(Bundle),
    SignedBundle = ar_bundles:sign_item(Bundle, hb:wallet()),
    {ok, SignedBundle}.

assignments_to_bundle(Store, Assignments) ->
    assignments_to_bundle(Store, Assignments, #{}).
assignments_to_bundle(_, [], Bundle) ->
    Bundle;
assignments_to_bundle(Store, [Assignment | Assignments], Bundle) ->
    {_, Slot} = lists:keyfind(<<"slot">>, 1, Assignment#tx.tags),
    {_, MessageID} = lists:keyfind(<<"message">>, 1, Assignment#tx.tags),
    {ok, Message} = hb_cache:read_message(Store, MessageID),
    ?event({adding_assignment_to_bundle, Slot, {requested, MessageID}, hb_util:id(Assignment, signed), hb_util:id(Assignment, unsigned)}),
    assignments_to_bundle(
        Store,
        Assignments,
        Bundle#{
            Slot =>
                ar_bundles:sign_item(
                    #tx{
                        tags = [
                            {<<"assignment">>, Slot},
                            {<<"message">>, MessageID}
                        ],
                        data = #{
                            <<"assignment">> => Assignment,
                            <<"message">> => Message
                        }
                    },
                    hb:wallet()
                )
        }
    ).