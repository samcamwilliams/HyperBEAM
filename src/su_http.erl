-module(su_http).
-export([handle/1]).
-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").
-ao_debug(print).

%%% The SU device's API functions. Enables clients to read/write messages into
%%% the schedule for a process.

handle(M) ->
    (choose_handler(M))(M).

choose_handler(M) ->
    Method = lists:keyfind(<<"Method">>, 1, M#tx.tags),
    Action = lists:keyfind(<<"Action">>, 1, M#tx.tags),
    case {Method, Action} of
        {{_, <<"GET">>}, {_, <<"Info">>}} -> fun info/1;
        {{_, <<"GET">>}, {_, <<"Slot">>}} -> fun current_slot/1;
        {{_, <<"GET">>}, {_, <<"Schedule">>}} -> fun current_schedule/1;
        {{_, <<"POST">>}, _} -> fun schedule/1
    end.

info(_M) ->
    Wallet = su_registry:get_wallet(),
    {ok,
        #tx{
            tags = [
                {<<"Unit">>, <<"Scheduler">>},
                {<<"Address">>, ar_util:id(ar_wallet:to_address(Wallet))}
            ],
            data =
                jiffy:encode(
                    lists:map(
                        fun ar_util:id/1,
                        su_registry:get_processes()
                    )
                )
            }
    }.

current_slot(M) ->
    {_, ProcID} = lists:keyfind(<<"Process">>, 1, M#tx.tags),
    {Timestamp, Hash, Height} = su_timestamp:get(),
    {ok, #tx{
        tags = [
            {<<"Process">>, ProcID},
            {<<"Current-Slot">>,
                su_process:get_current_slot(
                    su_registry:find(binary_to_list(ProcID)))
            },
            {<<"Timestamp">>, Timestamp},
            {<<"Block-Height">>, Height},
            {<<"Block-Hash">>, Hash}
        ]
    }}.

current_schedule(M) ->
    {_, ProcID} = lists:keyfind(<<"Process">>, 1, M#tx.tags),
    send_schedule(
        ao:get(store),
        ProcID,
        lists:keyfind(<<"From">>, 1, M#tx.tags),
        lists:keyfind(<<"To">>, 1, M#tx.tags)
    ).

schedule(CarrierM) ->
    ?c(scheduling_message),
    #{ <<"1">> := M } = CarrierM#tx.data,
    Store = ao:get(store),
	?no_prod("SU does not validate item before writing into stream."),
    case {ar_bundles:verify_item(M), lists:keyfind(<<"Type">>, 1, M#tx.tags)} of
        % {false, _} ->
        %     {ok,
        %         #tx{
        %             tags = [{<<"Status">>, <<"Failed">>}],
        %             data = <<"Data item is not valid.">>
        %         }
        %     };
        {_, {<<"Type">>, <<"Process">>}} ->
            ao_cache:write(Store, M),
            ao_client:upload(M),
            {ok,
                #tx{
                    tags =
                        [
                            {<<"Status">>, <<"OK">>},
                            {<<"Initial-Assignment">>, <<"0">>},
                            {<<"Process">>, ar_util:id(M, signed)}
                        ],
                    data = []
                }
            };
        {_, _} ->
            % If the process-id is not specified, use the target of the message as the process-id
            AOProcID =
                case lists:keyfind(<<"Process">>, 1, M#tx.tags) of
                    false -> binary_to_list(ar_util:id(M#tx.target));
                    {_, ProcessID} -> ProcessID
                end,
            {ok, su_process:schedule(su_registry:find(AOProcID, true), M)}
    end.

%% Private methods

% Send existing-SU GraphQL compatible results
% TODO: Refactor the type coercion here and in su_process:get_assignments.
% This is absurd.
send_schedule(Store, ProcID, false, To) ->
    send_schedule(Store, ProcID, 0, To);
send_schedule(Store, ProcID, From, false) ->
    send_schedule(Store, ProcID, From, su_process:get_current_slot(su_registry:find(ProcID)));
send_schedule(Store, ProcID, {_, From}, To) ->
    send_schedule(Store, ProcID, binary_to_integer(From), To);
send_schedule(Store, ProcID, From, {_, To}) ->
    send_schedule(Store, ProcID, From, binary_to_integer(To));
send_schedule(Store, ProcID, From, To) ->
    {Timestamp, Height, Hash} = su_timestamp:get(),
    {Assignments, More} = su_process:get_assignments(
        ProcID,
        From,
        To
    ),
    Bundle = #tx{
        tags =
            [
                {<<"Type">>, <<"Schedule">>},
                {<<"Process">>, ProcID},
                {<<"Continues">>, atom_to_binary(More, utf8)},
                {<<"Timestamp">>, list_to_binary(integer_to_list(Timestamp))},
                {<<"Block-Height">>, list_to_binary(integer_to_list(Height))},
                {<<"Block-Hash">>, Hash}
            ] ++
                case Assignments of
                    [] ->
                        [];
                    _ ->
                        {_, FromSlot} = lists:keyfind(
                            <<"Slot">>, 1, (hd(Assignments))#tx.tags
                        ),
                        {_, ToSlot} = lists:keyfind(
                            <<"Slot">>, 1, (lists:last(Assignments))#tx.tags
                        ),
                        [
                            {<<"From">>, FromSlot},
                            {<<"To">>, ToSlot}
                        ]
                end,
        data = assignments_to_bundle(Store, Assignments)
    },
    ?c(assignments_bundle_outbound),
    %ar_bundles:print(Bundle),
    SignedBundle = ar_bundles:sign_item(Bundle, ao:wallet()),
    {ok, SignedBundle}.

assignments_to_bundle(Store, Assignments) ->
    assignments_to_bundle(Store, Assignments, #{}).
assignments_to_bundle(_, [], Bundle) ->
    Bundle;
assignments_to_bundle(Store, [Assignment | Assignments], Bundle) ->
    {_, Slot} = lists:keyfind(<<"Slot">>, 1, Assignment#tx.tags),
    {_, MessageID} = lists:keyfind(<<"Message">>, 1, Assignment#tx.tags),
    Message = ao_cache:read_message(Store, MessageID),
    assignments_to_bundle(
        Store,
        Assignments,
        Bundle#{
            Slot =>
                ar_bundles:sign_item(
                    #tx{
                        tags = [
                            {<<"Assignment">>, Slot},
                            {<<"Message">>, MessageID}
                        ],
                        data = #{
                            <<"Assignment">> => Assignment,
                            <<"Message">> => Message
                        }
                    },
                    ao:wallet()
                )
        }
    ).