-module(dev_cu).
-export([push/2, execute/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/ao.hrl").
-ao_debug(print).

push(Msg, S = #{ assignment := Assignment, logger := _Logger }) ->
	?c(
		{pushing_message,
			{assignment, ar_util:id(Assignment, unsigned)},
			{message, ar_util:id(Msg, unsigned)}
		}
	),
    case ao_client:compute(Assignment, Msg) of
        {ok, Results} ->
            ?c(computed_results),
            {ok, S#{ results => Results }};
        Error ->
            throw({cu_error, Error})
    end.

execute(CarrierMsg, S) ->
    MaybeBundle = ar_bundles:hd(CarrierMsg),
    Store = ao:get(store),
    Wallet = ao:wallet(),
    {ok, Results} =
        case MaybeBundle of
            #tx{data = #{ <<"Message">> := _Msg, <<"Assignment">> := Assignment }} ->
                % TODO: Execute without needing to call the SU unnecessarily.
                {_, ProcID} = lists:keyfind(<<"Process">>, 1, Assignment#tx.tags),
				?c({dev_cu_computing_from_full_assignment, {process, ProcID}, {slot, ar_util:id(Assignment, signed)}}),
                cu_process:result(ProcID, ar_util:id(Assignment, signed), Store, Wallet);
            _ ->
                case lists:keyfind(<<"Process">>, 1, CarrierMsg#tx.tags) of
                    {_, Process} ->
                        {_, Slot} = lists:keyfind(<<"Slot">>, 1, CarrierMsg#tx.tags),
						?c({dev_cu_computing_from_slot_ref, {process, Process}, {slot, Slot}}),
						ar_bundles:print(CarrierMsg),
                        cu_process:result(Process, Slot, Store, Wallet);
                    false ->
                        {error, no_viable_computation}
                end
        end,
    {ResType, ModState = #{ results := ModResults }} =
        case lists:keyfind(<<"Attest-To">>, 1, CarrierMsg#tx.tags) of
            {_, RawAttestTo} ->
                AttestTo = ar_util:decode(RawAttestTo),
                ?c({attest_to_only_message, RawAttestTo}),
                case ar_bundles:find(AttestTo, Results) of
                    not_found ->
                        ?c(message_to_attest_to_not_found),
                        {ok,
                            S#{
                                results =>
                                    #tx {
                                        tags = [{<<"Status">>, <<"404">>}],
                                        data = <<"Requested message to attest to not in results bundle.">>
                                    }
                            }
                        };
                    _ ->
                        ?c(message_to_attest_to_found),
                        {ok, S#{
                            results => ar_bundles:sign_item(
                                #tx {
                                    tags = [
                                        {<<"Status">>, <<"200">>},
                                        {<<"Attestation-For">>, RawAttestTo}
                                    ],
                                    data = <<>>
                                },
                                ao:wallet()
                            )
                        }}
                end;
            false ->
                {ok, S#{ results => Results }}
        end,
    ?c(returning_computed_results),
	%ar_bundles:print(ModResults),
    {ResType, ModState}.


parse_slot(undefined) -> undefined;
parse_slot(<<>>) -> undefined;
parse_slot(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 -> Bin;
parse_slot(Slot) -> list_to_integer(binary_to_list(Slot)).