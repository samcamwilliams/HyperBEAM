-module(dev_cu).
-export([push/2, execute/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-hb_debug(print).

push(Msg, S = #{ assignment := Assignment, logger := _Logger }) ->
    ?event(
        {pushing_message,
            {assignment, hb_util:id(Assignment, unsigned)},
            {message, hb_util:id(Msg, unsigned)}
        }
    ),
    case hb_client:compute(Assignment, Msg) of
        {ok, Results} ->
            ?event(computed_results),
            {ok, S#{ results => Results }};
        Error ->
            throw({cu_error, Error})
    end.

execute(CarrierMsg, S) ->
    MaybeBundle = ar_bundles:hd(CarrierMsg),
    Store = hb_opts:get(store),
    Wallet = hb:wallet(),
    {ok, Results} =
        case MaybeBundle of
            #tx{data = #{ <<"Message">> := _Msg, <<"Assignment">> := Assignment }} ->
                % TODO: Execute without needing to call the SU unnecessarily.
                {_, ProcID} = lists:keyfind(<<"Process">>, 1, Assignment#tx.tags),
                ?event({dev_cu_computing_from_full_assignment, {process, ProcID}, {slot, hb_util:id(Assignment, signed)}}),
                hb_process:result(ProcID, hb_util:id(Assignment, signed), Store, Wallet);
            _ ->
                case lists:keyfind(<<"Process">>, 1, CarrierMsg#tx.tags) of
                    {_, Process} ->
                        {_, Slot} = lists:keyfind(<<"Slot">>, 1, CarrierMsg#tx.tags),
                        ?event({dev_cu_computing_from_slot_ref, {process, Process}, {slot, Slot}}),
                        hb_process:result(Process, Slot, Store, Wallet);
                    false ->
                        {error, no_viable_computation}
                end
        end,
    {ResType, ModState = #{ results := _ModResults }} =
        case lists:keyfind(<<"Attest-To">>, 1, CarrierMsg#tx.tags) of
            {_, RawAttestTo} ->
                AttestTo = hb_util:decode(RawAttestTo),
                ?event({attest_to_only_message, RawAttestTo}),
                case ar_bundles:find(AttestTo, Results) of
                    not_found ->
                        ?event(message_to_attest_to_not_found),
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
                        ?event(message_to_attest_to_found),
                        {ok, S#{
                            results => ar_bundles:sign_item(
                                #tx {
                                    tags = [
                                        {<<"Status">>, <<"200">>},
                                        {<<"Attestation-For">>, RawAttestTo}
                                    ],
                                    data = <<>>
                                },
                                hb:wallet()
                            )
                        }}
                end;
            false ->
                {ok, S#{ results => Results }}
        end,
    ?event(returning_computed_results),
    %ar_bundles:print(ModResults),
    {ResType, ModState}.