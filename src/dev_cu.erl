-module(dev_cu).
-export([push/2, execute/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/ao.hrl").
-ao_debug(print).

push(CarrierMsg, S = #{ assignment := Assignment, logger := _Logger }) ->
    Msg = ar_bundles:hd(CarrierMsg),
    case ao_client:compute(Assignment, Msg) of
        {ok, Results} ->
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
                ?c({executing, ProcID, Assignment#tx.id}),
                cu_process:result(ProcID, Assignment#tx.id, Store, Wallet);
            _ ->
                case lists:keyfind(<<"Process">>, 1, CarrierMsg#tx.tags) of
                    {_, Process} ->
                        {_, Slot} = lists:keyfind(<<"Slot">>, 1, CarrierMsg#tx.tags),
                        cu_process:result(Process, Slot, Store, Wallet);
                    false ->
                        {error, no_viable_computation}
                end
        end,
    {ok, S#{ results => Results }}.


parse_slot(undefined) -> undefined;
parse_slot(<<>>) -> undefined;
parse_slot(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 -> Bin;
parse_slot(Slot) -> list_to_integer(binary_to_list(Slot)).
