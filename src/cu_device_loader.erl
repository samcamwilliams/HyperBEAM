-module(cu_device_loader).
-export([from_id/1, from_id/2]).

%%% A module for handling the loading and execution of device modules inside
%%% the Erlang environments.

from_id(ID) -> from_id(ID, #{}).
from_id(ID, Opts) when is_atom(ID) ->
    try ID:module_info() -> {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
from_id(ID, Opts) when is_binary(ID) when byte_size(43) ->
    case lists:member(ID, ao:get(loadable_devices)) of
        true ->
            Msg = ao_client:download(ID),
            RelBin = erlang:system_info(otp_release),
            case lists:keyfind(<<"Content-Type">>, 1, Msg#tx.tags) of
                <<"BEAM/", RelBin/bitstring>> ->
                    {_, ModNameBin} = lists:keyfind(<<"Module-Name">>, 1, Msg#tx.tags),
                    ModName = list_to_atom(binary_to_list(ModNameBin)),
                    case erlang:load_module(ModName, Msg#tx.data) of
                        {module, _} -> {ok, ModName};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        false -> {error, module_not_admissable}
    end;
from_id(ID, Opts) ->
    case maps:get(ID, ao:get(preloaded_devices), unsupported) of
        unsupported -> {error, module_not_admissable}
        Mod -> {ok, Mod}
    end.