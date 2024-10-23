-module(dev_meta).
-export([execute/1]).
-include("include/ao.hrl").

%%% The hyperbeam HTTP API meta device, which is the default entry point
%%% for all messages that do not otherwise specify a device.
execute(M) ->
    ?c({meta_execute, M}),
    {Dev, ModifiedM} = find_device(M),
    cu_device:call(Dev, execute, [ModifiedM], #{ error_strategy => throw }).

%% @doc Locate the appropriate device module to execute for the given message.
%% If the message specifies a device for itself, use that. Otherwise, look for
%% a path that might specify a device, and if one is found, use that. Else,
%% use the default device.
find_device(M) ->
    case lists:keyfind(<<"Device">>, 1, M#tx.tags) of
        {_, DeviceID} ->
            {ok, DevMod} = cu_device_loader:from_id(DeviceID),
            {DevMod, M};
        false ->
            {_, Path} = lists:keyfind(<<"Path">>, 1, M#tx.tags),
            case Path of
                <<"/data/", ID/binary>> ->
                    {dev_data, M#tx{ tags = [{id, ID}|M#tx.tags] }};
                <<"/", _/binary>> -> {cu_device_loader:default(), M}
            end
    end.