-module(dev_meta).
-export([execute/1]).
-include("include/ao.hrl").

%%% The hyperbeam HTTP API meta device, which is the default entry point
%%% for all messages that do not otherwise specify a device.
execute(M) ->
    ?c({meta_execute, M}),
    {Dev, ModifiedM} = find_device(M),
    S0 =
        case cu_device:call(Dev, init, [ModifiedM], #{ error_strategy => throw }) of
            {ok, InitState} -> InitState;
            no_match -> #{}
        end,
    cu_device:call(Dev, execute, [ModifiedM, S0], #{ error_strategy => throw }).

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
            case binary:split(Path, <<"/">>) of
                [<<>>, <<"data">>, ID] ->
                    {dev_lookup, M#tx{ tags = [{id, ID}|M#tx.tags] }};
                <<"/">> -> {cu_device_loader:default(), M};
                Else ->
                    match_path_to_stack(Else, M, ao:get(default_path_messages))
            end
    end.

match_path_to_stack(Path, M, PathMessages) ->
    case binary:split(Path, <<"/">>) of
        [<<>>|Rest] -> match_path_to_stack(Rest, M, PathMessages);
        [DeviceName|Rest] ->
            case lists:keyfind(DeviceName, 1, PathMessages) of
                {_, DeviceStack} ->
                    Stack = dev_stack:create(DeviceStack, M),
                    {
                        Stack,
                        M#tx{
                            tags =
                                [{<<"path">>, << "/", Rest/binary >>}|M#tx.tags]
                        }
                    };
                false -> {cu_device_loader:default(), M}
            end
    end.