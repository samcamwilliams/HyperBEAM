-module(dev_meta).
-export([execute/1]).
-include("include/ao.hrl").

%%% The hyperbeam meta device, which is the default entry point
%%% for all messages on the HTTP API. This device executes a 'path' of
%%% functions upon a message, sequentially, returning the message resulting
%%% from the last function.

%% @doc Execute a message on hyperbeam. Also takes a tuple for internal use
%% of the form `{Mods, Msg, Path}`, where `Mods` is a list of device modules,
%% `Msg` is a message to execute, and `Path` is a list of function names to
%% execute on the message. In general, you probably should not do that. Use
%% the normal cu_device flow instead.
execute(CarrierMsg) when is_record(CarrierMsg, tx) ->
	execute(parse_carrier_msg(CarrierMsg));
execute({Mods, Msg, Path}) when is_list(Mods) ->
	Stack = dev_stack:create(Mods),
	?c({executing_stack, Stack, Path}),
	execute_path({dev_stack, execute},
		#{ devices => Stack, message => Msg },
		Path
	);
execute({Mod, Msg, Path}) ->
	?c({executing_device, Mod, Path}),
	execute_path(Mod, Msg, Path).

execute_path(_, M, []) -> {ok, M};
execute_path(Dev, M, [FuncName|Path]) ->
	?c({meta_executing_on_path, {device, Dev}, {function, FuncName}, {path, Path}}),
	Func = parse_path_to_func(FuncName),
	{ok, NewM} = cu_device:call(Dev, Func, [M], #{ error_strategy => throw }),
	execute_path(Dev, NewM, Path).

parse_path_to_func(BinName) when is_binary(BinName) ->
	binary_to_existing_atom(BinName, utf8);
parse_path_to_func(AtomName) when is_atom(AtomName) ->
	AtomName.

%% @doc Resolve the carrier message to an executable message, either by extracting
%% from its body or reading from its referenced ID.
parse_carrier_msg(CarrierMsg) ->
    case lists:keyfind(<<"Device">>, 1, CarrierMsg#tx.tags) of
        {_, DevMod} ->
            % If the carrier message itself contains a device, we execute
            % the path on that device.
            {
                binary_to_existing_atom(DevMod, utf8),
                extract_path_components(CarrierMsg),
                CarrierMsg
            };
        false ->
            % Otherwise, we try to parse the device from the path + body.
            do_parse_carrier_msg(CarrierMsg)
    end.

do_parse_carrier_msg(CarrierMsg) ->
    AllParts = [Start|_] = extract_path_components(CarrierMsg),
    case parse_path(AllParts) of
        {ID, ExecPath} when is_binary(ID) ->
            % When the first part of the path is an ID, we read the message
            % from the caches and apply the rest of the path to it.
            {ao_cache:read_message(ao:get(store), Start), CarrierMsg, ExecPath};
        {Dev, ExecPath} when is_atom(Dev) or is_list(Dev) ->
            % If the carrier path contains a device, we use that as the
            % root.
            {Dev, CarrierMsg, ExecPath};
        {undefined, Path} ->
            % If the device is not specified in the path, we simply execute
            % the path on the carrier message itself.
            {
                cu_device_loader:from_message(CarrierMsg),
                CarrierMsg,
                Path
            }
    end.

%% @doc Extract the components of the path from the carrier message.
extract_path_components(CarrierMsg) ->
    {_, Path} = lists:keyfind(<<"Path">>, 1, CarrierMsg#tx.tags),
    lists:filter(fun(Part) -> byte_size(Part) > 0 end,
        binary:split(Path, <<"/">>, [global])).

parse_path([ID|Path]) when is_binary(ID) andalso byte_size(ID) == 43 ->
    {ID, Path};
parse_path(All = [Start|Rest]) ->
    case lists:keyfind(Start, 1, ao:get(default_device_stacks)) of
        {_, {DefaultCall, Dev}} ->
            case Rest of
                [] -> {Dev, [DefaultCall]};
                _ -> {Dev, Rest}
            end;
        false ->
            {undefined, All}
    end.