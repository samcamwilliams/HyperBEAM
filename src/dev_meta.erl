-module(dev_meta).
-export([execute/2]).
-include("include/ao.hrl").

%%% The hyperbeam meta device, which is the default entry point
%%% for all messages processed by the machine. This device executes a 'path' of
%%% functions upon a message, sequentially, returning the message resulting
%%% from the last function.

%%% The ideal API should support:
%%% GET /id/Execute/Report?1.Mode=Fast&2.Mode=Terse <- Edge case?
%%% GET /id/Outbox/1/Target <- Getting results from a process. Common case.
%%% GET /id/Execute/Output?Action=Transfer <- 'Normal' dry-run request.
%%% POST /id/Execute/Output?Action=Transfer <- 'Normal' push request.
%%% POST /id/Schedule <- Schedule the contained message to be executed.
%%% GET /id/Schedule <- Get the schedule of a process by its ID.
%%% POST /id/Schedule <- Send a new message to the schedule of a process.
%%% POST /id/Push <- Push a new message on a process.
%%% POST /Push <- Push a new message on the referenced message.
%%% GET /Execute <- Execute an assignment on the referenced process?
%%% GET /Thing/in/cache <- Get a thing from the cache by its path.

%% @doc Execute a message on hyperbeam. Also takes a tuple for internal use
%% of the form `{Mods, Msg, Path}`, where `Mods` is a list of device modules,
%% `Msg` is a message to execute, and `Path` is a list of function names to
%% execute on the message. In general, you probably should not do that. Use
%% the normal ao_device flow instead.
execute(CarrierMsg, S) when is_record(CarrierMsg, tx) ->
	?event({executing_carrier_msg, CarrierMsg}),
	execute(parse_carrier_msg(CarrierMsg, S), S);
execute({Mods, Msg, Path}, S) when is_list(Mods) ->
	Stack = dev_stack:create(Mods),
	?event({executing_stack, Stack, Path}),
	execute_path({dev_stack, execute},
		#{ devices => Stack, message => Msg },
		Path,
		S
	);
execute({Mod, Msg, Path}, S) ->
	?event({executing_device, Mod, Path}),
	execute_path(Mod, Msg, Path, S).

execute_path(_, M, [], _) -> {ok, M};
execute_path(Dev, M, [FuncName|Path], S) ->
	?event({meta_executing_on_path, {device, Dev}, {function, FuncName}, {path, Path}}),
	Func = parse_path_to_func(FuncName),
	{ok, NewM} = ao_device:call(Dev, Func, [M], #{ error_strategy => throw }, S),
	execute_path(Dev, NewM, Path, S).

parse_path_to_func(BinName) when is_binary(BinName) ->
	binary_to_existing_atom(BinName, utf8);
parse_path_to_func(AtomName) when is_atom(AtomName) ->
	AtomName.

%% @doc Resolve the carrier message to an executable message, either by extracting
%% from its body or reading from its referenced ID.
parse_carrier_msg(CarrierMsg, S) ->
    case lists:keyfind(<<"Device">>, 1, CarrierMsg#tx.tags) of
        {_, _} ->
            % If the carrier message itself contains a device, we should execute
            % the path on that device.
            {
                path_to_list(CarrierMsg),
                CarrierMsg
            };
        false ->
            % Otherwise, we try to parse the device from the path + body.
            do_parse_carrier_msg(CarrierMsg, S)
    end.

do_parse_carrier_msg(CarrierMsg, S) ->
    AllParts = [Start|_] = path_to_list(CarrierMsg),
    case parse_path(AllParts, S) of
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
                CarrierMsg,
                Path
            }
    end.

%% @doc Extract the components of the path from the carrier message.
path_to_list(CarrierMsg) ->
    {_, Path} = lists:keyfind(<<"Path">>, 1, CarrierMsg#tx.tags),
    lists:filter(fun(Part) -> byte_size(Part) > 0 end,
        binary:split(Path, <<"/">>, [global])).

%% @doc Identify the device and path of the carrier message.
parse_path([ID|Path], #{ store := RawStore }) when byte_size(ID) == 43 ->
	Store =
		case ao:get(access_remote_cache_for_client) of
			true -> RawStore;
			false -> ao_store:scope(RawStore, local)
		end,
	case ao_cache:read_message(Store, ID) of
		{ok, Msg} ->
			{Msg, Path};
		Error ->
			Error
	end;
parse_path(All = [Start|Rest], _S) ->
    case lists:keyfind(Start, 1, ao:get(default_device_stacks)) of
        {_, {DefaultCall, Dev}} ->
            case Rest of
                [] -> {Dev, [DefaultCall]};
                _ -> {Dev, Rest}
            end;
        false ->
            {undefined, All}
    end.