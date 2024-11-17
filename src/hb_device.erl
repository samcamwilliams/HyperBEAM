-module(hb_device).
-export([call/2, call/3, call/4]).
-include("include/hb.hrl").

%%% This module is the root of the device call logic in hyperbeam.
%%% 
%%% Every device is a collection of keys/functions that can be called in order
%%% to yield their values. Each key may return another message, or a 
%%% binary.
%%% 
%%% When a device key is called, it is passed the current message (likely its 
%%% state), as well as an optional additional message. It must return a tuple of
%%% the form {Status, NewMessage}, where Status is either ok or error, and 
%%% NewMessage is either a new message or a binary.

%% @doc Get the value of a message's key by running its associated device
%% function. Optionally, pass a message containing parameters to the call, as
%% well as options that control the runtime environment. This function returns
%% the raw result of the device function call: Typically, but not necessarily,
%% {ok | error, NewMessage}. In many cases the device will not implement the key,
%% however, so the default device is used instead. The default (dev_identity)
%% device simply returns the value associated with the key as it exists in the
%% message's underlying Erlang map. In this way, devices are able to implement
%% 'special' keys which do not exist as values in the message's map, while still
%% exposing the 'normal' keys of a map.
call(Msg, Key) ->
	call(Msg, Key, #{}).
call(Msg, Key, ParamMsg) ->
	call(Msg, Key, ParamMsg, #{}).
call(Msg, Key, ParamMsg, Opts) ->
	try
		% First, try to load the device and get the function to call.
		{Status, Fun} = message_to_fun(Msg, Key),
		% Next, generate the arguments to pass to the function based on the
		% status of the function load.
		Args =
			case Status of
				ok -> [Msg, ParamMsg, Opts];
				add_key -> [Key, Msg, ParamMsg, Opts]
			end,
		% Then, try to execute the function.
		try apply(Fun, ?event(truncate_args(Fun, Args)))
		catch
			ExecClass:ExecException:ExecStacktrace ->
				handle_error(
					device_call,
					{ExecClass, ExecException, ExecStacktrace},
					Opts
				)
		end
	catch
		Class:Exception:Stacktrace ->
			handle_error(
				loading_device,
				{Class, Exception, Stacktrace},
				Opts
			)
	end.

%% @doc Handle an error in a device call.
handle_error(Whence, {Class, Exception, Stacktrace}, Opts) ->
	case maps:get(error_strategy, Opts, throw) of
		throw -> erlang:raise(Class, Exception, Stacktrace);
		_ -> {error, Whence, {Class, Exception, Stacktrace}}
	end.

%% @doc Truncate the arguments of a function to the number of arguments it
%% actually takes.
truncate_args(Fun, Args) ->
	{arity, Arity} = erlang:fun_info(Fun, arity),
	lists:sublist(Args, Arity).

%% @doc Calculate the Erlang function that should be called to get a value for
%% a given key from a device.
%% 
%% This comes in 5 forms:
%% 1. The device has a `handler` key in its `Dev:info()` map, which is a 
%% function that takes a key and returns a function to handle that key. We pass
%% the key as an additional argument to this function.
%% 2. The device has a function of the name `Key`, which should be called 
%% directly.
%% 3. The device does not implement the key, but does have a default handler 
%% for us to call. We pass it the key as an additional argument.
%% 4. The device does not implement the key, and has no default handler. We use
%% the default device to handle the key.
%% 5. The message does not specify a device, so we use the default device.
%% 
%% Returns {ok | add_key, Fun} where Fun is the function to call, and add_key
%% indicates that the key should be added to the start of the call's arguments.
message_to_fun(Msg, Key) when not is_map_key(device, Msg) ->
	message_to_fun(Msg#{ device => default() }, Key);
message_to_fun(Msg = #{ device := RawDev }, Key) ->
	Dev =
		case device_id_to_module(RawDev) of
			{error, _} ->
				% Error case: A device is specified, but it is not loadable.
				throw({error, {device_not_loadable, RawDev}});
			{ok, DevMod} -> DevMod
		end,
	case maps:find(handler, Info = info(Dev, Msg)) of
		{ok, Handler} ->
			% Case 1: The device has an explicit handler function.
			{add_key, Handler};
		error ->
			case find_exported_function(Dev, Key, 3) of
				{ok, Func} ->
					% Case 2: The device has a function of the name `Key`.
					{ok, Func};
				not_found ->
					case maps:find(default, Info) of
						{ok, DefaultFunc} ->
							% Case 3: The device has a default handler.
							{add_key, DefaultFunc};
						error ->
							% Case 4: The device has no default handler.
							% We use the default device to handle the key.
							message_to_fun(Msg#{ device => default() }, Key)
					end
			end
	end.

%% @doc Find the function in a module with the highest arity that has the given
%% name, if it exists.
find_exported_function(_Mod, _Key, 0) -> not_found;
find_exported_function(Mod, Key, Arity) ->
	case erlang:function_exported(Mod, Key, Arity) of
		true -> {ok, fun Mod:Key/Arity};
		false -> find_exported_function(Mod, Key, Arity - 1)
	end.

%% @doc Load a device module from its name or a message ID.
%% Returns {ok, Executable} where Executable is the device module. On error,
%% a tuple of the form {error, Reason} is returned.
device_id_to_module(ID) -> device_id_to_module(ID, #{}).
device_id_to_module(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
device_id_to_module(ID, _Opts) when is_binary(ID) and byte_size(ID) == 43 ->
	case hb:get(load_remote_devices) of
		true ->
			case lists:member(ID, hb:get(trusted_device_signers)) of
				true ->
					Msg = hb_client:download(ID),
					RelBin = erlang:system_info(otp_release),
					case lists:keyfind(<<"Content-Type">>, 1, Msg#tx.tags) of
						<<"BEAM/", RelBin/bitstring>> ->
							{_, ModNameBin} =
								lists:keyfind(<<"Module-Name">>, 1, Msg#tx.tags),
							ModName = list_to_atom(binary_to_list(ModNameBin)),
							case erlang:load_module(ModName, Msg#tx.data) of
								{module, _} -> {ok, ModName};
								{error, Reason} -> {error, Reason}
							end
					end;
				false -> {error, device_signer_not_trusted}
			end;
		false ->
			{error, remote_devices_disabled}
	end;
device_id_to_module(ID, _Opts) ->
    case maps:get(ID, hb:get(preloaded_devices), unsupported) of
        unsupported -> {error, module_not_admissable};
        Mod -> {ok, Mod}
    end.

%% @doc Get the info map for a device, optionally giving it a message if the
%% device's info function is parameterized by one.
info(DevMod, Msg) ->
	case find_exported_function(DevMod, info, 1) of
		{ok, Fun} ->
			apply(Fun, truncate_args(Fun, [Msg]));
		not_found -> #{}
	end.

%% @doc The default device is the identity device, which simply returns the
%% value associated with any key as it exists in its Erlang map.
default() -> dev_identity.