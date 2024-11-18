-module(hb_device).
-export([call/2, call/3, call/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc This module is the root of the device call logic in hyperbeam.
%%% 
%%% Every device is a collection of keys/functions that can be called in order
%%% to yield their values. Each key may return another message, or a 
%%% binary.
%%% 
%%% When a device key is called, it is passed the current message (likely its 
%%% state), as well as an optional additional message. It must return a tuple of
%%% the form {Status, NewMessage}, where Status is either ok or error, and 
%%% NewMessage is either a new message or a binary.
%%% 
%%% Devices can be expressed as either modules or maps. They can also be 
%%% referenced by an Arweave ID, which can be used to load a device from 
%%% the network (depending on the value of the `load_remote_devices` and 
%%% `trusted_device_signers` settings).

%% @doc Get the value of a message's key by running its associated device
%% function. Optionally, pass a message containing parameters to the call, as
%% well as options that control the runtime environment. This function returns
%% the raw result of the device function call: Typically, but not necessarily,
%% {ok | error, NewMessage}.
%% 
%% In many cases the device will not implement the key, however, so the default
%% device is used instead. The default (`dev_identity`) simply returns the value
%% associated with the key as it exists in the message's underlying Erlang map.
%% In this way, devices are able to implement 'special' keys which do not exist
%% as values in the message's map, while still exposing the 'normal' keys of a
%% map. 'Special' keys which do not exist as values in the message's map are
%% simply ignored.
%%
%% Finally, this function can also take an explicit lambda to call rather than
%% looking up the key in the device. This allows devices to call functions from
%% other modules to yield the value for a key (or set of keys), implementing a
%% form of 'inheritence' or 'delegation' between devices if desired.
call(Msg, Key) ->
	call(Msg, Key, #{}).
call(Msg, Key, ParamMsg) ->
	call(Msg, Key, ParamMsg, #{}).
call(Msg, Key, ParamMsg, Opts) when not is_function(Key) ->
	{Fun, NewOpts} =
		try
			% First, try to load the device and get the function to call.
			{Status, ReturnedFun} = message_to_fun(Msg, Key),
			% Next, add an option to the Opts map to indicate if we should
			% add the key to the start of the arguments.
			{
				ReturnedFun,
				Opts#{ add_key =>
					case Status of
						add_key -> Key;
						_ -> false
					end
				}
			}
		catch
			Class:Exception:Stacktrace ->
				handle_error(
					loading_device,
					{Class, Exception, Stacktrace},
					Opts
				)
		end,
	call(Msg, Fun, ParamMsg, NewOpts);
call(Msg, Fun, ParamMsg, Opts) ->
	% First, determine the arguments to pass to the function.
	% While calculating the arguments we unset the add_key option.
	UserOpts = maps:remove(add_key, Opts),
	Args =
		case maps:get(add_key, Opts, false) of
			false -> [Msg, ParamMsg, UserOpts];
			Key -> [Key, Msg, ParamMsg, UserOpts]
		end,
	% Then, try to execute the function.
	try apply(Fun, truncate_args(Fun, Args))
	catch
		ExecClass:ExecException:ExecStacktrace ->
			handle_error(
				device_call,
				{ExecClass, ExecException, ExecStacktrace},
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
%% This comes in 7 forms:
%% 1. The message does not specify a device, so we use the default device.
%% 2. The device has a `handler` key in its `Dev:info()` map, which is a 
%% function that takes a key and returns a function to handle that key. We pass
%% the key as an additional argument to this function.
%% 3. The device has a function of the name `Key`, which should be called 
%% directly.
%% 4. The device does not implement the key, but does have a default handler 
%% for us to call. We pass it the key as an additional argument.
%% 5. The device does not implement the key, and has no default handler. We use
%% the default device to handle the key.
%% Error: If the device is specified, but not loadable, we raise an error.
%% 
%% Returns {ok | add_key, Fun} where Fun is the function to call, and add_key
%% indicates that the key should be added to the start of the call's arguments.
message_to_fun(Msg, Key) when not is_map_key(device, Msg) ->
	% Case 1: The message does not specify a device, so we use the default device.
	message_to_fun(Msg#{ device => default() }, Key);
message_to_fun(Msg = #{ device := RawDev }, Key) ->
	Dev =
		case device_id_to_executable(RawDev) of
			{error, _} ->
				% Error case: A device is specified, but it is not loadable.
				throw({error, {device_not_loadable, RawDev}});
			{ok, DevMod} -> DevMod
		end,
	case maps:find(handler, Info = info(Dev, Msg)) of
		{ok, Handler} ->
			% Case 2: The device has an explicit handler function.
			{add_key, Handler};
		error ->
			case find_exported_function(Dev, Key, 3) of
				{ok, Func} ->
					% Case 3: The device has a function of the name `Key`.
					{ok, Func};
				not_found ->
					case maps:find(default, Info) of
						{ok, DefaultFunc} ->
							% Case 4: The device has a default handler.
							{add_key, DefaultFunc};
						error ->
							% Case 5: The device has no default handler.
							% We use the default device to handle the key.
							case default() of
								Dev ->
									% We are already using the default device,
									% so we cannot resolve the key. This should
									% never actually happen in practice, but it
									% resolves an infinite loop that can occur
									% during development.
									throw({
										error,
										default_device_could_not_resolve_key,
										{key, Key}
									});
								DefaultDev ->
									message_to_fun(
										Msg#{ device => DefaultDev },
										Key
									)
							end
					end
			end
	end.

%% @doc Find the function with the highest arity that has the given name, if it
%% exists.
%%
%% If the device is a module, we look for a function with the given name.
%% 
%% If the device is a map, we look for a key in the map. First we try to find
%% the key using its literal value. If that fails, we cast the key to an atom
%% and try again.
find_exported_function(Dev, Key, MaxArity) when is_map(Dev) ->
	case maps:get(Key, Dev, not_found) of
		not_found ->
			case key_to_atom(Key) of
				undefined -> not_found;
				Key ->
					% The key is unchanged, so we return not_found.
					not_found;
				KeyAtom ->
					% The key was cast to an atom, so we try again.
					find_exported_function(Dev, KeyAtom, MaxArity)
			end;
		Fun when is_function(Fun) ->
			case erlang:fun_info(Fun, arity) of
				{arity, Arity} when Arity =< MaxArity ->
					{ok, Fun};
				_ -> not_found
			end
	end;
find_exported_function(_Mod, _Key, Arity) when Arity < 0 -> not_found;
find_exported_function(NonAtomKey, Mod, Arity) when not is_atom(NonAtomKey) ->
	case key_to_atom(NonAtomKey) of
		undefined -> not_found;
		KeyAtom -> find_exported_function(Mod, KeyAtom, Arity)
	end;
find_exported_function(Mod, Key, Arity) ->
	case erlang:function_exported(Mod, Key, Arity) of
		true ->
			{ok, fun Mod:Key/Arity};
		false -> find_exported_function(Mod, Key, Arity - 1)
	end.

%% @doc Convert a key to an atom. Takes care of casting from binaries, lists,
%% and iolists. Note: This function is safe, but may return undefined if the
%% key is not an atom, binary, list, iolist, or an existing Erlang atom.
key_to_atom(Key) ->
	try unsafe_key_to_atom(Key)
	catch _:badarg -> undefined
	end.
unsafe_key_to_atom(Key) when is_binary(Key) -> binary_to_existing_atom(Key, utf8);
unsafe_key_to_atom(Key) when is_list(Key) -> 
	FlattenedKey = lists:flatten(Key),
	list_to_existing_atom(FlattenedKey);
unsafe_key_to_atom(Key) when is_atom(Key) -> Key.

%% @doc Load a device module from its name or a message ID.
%% Returns {ok, Executable} where Executable is the device module. On error,
%% a tuple of the form {error, Reason} is returned.
device_id_to_executable(ID) -> device_id_to_executable(ID, #{}).
device_id_to_executable(Map, _Opts) when is_map(Map) -> {ok, Map};
device_id_to_executable(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
device_id_to_executable(ID, Opts) when is_binary(ID) and byte_size(ID) == 43 ->
	case hb:get(load_remote_devices) of
		true ->
			{ok, Msg} = hb_cache:read_message(maps:get(store, Opts), ID),
			Trusted =
				lists:any(
					fun(Signer) ->
						lists:member(Signer, hb:get(trusted_device_signers))
					end,
					hb_message:signers(Msg)
				),
			case Trusted of
				true ->
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
device_id_to_executable(ID, _Opts) ->
    case maps:get(ID, hb:get(preloaded_devices), unsupported) of
        unsupported -> {error, module_not_admissable};
        Mod -> {ok, Mod}
    end.

%% @doc Get the info map for a device, optionally giving it a message if the
%% device's info function is parameterized by one.
info(DevMod, Msg) ->
	case find_exported_function(DevMod, info, 1) of
		{ok, Fun} -> apply(Fun, truncate_args(Fun, [Msg]));
		not_found -> #{}
	end.

%% @doc The default device is the identity device, which simply returns the
%% value associated with any key as it exists in its Erlang map.
default() -> dev_identity.

%%% Tests

key_from_id_device_test() ->
	?assertEqual({ok, 1}, hb_device:call(#{ a => 1 }, a)).

keys_from_id_device_test() ->
	?assertEqual({ok, [a]}, hb_device:call(#{ a => 1, "priv_a" => 2 }, keys)).

%% @doc Generates a test device with three keys, each of which uses
%% progressively more of the arguments that can be passed to a device key.
device_with_keys_using_args_test() ->
	#{
		key_using_only_state =>
			fun(State) ->
				{ok,
					<<(maps:get(state_key, State))/binary>>
				}
			end,
		key_using_state_and_msg =>
			fun(State, Msg) ->
				{ok,
					<<
						(maps:get(state_key, State))/binary,
						(maps:get(msg_key, Msg))/binary
					>>
				}
			end,
		key_using_all =>
			fun(State, Msg, Opts) ->
				{ok,
					<<
						(maps:get(state_key, State))/binary,
						(maps:get(msg_key, Msg))/binary,
						(maps:get(opts_key, Opts))/binary
					>>
				}
			end
	}.

%% @doc Test that arguments are passed to a device key as expected.
%% Particularly, we need to ensure that the key function in the device can 
%% specify any arity (1 through 3) and the call is handled correctly.
key_from_id_device_with_args_test() ->
	Msg =
		#{
			device => device_with_keys_using_args_test(),
			state_key => <<"1">>
		},
	?assertEqual(
		{ok, <<"1">>},
		hb_device:call(
			Msg,
			key_using_only_state,
			#{ msg_key => <<"2">> } % Param message, which is ignored
		)
	),
	?assertEqual(
		{ok, <<"13">>},
		hb_device:call(
			Msg,
			key_using_state_and_msg,
			#{ msg_key => <<"3">> } % Param message, with value to add
		)
	),
	?assertEqual(
		{ok, <<"1337">>},
		hb_device:call(
			Msg,
			key_using_all,
			#{ msg_key => <<"3">> }, % Param message
			#{ opts_key => <<"37">> } % Opts
		)
	).

device_with_handler_function_test() ->
	Msg =
		#{
			device =>
				#{
					info =>
						fun() ->
							#{
								handler =>
									fun(test_key, _S) ->
										{ok, <<"GOOD">>}
									end
							}
						end
				},
			test_key => <<"BAD">>
		},
	?assertEqual(
		{ok, <<"GOOD">>},
		hb_device:call(Msg, test_key)
	).

device_with_default_handler_function_test() ->
	Msg =
		#{
			device =>
				#{
					info =>
						fun() ->
							#{
								default =>
									fun(_, _State) ->
										{ok, <<"DEFAULT">>}
									end
							}
						end,
					state_key =>
						fun(_) ->
							{ok, <<"STATE">>}
						end
				}
		},
	?assertEqual(
		{ok, <<"STATE">>},
		hb_device:call(Msg, state_key)
	),
	?assertEqual(
		{ok, <<"DEFAULT">>},
		hb_device:call(Msg, any_random_key)
	).
