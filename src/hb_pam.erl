-module(hb_pam).
%%% Main device API:
-export([resolve/2, resolve/3, resolve/4]).
-export([to_key/1, to_key/2, load_device/1]).
%%% Shortcuts:
-export([keys/1, keys/2]).
-export([get/2, get/3, get_default/3, get_default/4]).
-export([set/2, set/3, set/4, remove/2, remove/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @moduledoc This module is the root of the device call logic of the 
%%% Permaweb Abstract Machine (PAM) in HyperBEAM.
%%% 
%%% At the PAM layer, every device is simply a collection of keys that can be
%%% resolved in order to yield their values. Each key may return another 
%%% message or a binary:
%%% 
%%% 	resolve(Message1, Message2) -> {Status, Message3}
%%% 
%%% See `docs/permaweb-abstract-machine.md` for more information about PAM.
%%% 
%%% When a device key is called, it is passed the `Message1` (likely its state),
%%% as well as the message to 'apply' to it. It must return a tuple of the
%%% form {Status, NewMessage}, where Status is either ok or error, and 
%%% NewMessage is either a new message or a binary.
%%% 
%%% The key to resolve is typically specified by the `Path` field of the 
%%% message.
%%% 
%%% In the HyperBEAM implementation (this module), `Message1` can be replaced
%%% a function name to execute for ease of development with PAM. In this 
%%% case, the function name is cast to an unsigned message with the `Path` set
%%% to the given name.
%%% 
%%% Devices can be expressed as either modules or maps. They can also be 
%%% referenced by an Arweave ID, which can be used to load a device from 
%%% the network (depending on the value of the `load_remote_devices` and 
%%% `trusted_device_signers` environment settings).

%% @doc Get the value of a message's key by running its associated device
%% function. Optionally, pass a message containing parameters to the call, as
%% well as options that control the runtime environment. This function returns
%% the raw result of the device function call: {ok | error, NewMessage}.
%% 
%% In many cases the device will not implement the key, however, so the default
%% device is used instead. The default (`dev_message`) simply returns the 
%% value associated with the key as it exists in the message's underlying
%% Erlang map.
%% 
%% In this way, devices are able to implement 'special' keys which do not exist
%% as values in the message's map, while still exposing the 'normal' keys of a
%% map. 'Special' keys which do not exist as values in the message's map are
%% simply ignored.
%%
%% Finally, this function can also take an explicit lambda to call rather than
%% looking up the key in the device. This allows devices to call functions from
%% other modules to yield the value for a key (or set of keys), implementing a
%% form of 'inheritence' or 'delegation' between devices if desired.
resolve(Msg1, Request) ->
	resolve(Msg1, Request, default_runtime_opts(Msg1)).
resolve(Msg1, Msg2, Opts) when is_map(Msg2) and is_map(Opts) ->
	?event({resolve, Msg1, Msg2, Opts}),
	resolve(Msg1, key_from_path(Msg2, Opts), Msg2, Opts);
resolve(Msg1, Key, Msg2) ->
	resolve(
		Msg1,
		Key,
		Msg2#{ <<"Path">> => Key },
		default_runtime_opts(Msg1)
	).
resolve(Msg1, Key, Msg2, Opts) when not is_function(Key) ->
	?event({pre_resolve_func_load, {msg1, Msg1}, {key, Key}, {msg2, Msg2}, {opts, Opts}}),
	{Fun, NewOpts} =
		try
			% First, try to load the device and get the function to call.
			{Status, ReturnedFun} = message_to_fun(Msg1, Key),
			% Next, add an option to the Opts map to indicate if we should
			% add the key to the start of the arguments. Note: This option
			% is used downstream by other devices (like `dev_stack`), so 
			% should be changed with care.
			{
				ReturnedFun,
				Opts#{
					add_key =>
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
	resolve(Msg1, Fun, Msg2, NewOpts);
resolve(Msg1, Fun, Msg2, Opts) ->
	?event({resolve_call, Msg1, Fun, Msg2, Opts}),
	% First, determine the arguments to pass to the function.
	% While calculating the arguments we unset the add_key option.
	UserOpts = maps:remove(add_key, Opts),
	Args =
		case maps:get(add_key, Opts, false) of
			false -> [Msg1, Msg2, UserOpts];
			Key -> [Key, Msg1, Msg2, UserOpts]
		end,
	% Then, try to execute the function.
	try
		Res = apply(Fun, truncate_args(Fun, Args)),
		?event({resolve_result, Res}),
		case Res of
			_ when is_tuple(Res) and element(1, Res) == error ->
				throw(Res);
			Else ->
				Else
		end
	catch
		ExecClass:ExecException:ExecStacktrace ->
			handle_error(
				device_call,
				{ExecClass, ExecException, ExecStacktrace},
				Opts
			)
	end.

%% @doc Shortcut for resolving a key in a message without its 
%% status if it is `ok`. This makes it easier to write complex 
%% logic on top of messages while maintaining a functional style.
get(Msg, Key) ->
	% Note: This function should not be refactored to use `get/3`, as it
	% should honor the default runtime options that `resolve/2` provides.
	ensure_ok(Key, resolve(Msg, Key), #{}).
get(Msg, Key, Opts) -> ensure_ok(Key, resolve(Msg, Key, Opts), Opts).

%% @doc Get the value of a key from a message, returning a default value if the
%% key is not found.
get_default(Msg, Key, Default) ->
	get_default(Msg, Key, Default, #{}).
get_default(Msg, Key, Default, Opts) ->
	case resolve(Msg, Key, Opts) of
		{ok, Value} -> Value;
		{error, _} -> Default
	end.

%% @doc Shortcut to get the list of keys from a message.
keys(Msg) -> keys(Msg, #{}).
keys(Msg, Opts) -> get(Msg, keys, Opts).

%% @doc Shortcut for setting a key in the message using its underlying device.
%% Like the `get/3` function, this function honors the `error_strategy` option.
%% `set` works with maps and recursive paths while maintaining the appropriate
%% `HashPath` for each step.
set(Msg1, Msg2) ->
	set(Msg1, Msg2, #{}).
set(Msg1, Msg2, _Opts) when map_size(Msg2) == 0 -> Msg1;
set(Msg1, Msg2, Opts) when is_map(Msg2) ->
	Val = get(Msg1, Key = hd(keys(Msg2, Opts)), Opts),
	set(set(Msg1, Key, Val, Opts), remove(Msg2, Key, Opts), Opts).
set(Msg1, Key, Value, Opts) ->
	deep_set(Msg1, to_path(Key), Value, Opts).

%% @doc Recursively search a map, resolving keys, and set the value of the key
%% at the given path.
deep_set(Msg, [LastKey], Value, Opts) ->
	ensure_ok(LastKey, resolve(Msg, set, #{ LastKey => Value }, Opts), Opts);
deep_set(Msg, [Key | Rest], Value, Opts) ->
	deep_set(
		set(
			Msg,
			Key,
			deep_set(Msg, Rest, Value, Opts),
			Opts
		),
		Rest,
		Value,
		Opts
	).

%% @doc Remove a key from a message, using its underlying device.
remove(Msg, Key) -> remove(Msg, Key, #{}).
remove(Msg, Key, Opts) ->
	ensure_ok(Key, resolve(Msg, remove, #{ key => Key }, Opts), Opts).

%% @doc Helper for returning `Message2` if the resolution has a status of `ok`,
%% or handling an error based on the value of the `error_strategy` option.
ensure_ok(_Key, {ok, Value}, _Opts) -> Value;
ensure_ok(_Key, {error, Reason}, #{ error_strategy := X }) when X =/= throw ->
	{unexpected, {error, Reason}};
ensure_ok(Key, {error, Reason}, _Opts) ->
	throw({pam_resolution_error, Key, Reason}).

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
	message_to_fun(Msg#{ device => default_module() }, Key);
message_to_fun(Msg = #{ device := RawDev }, Key) ->
	Dev =
		case load_device(RawDev) of
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
							case default_module() of
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
			case to_key(Key) of
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
find_exported_function(Mod, Key, Arity) when not is_atom(Key) ->
	case to_key(Key) of
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
%% integers, and iolists. This function is unsafe by befault, throwing an error
%% if the key is not castable, but returns undefined in error cases if the
%% error_strategy option is set to a value other than throw.
to_key(Key) -> to_key(Key, #{ error_strategy => throw }).
to_key(Key, Opts) -> 
	try to_atom_unsafe(Key)
	catch Type:_:Trace ->
		case maps:get(error_strategy, Opts, throw) of
			throw -> erlang:raise(Type, invalid_key, Trace);
			_ -> undefined
		end
	end.

%% @doc Helper function for key_to_atom that does not check for errors.
to_atom_unsafe(Key) when is_integer(Key) ->
	integer_to_binary(Key);
to_atom_unsafe(Key) when is_binary(Key) ->
	binary_to_existing_atom(hb_util:to_lower(Key), utf8);
to_atom_unsafe(Key) when is_list(Key) -> 
	FlattenedKey = lists:flatten(Key),
	list_to_existing_atom(FlattenedKey);
to_atom_unsafe(Key) when is_atom(Key) -> Key.

%% @doc Convert a term into an executable path. Supports binaries, lists, and
%% atoms. Notably, it does not support strings as lists of characters.
to_path(Path) -> to_path(Path, #{ error_strategy => throw }).
to_path(Binary, Opts) when is_binary(Binary) ->
	?event({to_path, Binary}),
	case binary:match(Binary, <<"/">>) of
		nomatch -> [Binary];
		_ ->
			to_path(
				lists:filter(
					fun(Part) -> byte_size(Part) > 0 end,
					binary:split(Binary, <<"/">>, [global])
				),
				Opts
			)
	end;
to_path(List, Opts) when is_list(List) ->
	lists:map(fun(Part) -> to_key(Part, Opts) end, List);
to_path(Atom, _Opts) when is_atom(Atom) -> [Atom].

%% @doc Extract the key from a `Message2`'s `Path` field. Returns the first
%% element of the path if it is a list, otherwise returns the path as is.
key_from_path(Msg2, Opts) ->
	?event({key_from_path, resolve(Msg2, <<"Path">>, Opts)}),
	hd(to_path(resolve(Msg2, <<"Path">>, Opts))).

%% @doc Load a device module from its name or a message ID.
%% Returns {ok, Executable} where Executable is the device module. On error,
%% a tuple of the form {error, Reason} is returned.
load_device(ID) -> load_device(ID, #{}).
load_device(Map, _Opts) when is_map(Map) -> {ok, Map};
load_device(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
load_device(ID, Opts) when is_binary(ID) and byte_size(ID) == 43 ->
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
load_device(ID, _Opts) ->
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

%% @doc The default runtime options for a message. At the moment the `Message1`
%% but it is included such that we can modulate the options based on the message
%% if needed in the future.
default_runtime_opts(_Msg1) ->
	#{
		error_strategy => throw
	}.

%% @doc The default device is the identity device, which simply returns the
%% value associated with any key as it exists in its Erlang map. It should also
%% implement the `set` key, which returns a `Message3` with the values changed
%% according to the `Message2` passed to it.
default_module() -> dev_message.

%%% Tests

key_from_id_device_test() ->
	?assertEqual({ok, 1}, hb_pam:resolve(#{ a => 1 }, a)).

keys_from_id_device_test() ->
	?assertEqual({ok, [a]}, hb_pam:resolve(#{ a => 1, "priv_a" => 2 }, keys)).

path_test() ->
	?assertEqual({ok, test_path}, hb_pam:resolve(#{ path => test_path }, path)),
	?assertEqual({ok, a}, hb_pam:resolve(#{ <<"Path">> => [a] }, <<"Path">>)).

%% @doc Generates a test device with three keys, each of which uses
%% progressively more of the arguments that can be passed to a device key.
generate_device_with_keys_using_args() ->
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
			device => generate_device_with_keys_using_args(),
			state_key => <<"1">>
		},
	?assertEqual(
		{ok, <<"1">>},
		hb_pam:resolve(
			Msg,
			key_using_only_state,
			#{ msg_key => <<"2">> } % Param message, which is ignored
		)
	),
	?assertEqual(
		{ok, <<"13">>},
		hb_pam:resolve(
			Msg,
			key_using_state_and_msg,
			#{ msg_key => <<"3">> } % Param message, with value to add
		)
	),
	?assertEqual(
		{ok, <<"1337">>},
		hb_pam:resolve(
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
		hb_pam:resolve(Msg, test_key)
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
		hb_pam:resolve(Msg, state_key)
	),
	?assertEqual(
		{ok, <<"DEFAULT">>},
		hb_pam:resolve(Msg, any_random_key)
	).

basic_get_test() ->
	Msg = #{ key1 => <<"value1">>, key2 => <<"value2">> },
	?assertEqual(<<"value1">>, hb_pam:get(Msg, key1)),
	?assertEqual(<<"value2">>, hb_pam:get(Msg, key2)).

basic_set_test() ->
	Msg = #{ key1 => <<"value1">>, key2 => <<"value2">> },
	UpdatedMsg = hb_pam:set(Msg, #{ key1 => <<"new_value1">> }),
	?assertEqual(<<"new_value1">>, hb_pam:get(UpdatedMsg, key1)),
	?assertEqual(<<"value2">>, hb_pam:get(UpdatedMsg, key2)).

get_with_device_test() ->
	Msg =
		#{
			device => generate_device_with_keys_using_args(),
			state_key => <<"STATE">>
		},
	?assertEqual(<<"STATE">>, hb_pam:get(Msg, state_key)),
	?assertEqual(<<"STATE">>, hb_pam:get(Msg, key_using_only_state)).

set_with_device_test() ->
	Msg =
		#{
			device =>
				#{
					set =>
						fun(State, _Msg) ->
							{ok,
								State#{
									set_count =>
										1 + maps:get(set_count, State, 0)
								}
							}
						end
				},
			state_key => <<"STATE">>
		},
	?assertEqual(<<"STATE">>, hb_pam:get(Msg, state_key)),
	SetOnce = hb_pam:set(Msg, #{ state_key => <<"SET_ONCE">> }),
	?assertEqual(1, hb_pam:get(SetOnce, set_count)),
	SetTwice = hb_pam:set(SetOnce, #{ state_key => <<"SET_TWICE">> }),
	?assertEqual(2, hb_pam:get(SetTwice, set_count)),
	?assertEqual(<<"STATE">>, hb_pam:get(SetTwice, state_key)).

deep_set_test() ->
	Msg = #{ a => #{ b => #{ c => 1 } } },
	?assertEqual(#{ a => #{ b => #{ c => 2 } } },
		hb_pam:set(Msg, [a, b, c], 2, #{})),
	Device = #{
		set =>
			fun(Msg1, Msg2) ->
				% Merge but keep the old state nested under the new key.
				{ok, maps:merge(Msg1#{ old_key => Msg2 }, Msg2)}
			end
	},
	?assertEqual(
		#{
			a => #{
				b => #{
					c => 2,
					old_key => #{
						c => 1
					}
				},
				old_key => #{
					b => #{
						c => 1
					}
				}
			}
		},
		set(
			Msg,
			[a, b, c],
			2,
			#{ device => Device }
		)
	).
