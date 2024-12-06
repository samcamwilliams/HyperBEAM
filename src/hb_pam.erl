-module(hb_pam).
%%% Main device API:
-export([resolve/2, resolve/3, load_device/1]).
-export([to_key/1, to_key/2, key_to_binary/1, key_to_binary/2]).
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
%%% 
%%% Resolution options:
%%% 
%%% `update_hashpath`: Whether to add the `Msg2` to `HashPath` for the `Msg3`.
%%% 					Default: true.
%%% `cache_results`:   Whether to cache the resolved `Msg3`.
%%% 					Default: true.
%%% `add_key`:         Whether to add the key to the start of the arguments.
%%% 					Default: <not set>.
%%% 
%%% In general, all of these options are dangerous. Don't use them unless you
%%% know what you are doing.

%% @doc Get the value of a message's key by running its associated device
%% function. Optionally, pass a message containing parameters to the call, as
%% well as options that control the runtime environment. This function returns
%% the raw result of the device function call: {ok | error, NewMessage}.
%% 
%% In many cases the device will not implement the key, however, so the default
%% device is used instead. The default (`dev_message`) simply returns the 
%% value associated with the key as it exists in the message's underlying
%% Erlang map. In this way, devices are able to implement 'special' keys which
%% do not exist as values in the message's map, while still exposing the 'normal'
%% keys of a map. 'Special' keys which do not exist as values in the message's
%% map are simply ignored.
resolve(Msg1, Request) ->
    resolve(Msg1, Request, default_runtime_opts(Msg1)).
resolve(Msg1, Msg2, Opts) when is_map(Msg2) and is_map(Opts) ->
    prepare_resolve(Msg1, Msg2, Opts);
resolve(Msg1, Key, Opts) ->
    prepare_resolve(Msg1, #{ path => Key }, Opts).

%% @doc Internal function for resolving the path from a message and loading
%% the function to call.
prepare_resolve(Msg1, Msg2, Opts) ->
    %?event({pre_resolve_func_load, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    {Fun, NewOpts} =
        try
            Key = hb_path:hd(Msg2, Opts),
            % Try to load the device and get the function to call.
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
    do_resolve(Msg1, Fun, Msg2, NewOpts).
do_resolve(Msg1, Fun, Msg2, Opts) ->
    ?event({resolving, Fun, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
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
        Msg3 = apply(Fun, truncate_args(Fun, Args)),
        ?event({resolved_result, {explicit, Msg3}}),
        handle_resolved_result(Msg3, Msg2, UserOpts)
    catch
        ExecClass:ExecException:ExecStacktrace ->
            handle_error(
                device_call,
                {ExecClass, ExecException, ExecStacktrace},
                Opts
            )
    end.

%% @doc Internal function for handling the result of a device call.
%% If the result is a binary or something that doesn't have an `ok` status,
%% we return it as is. Otherwise, we need to:
%% 1. Set the HashPath of the Msg3 we have generated from
%% the Msg1 we started with.
%% 2. Pop the first element of the path from Msg2.
%% 3. Write the result to the cache unless the `cache` option is set to false.
%% 4. If there are still elements in the path, we recurse through execution.
%% If additional elements are included as part of the result, we pass them 
%% through to the caller.
handle_resolved_result(Msg3, _Msg2, _UserOpts) when is_binary(Msg3) ->
    Msg3;
handle_resolved_result(Result, Msg2, Opts) when is_tuple(Result) ->
    handle_resolved_result(tuple_to_list(Result), Msg2, Opts);
handle_resolved_result(Msg2List = [Status, Result|_], _Msg2, _Opts)
        when Status =/= ok orelse not is_map(Result) ->
    list_to_tuple(Msg2List);
handle_resolved_result([ok, Msg3Raw | Rest], Msg2, Opts) ->
    Msg3 =
        case hb_opts:get(update_hashpath, true, Opts#{ only => local }) of
            true ->
                % ?event(
                % 	{pushing_hashpath_onto,
                % 		{msg3, {explicit, Msg3Raw}},
                % 		{msg2, Msg2},
                % 		{opts, Opts}
                % 	}
                % ),
                hb_path:push(hashpath, Msg3Raw, Msg2);
            false -> Msg3Raw
        end,
    case hb_opts:get(cache_results, true, Opts) of
        true ->
            hb_cache:write(
                hb_opts:get(store, no_valid_store, Opts),
                hb_message:message_to_tx(Msg3)
            );
        false ->
            ok
    end,
    case hb_path:tl(Msg2, Opts) of
        Res when Res == undefined orelse Res == [] ->
            % The path resolved to the last element, so we return
            % to the caller.
            list_to_tuple([ok, Msg3 | Rest]);
        NextMsg ->
            % There are more elements in the path, so we recurse.
            ?event({recursing, {input, Msg3}, {next, NextMsg}}),
            resolve(Msg3, NextMsg, Opts)
    end.

%% @doc Shortcut for resolving a key in a message without its 
%% status if it is `ok`. This makes it easier to write complex 
%% logic on top of messages while maintaining a functional style.
get(Path, Msg) ->
    get(Path, Msg, default_runtime_opts(Msg)).
get(Path, Msg, Opts) ->
    ensure_ok(Path, resolve(Msg, #{ path => Path }, Opts), Opts).

%% @doc Get the value of a key from a message, returning a default value if the
%% key is not found.
get_default(Key, Msg, Default) ->
    get_default(Msg, Key, Default, #{}).
get_default(Key, Msg, Default, Opts) ->
    case resolve(Msg, Key, Opts) of
        {ok, Value} -> Value;
        {error, _} -> Default
    end.

%% @doc Shortcut to get the list of keys from a message.
keys(Msg) -> keys(Msg, #{}).
keys(Msg, Opts) -> get(keys, Msg, Opts).

%% @doc Shortcut for setting a key in the message using its underlying device.
%% Like the `get/3` function, this function honors the `error_strategy` option.
%% `set` works with maps and recursive paths while maintaining the appropriate
%% `HashPath` for each step.
set(Msg1, Msg2) ->
    set(Msg1, Msg2, #{}).
set(Msg1, Msg2, Opts) when is_map(Msg2) ->
    ?event({set_called, {msg1, Msg1}, {msg2, Msg2}}),
    case ?IS_EMPTY_MESSAGE(Msg2) of
        true -> Msg1;
        false ->
            % First, get the first key and value to set.
            Key = hd(keys(Msg2, Opts#{ hashpath => ignore })),
            Val = get(Key, Msg2, Opts),
            ?event({got_val_to_set, {key, Key}, {val, Val}}),
            % Then, set the key and recurse, removing the key from the Msg2.
            set(set(Msg1, Key, Val, Opts), remove(Msg2, Key, Opts), Opts)
    end.
set(Msg1, Key, Value, Opts) ->
    % For an individual key, we run deep_set with the key as the path.
    % This handles both the case that the key is a path as well as the case
    % that it is a single key.
    Path = hb_path:term_to_path(Key),
    %?event({setting_individual_key, {msg1, Msg1}, {key, Key}, {path, Path}, {value, Value}}),
    deep_set(Msg1, Path, Value, Opts).

%% @doc Recursively search a map, resolving keys, and set the value of the key
%% at the given path.
deep_set(Msg, [Key], Value, Opts) ->
    %?event({setting_last_key, {key, Key}, {value, Value}}),
    device_set(Msg, Key, Value, Opts);
deep_set(Msg, [Key|Rest], Value, Opts) ->
    {ok, SubMsg} = resolve(Msg, Key, Opts),
    ?event({traversing_deeper_to_set, {current_key, Key}, {current_value, SubMsg}, {rest, Rest}}),
    device_set(Msg, Key, deep_set(SubMsg, Rest, Value, Opts), Opts).

device_set(Msg, Key, Value, Opts) ->
    ?event({calling_device_set, {key, Key}, {value, Value}}),
    ensure_ok(Key, resolve(Msg, #{ path => set, Key => Value }, Opts), Opts).

%% @doc Remove a key from a message, using its underlying device.
remove(Msg, Key) -> remove(Msg, Key, #{}).
remove(Msg, Key, Opts) ->
    ensure_ok(Key, resolve(Msg, #{ path => remove, item => Key }, Opts), Opts).

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
    ?event({message_to_fun, {msg, Msg}, {device, RawDev}, {key, Key}}),
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
                    case is_exported(Dev, Key) of
                        true -> {ok, Fun};
                        false -> not_found
                    end;
                _ -> not_found
            end
    end;
find_exported_function(_Mod, _Key, Arity) when Arity < 0 -> not_found;
find_exported_function(Mod, Key, Arity) when not is_atom(Key) ->
    case to_key(Key) of
        ConvertedKey when is_atom(ConvertedKey) ->
            find_exported_function(Mod, ConvertedKey, Arity);
        undefined -> not_found;
        BinaryKey when is_binary(BinaryKey) ->
            not_found
    end;
find_exported_function(Mod, Key, Arity) ->
    case erlang:function_exported(Mod, Key, Arity) of
        true ->
            case is_exported(Mod, Key) of
                true -> {ok, fun Mod:Key/Arity};
                false -> not_found
            end;
        false -> find_exported_function(Mod, Key, Arity - 1)
    end.

%% @doc Check if a device is guarding a key via its `exports` list. Defaults to
%% true if the device does not specify an `exports` list. The `info` function is
%% always exported, if it exists.
is_exported(_, info) -> true;
is_exported(Dev, Key) ->
    case info(Dev) of
        #{ exports := Exports } ->
            lists:member(Key, Exports);
        _ -> true
    end.

%% @doc Convert a key to an atom if it already exists in the Erlang atom table,
%% or to a binary otherwise.
to_key(Key) -> to_key(Key, #{ error_strategy => throw }).
to_key(Key, _Opts) when byte_size(Key) == 43 -> Key;
to_key(Key, _Opts) -> 
    try to_atom_unsafe(Key)
    catch _Type:_:_Trace -> key_to_binary(Key)
    end.

%% @doc Convert a key to its binary representation.
key_to_binary(Key) -> key_to_binary(Key, #{}).
key_to_binary(Key, _Opts) when is_binary(Key) -> Key;
key_to_binary(Key, _Opts) when is_atom(Key) -> atom_to_binary(Key);
key_to_binary(Key, _Opts) when is_list(Key) -> list_to_binary(Key).

%% @doc Helper function for key_to_atom that does not check for errors.
to_atom_unsafe(Key) when is_integer(Key) ->
    integer_to_binary(Key);
to_atom_unsafe(Key) when is_binary(Key) ->
    binary_to_existing_atom(hb_util:to_lower(Key), utf8);
to_atom_unsafe(Key) when is_list(Key) ->
    FlattenedKey = lists:flatten(Key),
    list_to_existing_atom(FlattenedKey);
to_atom_unsafe(Key) when is_atom(Key) -> Key.

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
    case hb_opts:get(load_remote_devices) of
        true ->
            {ok, Msg} = hb_cache:read_message(maps:get(store, Opts), ID),
            Trusted =
                lists:any(
                    fun(Signer) ->
                        lists:member(Signer, hb_opts:get(trusted_device_signers))
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
    case maps:get(ID, hb_opts:get(preloaded_devices), unsupported) of
        unsupported -> {error, module_not_admissable};
        Mod -> {ok, Mod}
    end.

%% @doc Get the info map for a device, optionally giving it a message if the
%% device's info function is parameterized by one.
info(DevMod) -> info(DevMod, #{}).
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
    ?assertEqual({ok, [test_path]}, hb_pam:resolve(#{ path => [test_path] }, path)),
    ?assertEqual({ok, [a]}, hb_pam:resolve(#{ <<"Path">> => [a] }, <<"Path">>)).

key_to_binary_test() ->
    ?assertEqual(<<"a">>, hb_pam:key_to_binary(a)),
    ?assertEqual(<<"a">>, hb_pam:key_to_binary(<<"a">>)),
    ?assertEqual(<<"a">>, hb_pam:key_to_binary("a")).

resolve_binary_key_test() ->
    ?assertEqual({ok, 1}, hb_pam:resolve(#{ a => 1 }, <<"a">>)),
    ?assertEqual({ok, 1}, hb_pam:resolve(#{ <<"Test-Header">> => 1 }, <<"Test-Header">>)).

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
            #{
                path => key_using_only_state,
                msg_key => <<"2">> % Param message, which is ignored
            }
        )
    ),
    ?assertEqual(
        {ok, <<"13">>},
        hb_pam:resolve(
            Msg,
            #{
                path => key_using_state_and_msg,
                msg_key => <<"3">> % Param message, with value to add
            }
        )
    ),
    ?assertEqual(
        {ok, <<"1337">>},
        hb_pam:resolve(
            Msg,
            #{
                path => key_using_all,
                msg_key => <<"3">> % Param message
            },
            #{
                opts_key => <<"37">> % Opts
            }
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
    ?assertEqual(<<"value1">>, hb_pam:get(key1, Msg)),
    ?assertEqual(<<"value2">>, hb_pam:get(key2, Msg)).

basic_set_test() ->
    Msg = #{ key1 => <<"value1">>, key2 => <<"value2">> },
    UpdatedMsg = hb_pam:set(Msg, #{ key1 => <<"new_value1">> }),
    ?event({set_key_complete, {key, key1}, {value, <<"new_value1">>}}),
    ?assertEqual(<<"new_value1">>, hb_pam:get(key1, UpdatedMsg)),
    ?assertEqual(<<"value2">>, hb_pam:get(key2, UpdatedMsg)).

get_with_device_test() ->
    Msg =
        #{
            device => generate_device_with_keys_using_args(),
            state_key => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_pam:get(state_key, Msg)),
    ?assertEqual(<<"STATE">>, hb_pam:get(key_using_only_state, Msg)).

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
    ?assertEqual(<<"STATE">>, hb_pam:get(state_key, Msg)),
    SetOnce = hb_pam:set(Msg, #{ state_key => <<"SET_ONCE">> }),
    ?assertEqual(1, hb_pam:get(set_count, SetOnce)),
    SetTwice = hb_pam:set(SetOnce, #{ state_key => <<"SET_TWICE">> }),
    ?assertEqual(2, hb_pam:get(set_count, SetTwice)),
    ?assertEqual(<<"STATE">>, hb_pam:get(state_key, SetTwice)).

deep_set_test() ->
    % First validate second layer changes are handled correctly.
    Msg0 = #{ a => #{ b => 1 } },
    ?assertMatch(#{ a := #{ b := 2 } },
        hb_pam:set(Msg0, [a, b], 2, #{})),
    % Now validate deeper layer changes are handled correctly.
    Msg = #{ a => #{ b => #{ c => 1 } } },
    ?assertMatch(#{ a := #{ b := #{ c := 2 } } },
        hb_pam:set(Msg, [a, b, c], 2, #{})).

deep_set_with_device_test() ->
    Device = #{
        set =>
            fun(Msg1, Msg2) ->
                % A device where the set function modifies the key
                % and adds a modified flag.
                {Key, Val} = hd(maps:to_list(maps:remove(path, Msg2))),
                {ok, Msg1#{ Key => Val, modified => true }}
            end
    },
    % A message with an interspersed custom device: A and C have it,
    % B does not. A and C will have the modified flag set to true.
    Msg = #{
        device => Device,
        a =>
            #{
                b =>
                    #{
                        device => Device,
                        c => 1,
                        modified => false
                    },
                modified => false
            },
        modified => false
    },
    Outer = deep_set(Msg, [a, b, c], 2, #{}),
    A = hb_pam:get(a, Outer),
    B = hb_pam:get(b, A),
    C = hb_pam:get(c, B),
    ?assertEqual(2, C),
    ?assertEqual(true, hb_pam:get(modified, Outer)),
    ?assertEqual(false, hb_pam:get(modified, A)),
    ?assertEqual(true, hb_pam:get(modified, B)).

device_exports_test() ->
    ?assert(is_exported(dev_message, info)),
    ?assert(is_exported(dev_message, set)),
    ?assert(not is_exported(dev_message, not_exported)),
    Dev = #{
        info => fun() -> #{ exports => [set] } end,
        set => fun(_, _) -> {ok, <<"SET">>} end
    },
    ?assert(is_exported(Dev, info)),
    ?assert(is_exported(Dev, set)),
    ?assert(not is_exported(Dev, not_exported)).