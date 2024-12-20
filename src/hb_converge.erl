-module(hb_converge).
%%% Main Converge API:
-export([resolve/2, resolve/3, load_device/2, message_to_device/2]).
-export([to_key/1, to_key/2, key_to_binary/1, key_to_binary/2]).
%%% Shortcuts and tools:
-export([info/2, keys/1, keys/2, keys/3]).
-export([get/2, get/3, get/4, set/2, set/3, set/4, remove/2, remove/3]).
-export([truncate_args/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @moduledoc This module is the root of the device call logic of the 
%%% Converge Protocol in HyperBEAM.
%%% 
%%% At the implementation level, every message is simply a collection of keys,
%%% dictated by its `Device`, that can be resolved in order to yield their
%%% values. Each key may return another message or a raw value:
%%% 
%%% 	converge(Message1, Message2) -> {Status, Message3}
%%% 
%%% Under-the-hood, `Converge(Message1, Message2)` leads to the evaluation of
%%% `DeviceMod:PathPart(Message1, Message2)`, which defines the user compute
%%% to be performed. If `Message1` does not specify a device, `dev_message` is
%%% assumed. The key to resolve is specified by the `Path` field of the message.
%%% 
%%% After each output, the `HashPath` is updated to include the `Message2`
%%% that was executed upon it.
%%% 
%%% Because each message implies a device that can resolve its keys, as well
%%% as generating a merkle tree of the computation that led to the result,
%%% you can see Converge Protocol as a system for cryptographically chaining 
%%% the execution of `combinators`. See `docs/converge-protocol.md` for more 
%%% information about Converge.
%%% 
%%% The `Fun(Message1, Message2)` pattern is repeated throughout the HyperBEAM 
%%% codebase, sometimes with `MessageX` replaced with `MX` or `MsgX` for brevity.
%%% 
%%% Message3 can be either a new message or a raw output value (a binary, integer,
%%% float, atom, or list of such values).
%%% 
%%% Devices can be expressed as either modules or maps. They can also be 
%%% referenced by an Arweave ID, which can be used to load a device from 
%%% the network (depending on the value of the `load_remote_devices' and 
%%% `trusted_device_signers' environment settings).
%%% 
%%% HyperBEAM device implementations are defined as follows:
%%% 
%%%     DevMod:ExportedFunc : Key resolution functions. All are assumed to be
%%%                           device keys (thus, present in every message that
%%%                           uses it) unless specified by `DevMod:info()`.
%%%                           Each function takes a set of parameters
%%%                           of the form `DevMod:KeyHandler(Msg1, Msg2, Opts)`.
%%%                           Each of these arguments can be ommitted if not
%%%                           needed. Non-exported functions are not assumed
%%%                           to be device keys.
%%%
%%%     DevMod:info : Optional. Returns a map of options for the device. All 
%%%                   options are optional and assumed to be the defaults if 
%%%                   not specified. This function can accept a `Message1` as 
%%%                   an argument, allowing it to specify its functionality 
%%%                   based on a specific message if appropriate.
%%% 
%%%     info/exports : Overrides the export list of the Erlang module, such that
%%%                   only the functions in this list are assumed to be device
%%%                   keys. Defaults to all of the functions that DevMod 
%%%                   exports in the Erlang environment.
%%%
%%%     info/excludes : A list of keys that should not be resolved by the device,
%%%                     despite being present in the Erlang module exports list.
%%% 
%%%     info/handler : A function that should be used to handle _all_ keys for 
%%%                    messages using the device.
%%% 
%%%     info/default : A function that should be used to handle all keys that
%%%                    are not explicitly implemented by the device. Defaults to
%%%                    the `dev_message` device, which contains general keys for 
%%%                    interacting with messages.
%%% 
%%%     info/default_mod : A different device module that should be used to
%%%                        handle all keys that are not explicitly implemented
%%%                        by the device. Defaults to the `dev_message` device.
%%% 
%%% The HyperBEAM resolver also takes a number of runtime options that change
%%% the way that the environment operates:
%%% 
%%% `update_hashpath`:  Whether to add the `Msg2` to `HashPath` for the `Msg3`.
%%% 					Default: true.
%%% `cache_results`:    Whether to cache the resolved `Msg3`.
%%% 					Default: true.
%%% `add_key`:          Whether to add the key to the start of the arguments.
%%% 					Default: <not set>.

%% @doc Takes a singleton message and parse Msg1 and Msg2 from it, then invoke
%% `resolve`.
resolve(Msg, Opts) ->
    Path =
        hb_path:term_to_path(
            hb_converge:get(
                path,
                Msg,
                #{ hashpath => ignore }
            ),
            Opts
        ),
    case Path of
        [ Msg1ID | _Rest ] when ?IS_ID(Msg1ID) ->
            ?event({normalizing_single_message_message_path, Msg}),
            {ok, Msg1} = hb_cache:read(Msg1ID, Opts),
            resolve(
                Msg1,
                hb_path:tl(Msg, Opts),
                Opts
            );
        SingletonPath ->
            resolve(Msg, #{ path => SingletonPath }, Opts)
    end.

%% @doc Get the value of a message's key by running its associated device
%% function. Optionally, takes options that control the runtime environment. 
%% This function returns the raw result of the device function call:
%% {ok | error, NewMessage}.
resolve(Msg1, Msg2, Opts) ->
    resolve_stage(0, Msg1, Msg2, Opts).

%% @doc Internal function for handling request resolution.
%% The resolver is composed of a series of discrete phases:
%%      0: Cache lookup.
%%      1: Validation check.
%%      2: Path normalization.
%%      3: Persistent-resolver lookup.
%%      4: Device lookup.
%%      5: Execution.
%%      6: Cryptographic linking.
%%      7: Result caching.
%%      8: Notify waiters.
%%      9: Recurse, fork, or terminate.

resolve_stage(0, Msg1, Msg2, Opts) when is_list(Msg1) ->
    ?event(converge_core, {stage, 0, list_normalize}, Opts),
    % Normalize lists to numbered maps (base=1) if necessary.
    resolve_stage(0,
        maps:from_list(
            lists:zip(
                lists:seq(1, length(Msg1)),
                Msg1
            )
        ),
        Msg2,
        Opts
    );
resolve_stage(0, Msg1, Path, Opts) when not is_map(Path) ->
    ?event(converge_core, {stage, 0, path_to_message_normalize}, Opts),
    % If we have been given a Path rather than a full Msg2, construct the
    % message around it and recurse.
    resolve_stage(0, Msg1, #{ path => Path }, Opts);
resolve_stage(0, Msg1, Msg2, Opts) ->
    ?event(converge_core, {stage, 0, cache_lookup}, Opts),
    case hb_cache:read_output(Msg1, Msg2, Opts) of
        {ok, Msg3} ->
            ?event({cache_hit, {msg1, Msg1}, {msg2, Msg2}, {msg3, Msg3}}),
            {ok, Msg3};
        not_found ->
            resolve_stage(1, Msg1, Msg2, Opts)
    end;
resolve_stage(1, Msg1, Msg2, Opts) ->
    ?event(converge_core, {stage, 1, validation_check}, Opts),
    % Validation check: Check if the message is valid.
    Msg1Valid = (hb_message:signers(Msg1) == []) orelse hb_message:verify(Msg1),
    Msg2Valid = (hb_message:signers(Msg2) == []) orelse hb_message:verify(Msg2),
    ?no_prod("Enable message validity checks!"),
    case {Msg1Valid, Msg2Valid} of
        _ -> resolve_stage(2, Msg1, Msg2, Opts);
        _ -> error_invalid_message(Msg1, Msg2, Opts)
    end;
resolve_stage(2, Msg1, Msg2, Opts) ->
    ?event(converge_core, {stage, 2, path_normalization}, Opts),
    % Path normalization: Ensure that the path is requesting a single key.
    % Stash remaining path elements in `priv/Converge/Remaining-Path`.
    % Stash the original path in `priv/Converge/Original-Path`, if it
    % is not already there from a previous resolution.
    InitialPriv = hb_private:from_message(Msg1),
    OriginalPath =
        case InitialPriv of
            #{ <<"Converge">> := #{ <<"Original-Path">> := XPath } } ->
                XPath;
            _ -> hb_path:from_message(request, Msg2)
        end,
    Head = hb_path:hd(Msg2, Opts),
    FullPath = hb_path:from_message(request, Msg2),
    RemainingPath = hb_path:tl(FullPath, Opts),
    Msg2UpdatedPriv =
        Msg2#{
            priv =>
                InitialPriv#{
                    <<"Converge">> =>
                        #{
                            <<"Original-Path">> => OriginalPath,
                            <<"Remaining-Path">> => RemainingPath
                        }
                }
        },
    resolve_stage(3, Msg1, Msg2UpdatedPriv#{ path => Head }, Opts);
resolve_stage(3, Msg1, Msg2, Opts) ->
    ?event(converge_core, {stage, 3}, Opts),
    % Persistent-resolver lookup: Search for local (or Distributed
    % Erlang cluster) processes that are already performing the execution.
    % Before we search for a live executor, we check if the device specifies 
    % a function that tailors the 'group' name of the execution. For example, 
    % the `dev_process` device 'groups' all calls to the same process onto
    % calls to a single executor. By default, `{Msg1, Msg2}` is used as the
    % group name.
    case hb_persistent:find_or_register(Msg1, Msg2, Opts) of
        {leader, ExecName} ->
            % We are the leader for this resolution. Continue to the next stage.
            resolve_stage(4, Msg1, Msg2, ExecName, Opts);
        {wait, Leader} ->
            % There is another executor of this resolution in-flight.
            % Bail execution, register to receive the response, then
            % wait.
            hb_persistent:await(Leader, Msg1, Msg2, Opts);
        {infinite_recursion, GroupName} ->
            % We are the leader for this resolution, but we executing the 
            % computation again. This may plausibly be OK in _some_ cases,
            % but in general it is the sign of a bug.
            ?event(
                converge_core,
                {infinite_recursion,
                    {exec_group, GroupName},
                    {msg1, Msg1},
                    {msg2, Msg2},
                    {opts, Opts}
                },
                Opts
            ),
            case hb_opts:get(allow_infinite, false, Opts) of
                true ->
                    % We are OK with infinite loops, so we just continue.
                    resolve_stage(4, Msg1, Msg2, GroupName, Opts);
                false ->
                    % We are not OK with infinite loops, so we raise an error.
                    error_infinite(Msg1, Msg2, Opts)
            end
    end.
resolve_stage(4, Msg1, Msg2, ExecName, Opts) ->
    ?event(converge_core, {stage, 4, ExecName}, Opts),
    %% Device lookup: Find the Erlang function that should be utilized to 
    %% execute Msg2 on Msg1.
	{ResolvedFunc, NewOpts} =
		try
			Key = hb_path:hd(Msg2, Opts),
			% Try to load the device and get the function to call.
            ?event(
                {
                    resolving_key,
                    {key, Key},
                    {msg1, Msg1},
                    {msg2, Msg2},
                    {opts, Opts}
                }
            ),
			{Status, _Mod, Func} = message_to_fun(Msg1, Key, Opts),
			?event(
				{resolving, Key,
					{func, Func},
					{msg1, Msg1},
					{msg2, Msg2},
					{opts, Opts}
				}
			),
			% Next, add an option to the Opts map to indicate if we should
			% add the key to the start of the arguments.
			{
				Func,
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
                ?event(
                    converge_result,
                    {
                        load_device_failed,
                        {msg1, Msg1},
                        {msg2, Msg2},
                        {exec_name, ExecName},
                        {exec_class, Class},
                        {exec_exception, Exception},
                        {exec_stacktrace, Stacktrace},
                        {opts, Opts}
                    }
                ),
                % If the device cannot be loaded, we alert the caller.
				handle_error(
                    ExecName,
					loading_device,
					{Class, Exception, Stacktrace},
					Opts
				)
		end,
	resolve_stage(5, ResolvedFunc, Msg1, Msg2, ExecName, NewOpts).
resolve_stage(5, Func, Msg1, Msg2, ExecName, Opts) ->
    ?event(converge_core, {stage, 5, ExecName}, Opts),
	% Execution.
	% First, determine the arguments to pass to the function.
	% While calculating the arguments we unset the add_key option.
	UserOpts = maps:remove(add_key, Opts),
	Args =
		case maps:get(add_key, Opts, false) of
			false -> [Msg1, Msg2, UserOpts];
			Key -> [Key, Msg1, Msg2, UserOpts]
		end,
    % Try to execute the function.
    Res = 
        try
            MsgRes = apply(Func, truncate_args(Func, Args)),
            ?event(
                converge_result,
                {
                    result,
                    {exec_name, ExecName},
                    {msg1, Msg1},
                    {msg2, Msg2},
                    {msg_res, MsgRes},
                    {opts, Opts}
                },
                Opts
            ),
            MsgRes
        catch
            ExecClass:ExecException:ExecStacktrace ->
                ?event(
                    converge_core,
                    {device_call_failed, ExecName, {func, Func}},
                    Opts
                ),
                ?event(
                    converge_result,
                    {
                        exec_failed,
                        {msg1, Msg1},
                        {msg2, Msg2},
                        {exec_name, ExecName},
                        {func, Func},
                        {exec_class, ExecClass},
                        {exec_exception, ExecException},
                        {exec_stacktrace, ExecStacktrace},
                        {opts, Opts}
                    }
                ),
                % If the function call fails, we raise an error in the manner
                % indicated by caller's `#Opts`.
                handle_error(
                    ExecName,
                    device_call,
                    {ExecClass, ExecException, ExecStacktrace},
                    Opts
                )
        end,
    resolve_stage(6, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(6, Msg1, Msg2, {ok, Msg3}, ExecName, Opts) when is_map(Msg3) ->
    ?event(converge_core, {stage, 6, ExecName, cryptographic_linking}, Opts),
    % Cryptographic linking. Now that we have generated the result, we
    % need to cryptographically link the output to its input via a hashpath.
    resolve_stage(7, Msg1, Msg2,
        case hb_opts:get(hashpath, update, Opts#{ only => local }) of
            update -> {ok, hb_path:push(hashpath, Msg3, Msg2)};
            ignore -> {ok, Msg3}
        end,
        ExecName,
        Opts
    );
resolve_stage(6, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(converge_core, {stage, 6, ExecName, abnormal_skip_link}, Opts),
    % Skip cryptographic linking if the result is abnormal.
    resolve_stage(7, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(7, Msg1, Msg2, {ok, Msg3}, ExecName, Opts) ->
    ?event(converge_core, {stage, 7, ExecName, result_caching}, Opts),
    % Result caching: Optionally, cache the result of the computation locally.
    update_cache(Msg1, Msg2, Msg3, Opts),
    resolve_stage(8, Msg1, Msg2, {ok, Msg3}, ExecName, Opts);
resolve_stage(7, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(converge_core, {stage, 7, ExecName, skip_caching}, Opts),
    % Skip result caching if the result is abnormal.
    resolve_stage(8, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(8, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(converge_core, {stage, 8, ExecName}, Opts),
    % Notify processes that requested the resolution while we were executing and
    % unregister ourselves from the group.
    hb_persistent:unregister_notify(ExecName, Res, Opts),
    resolve_stage(9, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(9, _Msg1, Msg2, {ok, Msg3}, ExecName, Opts) ->
    ?event(converge_core, {stage, 9, ExecName}, Opts),
    % Recurse, fork, or terminate.
    #{ <<"Converge">> := #{ <<"Remaining-Path">> := RemainingPath } }
        = hb_private:from_message(Msg2),
	case RemainingPath of
		RecursivePath when RecursivePath =/= undefined ->
			% There are more elements in the path, so we recurse.
			?event({resolution_recursing, {remaining_path, RemainingPath}}),
			resolve(Msg3, Msg2#{ path => RemainingPath }, Opts);
		undefined ->
			% The path resolved to the last element, so we check whether
            % we should fork a new process with Msg3 waiting for messages,
            % or simply return to the caller. We prefer the global option, such
            % that node operators can control whether devices are able to 
            % generate long-running executions.
            case hb_opts:get(spawn_worker, false, Opts#{ prefer => global }) of
                false -> ok;
                true ->
                    % We should spin up a process that will hold `Msg3` 
                    % in memory for future executions.
                    hb_persistent:start_worker(Msg3, Opts)
            end,
            % Resolution has finished successfully, return to the
            % caller.
			?event({resolution_complete, {result, Msg3}, {request, Msg2}}),
            {ok, Msg3}
	end;
resolve_stage(9, _Msg1, _Msg2, OtherRes, ExecName, Opts) ->
    ?event(converge_core, {stage, 9, ExecName, abnormal_return}, Opts),
    OtherRes.

%% @doc Catch all return if we don't have the necessary messages in the cache.
error_not_found(Msg1, Msg2, Opts) ->
    ?event(converge_core, {not_found, {msg1, Msg1}, {msg2, Msg2}}, Opts),
    {
        error,
        #{
            <<"Status">> => <<"Not Found">>,
            <<"body">> => <<"Necessary messages not found in cache">>
        }
    }.

%% @doc Catch all return if the message is invalid.
error_invalid_message(Msg1, Msg2, Opts) ->
    ?event(converge_core, {invalid_message, {msg1, Msg1}, {msg2, Msg2}}, Opts),
    {
        error,
        #{
            <<"Status">> => <<"Forbidden">>,
            <<"body">> => <<"Request contains non-verifiable message.">>
        }
    }.

%% @doc Catch all return if we are in an infinite loop.
error_infinite(_Msg1, _Msg2, _Opts) ->
    {
        error,
        #{
            <<"Status">> => <<"Malformed Request">>,
            <<"body">> => <<"Request creates infinite recursion.">>
        }
    }.

%% @doc Write a resulting M3 message to the cache if requested.
update_cache(Msg1, Msg2, {ok, Msg3}, Opts) ->
    ExecCacheSetting = hb_opts:get(cache, always, Opts),
    M1CacheSetting = dev_message:get(<<"Cache-Control">>, Msg1, Opts),
    M2CacheSetting = dev_message:get(<<"Cache-Control">>, Msg2, Opts),
    case must_cache(ExecCacheSetting, M1CacheSetting, M2CacheSetting) of
        true ->
            case hb_opts:get(async_cache, false, Opts) of
                true ->
                    spawn(fun() ->
                        hb_cache:write_output(Msg1, Msg2, Msg3, Opts)
                    end);
                false ->
                    hb_cache:write_output(Msg1, Msg2, Msg3, Opts)
            end;
        false -> ok
    end;
update_cache(_, _, _, _) -> ok.

%% @doc Takes the `Opts` cache setting, M1, and M2 `Cache-Control` headers, and
%% returns true if the message should be cached.
must_cache(no_cache, _, _) -> false;
must_cache(no_store, _, _) -> false;
must_cache(none, _, _) -> false;
must_cache(_, CC1, CC2) ->
    CC1List = term_to_cache_control_list(CC1),
    CC2List = term_to_cache_control_list(CC2),  
    NoCacheSpecifiers = [no_cache, no_store, no_transform],
    lists:any(
        fun(X) -> lists:member(X, NoCacheSpecifiers) end,
        CC1List ++ CC2List
    ).

%% @doc Convert cache control specifier(s) to a normalized list.
term_to_cache_control_list({error, not_found}) -> [];
term_to_cache_control_list({ok, CC}) -> term_to_cache_control_list(CC);
term_to_cache_control_list(X) when is_list(X) ->
    lists:flatten(lists:map(fun term_to_cache_control_list/1, X));
term_to_cache_control_list(X) when is_binary(X) -> X;
term_to_cache_control_list(X) ->
    hb_path:term_to_path(X).

%% @doc Shortcut for resolving a key in a message without its status if it is
%% `ok`. This makes it easier to write complex logic on top of messages while
%% maintaining a functional style.
%% 
%% Additionally, this function supports the `{as, Device, Msg}` syntax, which
%% allows the key to be resolved using another device to resolve the key,
%% while maintaining the tracability of the `HashPath` of the output message.
%% 
%% Returns the value of the key if it is found, otherwise returns the default
%% provided by the user, or `not_found` if no default is provided.
get(Path, Msg) ->
    get(Path, Msg, default_runtime_opts(Msg)).
get(Path, Msg, Opts) ->
    get(Path, Msg, not_found, Opts).
get(Path, {as, Device, Msg}, Default, Opts) ->
    get(
        Path,
        set(Msg, #{ device => Device }, Opts#{ topic => converge_internal }),
        Default,
        Opts
    );
get(Path, Msg, Default, Opts) ->
	case resolve(Msg, #{ path => Path }, Opts) of
		{ok, Value} -> Value;
		{error, _} -> Default
	end.

%% @doc Shortcut to get the list of keys from a message.
keys(Msg) -> keys(Msg, #{}).
keys(Msg, Opts) -> keys(Msg, Opts, keep).
keys(Msg, Opts, keep) ->
    get(keys, Msg, Opts);
keys(Msg, Opts, remove) ->
    lists:filter(
        fun(Key) -> not lists:member(Key, ?CONVERGE_KEYS) end,
        keys(Msg, Opts, keep)
    ).

%% @doc Shortcut for setting a key in the message using its underlying device.
%% Like the `get/3' function, this function honors the `error_strategy' option.
%% `set' works with maps and recursive paths while maintaining the appropriate
%% `HashPath' for each step.
set(Msg1, Msg2) ->
    set(Msg1, Msg2, #{}).
set(Msg1, RawMsg2, Opts) when is_map(RawMsg2) ->
    Msg2 = maps:without([hashpath, priv], RawMsg2),
    ?event(converge_internal, {set_called, {msg1, Msg1}, {msg2, Msg2}}, Opts),
    % Get the next key to set. 
    case keys(Msg2, Opts#{ hashpath => ignore, topic => converge_internal }) of
        [] -> Msg1;
        [Key|_] ->
            % Get the value to set. Use Converge by default, but fall back to
            % getting via `maps` if it is not found.
            Val =
                case get(Key, Msg2, Opts#{ topic => converge_internal }) of
                    not_found -> maps:get(Key, Msg2);
                    Body -> Body
                end,
            ?event({got_val_to_set, {key, Key}, {val, Val}, {msg2, Msg2}}),
            % Next, set the key and recurse, removing the key from the Msg2.
            set(
                set(Msg1, Key, Val, Opts#{ topic => converge_internal }),
                remove(Msg2, Key, Opts#{ topic => converge_internal }),
                Opts
            )
    end.
set(Msg1, Key, Value, Opts) ->
    % For an individual key, we run deep_set with the key as the path.
    % This handles both the case that the key is a path as well as the case
    % that it is a single key.
    Path = hb_path:term_to_path(Key),
    % ?event(
    %     {setting_individual_key,
    %         {msg1, Msg1},
    %         {key, Key},
    %         {path, Path},
    %         {value, Value}
    %     }
    % ),
    deep_set(Msg1, Path, Value, Opts).

%% @doc Recursively search a map, resolving keys, and set the value of the key
%% at the given path.
deep_set(Msg, [Key], Value, Opts) ->
    %?event({setting_last_key, {key, Key}, {value, Value}}),
    device_set(Msg, Key, Value, Opts);
deep_set(Msg, [Key|Rest], Value, Opts) ->
    case resolve(Msg, Key, Opts) of 
        {ok, SubMsg} ->
            ?event(
                {traversing_deeper_to_set,
                    {current_key, Key},
                    {current_value, SubMsg},
                    {rest, Rest}
                }
            ),
            device_set(Msg, Key, deep_set(SubMsg, Rest, Value, Opts), Opts);
        _ ->
            ?event(
                {creating_new_map,
                    {current_key, Key},
                    {rest, Rest}
                }
            ),
            Msg#{ Key => deep_set(#{}, Rest, Value, Opts) }
    end.

device_set(Msg, Key, Value, Opts) ->
	?event(
        converge_internal,
        {
            calling_device_set,
            {msg, Msg},
            {applying_path, #{ path => set, Key => Value }}
        }
    ),
	Res = hb_util:ok(resolve(Msg, #{ path => set, Key => Value }, Opts), Opts),
	?event(
        converge_internal,
        {device_set_result, Res}
    ),
	Res.

%% @doc Remove a key from a message, using its underlying device.
remove(Msg, Key) -> remove(Msg, Key, #{}).
remove(Msg, Key, Opts) ->
	hb_util:ok(
        resolve(
            Msg,
            #{ path => remove, item => Key },
            Opts#{ topic => converge_internal }
        ),
        Opts
    ).

%% @doc Handle an error in a device call.
handle_error(ExecGroup, Whence, {Class, Exception, Stacktrace}, Opts) ->
    Error = {error, Whence, {Class, Exception, Stacktrace}},
    hb_persistent:unregister_notify(ExecGroup, Error, Opts),
    ?event(converge_core, {handle_error, Error, {opts, Opts}}),
    case maps:get(error_strategy, Opts, throw) of
        throw -> erlang:raise(Class, Exception, Stacktrace);
        _ -> Error
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
%% 2. The device has a `handler' key in its `Dev:info()' map, which is a
%% function that takes a key and returns a function to handle that key. We pass
%% the key as an additional argument to this function.
%% 3. The device has a function of the name `Key', which should be called
%% directly.
%% 4. The device does not implement the key, but does have a default handler
%% for us to call. We pass it the key as an additional argument.
%% 5. The device does not implement the key, and has no default handler. We use
%% the default device to handle the key.
%% Error: If the device is specified, but not loadable, we raise an error.
%%
%% Returns {ok | add_key, Fun} where Fun is the function to call, and add_key
%% indicates that the key should be added to the start of the call's arguments.
message_to_fun(Msg, Key, Opts) ->
    % Get the device module from the message.
	Dev = message_to_device(Msg, Opts),
    Info = info(Dev, Msg, Opts),
    % Is the key exported by the device?
    Exported = is_exported(Info, Key),
	?event(
        converge_devices,
        {message_to_fun,
            {dev, Dev},
            {key, Key},
            {is_exported, Exported},
            {opts, Opts}
        }
    ),
    % Does the device have an explicit handler function?
    case {maps:find(handler, Info), Exported} of
        {{ok, Handler}, true} ->
			% Case 2: The device has an explicit handler function.
			?event(
                converge_devices,
                {handler_found, {dev, Dev}, {key, Key}, {handler, Handler}}
            ),
			{Status, Func} = info_handler_to_fun(Handler, Msg, Key, Opts),
            {Status, Dev, Func};
		_ ->
			?event(converge_devices, {handler_not_found, {dev, Dev}, {key, Key}}),
			case {find_exported_function(Msg, Dev, Key, 3, Opts), Exported} of
				{{ok, Func}, true} ->
					% Case 3: The device has a function of the name `Key`.
					{ok, Dev, Func};
				_ ->
					case {maps:find(default, Info), Exported} of
						{{ok, DefaultFunc}, true} when is_function(DefaultFunc) ->
							% Case 4: The device has a default handler.
                            ?event({found_default_handler, {func, DefaultFunc}}),
							{add_key, Dev, DefaultFunc};
                        {{ok, DefaultMod}, true} when is_atom(DefaultMod) ->
							?event({found_default_handler, {mod, DefaultMod}}),
                            {Status, Func} =
                                message_to_fun(
                                    Msg#{ device => DefaultMod }, Key, Opts
                                ),
                            {Status, Dev, Func};
						_ ->
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
                                    ?event(
                                        {
                                            using_default_device,
                                            {dev, DefaultDev}
                                        }),
                                    message_to_fun(
                                        Msg#{ device => DefaultDev },
                                        Key,
                                        Opts
                                    )
							end
					end
			end
	end.

%% @doc Extract the device module from a message.
message_to_device(Msg, Opts) ->
    case dev_message:get(device, Msg, Opts) of
        {error, not_found} ->
            % The message does not specify a device, so we use the default device.
            default_module();
        {ok, DevID} ->
            case load_device(DevID, Opts) of
                {error, _} ->
                    % Error case: A device is specified, but it is not loadable.
                    throw({error, {device_not_loadable, DevID}});
                {ok, DevMod} -> DevMod
            end
    end.

%% @doc Parse a handler key given by a device's `info'.
info_handler_to_fun(Handler, _Msg, _Key, _Opts) when is_function(Handler) ->
	{add_key, Handler};
info_handler_to_fun(HandlerMap, Msg, Key, Opts) ->
	case maps:find(exclude, HandlerMap) of
		{ok, Exclude} ->
			case lists:member(Key, Exclude) of
				true ->
					{ok, MsgWithoutDevice} =
						dev_message:remove(Msg, #{ item => device }),
					message_to_fun(
						MsgWithoutDevice#{ device => default_module() },
						Key,
						Opts
					);
				false -> {add_key, maps:get(func, HandlerMap)}
			end;
		error -> {add_key, maps:get(func, HandlerMap)}
	end.

%% @doc Find the function with the highest arity that has the given name, if it
%% exists.
%%
%% If the device is a module, we look for a function with the given name.
%%
%% If the device is a map, we look for a key in the map. First we try to find
%% the key using its literal value. If that fails, we cast the key to an atom
%% and try again.
find_exported_function(Msg, Dev, Key, MaxArity, Opts) when is_map(Dev) ->
	case maps:get(Key, Dev, not_found) of
		not_found ->
			case to_key(Key) of
				undefined -> not_found;
				Key ->
					% The key is unchanged, so we return not_found.
					not_found;
				KeyAtom ->
					% The key was cast to an atom, so we try again.
					find_exported_function(Msg, Dev, KeyAtom, MaxArity, Opts)
			end;
		Fun when is_function(Fun) ->
			case erlang:fun_info(Fun, arity) of
				{arity, Arity} when Arity =< MaxArity ->
					case is_exported(Msg, Dev, Key, Opts) of
						true -> {ok, Fun};
						false -> not_found
					end;
				_ -> not_found
			end
	end;
find_exported_function(_Msg, _Mod, _Key, Arity, _Opts) when Arity < 0 ->
    not_found;
find_exported_function(Msg, Mod, Key, Arity, Opts) when not is_atom(Key) ->
	case to_key(Key, Opts) of
		ConvertedKey when is_atom(ConvertedKey) ->
			find_exported_function(Msg, Mod, ConvertedKey, Arity, Opts);
		undefined -> not_found;
		BinaryKey when is_binary(BinaryKey) ->
			not_found
	end;
find_exported_function(Msg, Mod, Key, Arity, Opts) ->
	%?event({finding, {mod, Mod}, {key, Key}, {arity, Arity}}),
	case erlang:function_exported(Mod, Key, Arity) of
		true ->
			case is_exported(Msg, Mod, Key, Opts) of
				true ->
					%?event({found, {ok, fun Mod:Key/Arity}}),
					{ok, fun Mod:Key/Arity};
				false ->
					%?event({result, not_found}),
					not_found
			end;
		false ->
			%?event(
            %     {
            %         find_exported_function_result,
            %         {mod, Mod},
            %         {key, Key},
            %         {arity, Arity},
            %         {result, false}
            %     }
            % ),
			find_exported_function(Msg, Mod, Key, Arity - 1, Opts)
	end.

%% @doc Check if a device is guarding a key via its `exports' list. Defaults to
%% true if the device does not specify an `exports' list. The `info' function is
%% always exported, if it exists. Elements of the `exludes` list are not
%% exported. Note that we check for info _twice_ -- once when the device is
%% given but the info result is not, and once when the info result is given.
%% The reason for this is that `info/3` calls other functions that may need to
%% check if a key is exported, so we must avoid infinite loops. We must, however,
%% also return a consistent result in the case that only the info result is
%% given, so we check for it in both cases.
is_exported(_Msg, _Dev, info, _Opts) -> true;
is_exported(Msg, Dev, Key, Opts) ->
	is_exported(info(Dev, Msg, Opts), Key).
is_exported(_, info) -> true;
is_exported(Info = #{ excludes := Excludes }, Key) ->
    case lists:member(to_key(Key), lists:map(fun to_key/1, Excludes)) of
        true -> false;
        false -> is_exported(maps:remove(excludes, Info), Key)
    end;
is_exported(#{ exports := Exports }, Key) ->
    lists:member(to_key(Key), lists:map(fun to_key/1, Exports));
is_exported(_Info, _Key) -> true.

%% @doc Convert a key to an atom if it already exists in the Erlang atom table,
%% or to a binary otherwise.
to_key(Key) -> to_key(Key, #{ error_strategy => throw }).
to_key(Key, _Opts) when byte_size(Key) == 43 -> Key;
to_key(Key, Opts) ->
    % If the `atom_keys' option is set, we try to convert the key to an atom.
    % If this fails, we fall back to using the binary representation.
    AtomKeys = hb_opts:get(atom_keys, true, Opts),
    if AtomKeys ->
        try to_atom_unsafe(Key)
        catch _Type:_:_Trace -> key_to_binary(Key, Opts)
        end;
        true -> key_to_binary(Key, Opts)
    end.

%% @doc Convert a key to its binary representation.
key_to_binary(Key) -> key_to_binary(Key, #{}).
key_to_binary(Key, _Opts) when is_binary(Key) -> Key;
key_to_binary(Key, _Opts) when is_atom(Key) -> atom_to_binary(Key);
key_to_binary(Key, _Opts) when is_list(Key) -> list_to_binary(Key);
key_to_binary(Key, _Opts) when is_integer(Key) -> integer_to_binary(Key).

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
load_device(Map, _Opts) when is_map(Map) -> {ok, Map};
load_device(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
load_device(ID, Opts) when is_binary(ID) and byte_size(ID) == 43 ->
	case hb_opts:get(load_remote_devices) of
		true ->
			{ok, Msg} = hb_cache:read(maps:get(store, Opts), ID),
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
								lists:keyfind(
                                    <<"Module-Name">>,
                                    1,
                                    Msg#tx.tags
                                ),
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
load_device(ID, Opts) ->
    case maps:get(ID, hb_opts:get(preloaded_devices), unsupported) of
        unsupported -> {error, module_not_admissable};
        Mod -> load_device(Mod, Opts)
    end.

%% @doc Get the info map for a device, optionally giving it a message if the
%% device's info function is parameterized by one.
info(Msg, Opts) ->
    info(message_to_device(Msg, Opts), Msg, Opts).
info(DevMod, Msg, Opts) ->
	%?event({calculating_info, {dev, DevMod}, {msg, Msg}}),
	case find_exported_function(Msg, DevMod, info, 1, Opts) of
		{ok, Fun} ->
			Res = apply(Fun, truncate_args(Fun, [Msg, Opts])),
			% ?event({
            %     info_result,
            %     {dev, DevMod},
            %     {args, truncate_args(Fun, [Msg])},
            %     {result, Res}
            % }),
			Res;
		not_found -> #{}
	end.

%% @doc The default runtime options for a message. At the moment the `Message1'
%% but it is included such that we can modulate the options based on the message
%% if needed in the future.
default_runtime_opts(_Msg1) ->
    #{
        error_strategy => throw
    }.

%% @doc The default device is the identity device, which simply returns the
%% value associated with any key as it exists in its Erlang map. It should also
%% implement the `set' key, which returns a `Message3' with the values changed
%% according to the `Message2' passed to it.
default_module() -> dev_message.

%%% Tests

resolve_simple_test() ->
    Res = hb_converge:resolve(#{ a => 1 }, a, #{}),
    ?assertEqual({ok, 1}, Res).

resolve_key_twice_test() ->
    % Ensure that the same message can be resolved again.
    % This is not as trivial as it may seem, because resolutions are cached and
    % de-duplicated.
    ?assertEqual({ok, 1}, hb_converge:resolve(#{ a => 1 }, a, #{})),
    ?assertEqual({ok, 1}, hb_converge:resolve(#{ a => 1 }, a, #{})).

resolve_from_multiple_keys_test() ->
    ?assertEqual(
        {ok, [a]},
        hb_converge:resolve(#{ a => 1, "priv_a" => 2 }, keys, #{})
    ).

resolve_path_element_test() ->
    ?assertEqual(
        {ok, [test_path]},
        hb_converge:resolve(#{ path => [test_path] }, path, #{})
    ),
    ?assertEqual(
        {ok, [a]},
        hb_converge:resolve(#{ <<"Path">> => [a] }, <<"Path">>, #{})
    ).

key_to_binary_test() ->
    ?assertEqual(<<"a">>, hb_converge:key_to_binary(a)),
    ?assertEqual(<<"a">>, hb_converge:key_to_binary(<<"a">>)),
    ?assertEqual(<<"a">>, hb_converge:key_to_binary("a")).

resolve_binary_key_test() ->
    ?assertEqual(
        {ok, 1},
        hb_converge:resolve(#{ a => 1 }, <<"a">>, #{})
    ),
    ?assertEqual(
        {ok, 1},
        hb_converge:resolve(
            #{
                <<"Test-Header">> => 1 },
                <<"Test-Header">>,
            #{}
        )
    ).

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

%% @doc Create a simple test device that implements the default handler.
gen_default_device() ->
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
    }.

%% @doc Create a simple test device that implements the handler key.
gen_handler_device() ->
    #{
        info =>
            fun() ->
                #{
                    handler =>
                        fun(set, M1, M2, Opts) ->
                            dev_message:set(M1, M2, Opts);
                        (_, _, _, _) ->
                            {ok, <<"HANDLER VALUE">>}
                        end
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
        hb_converge:resolve(
            Msg,
            #{
                path => key_using_only_state,
                msg_key => <<"2">> % Param message, which is ignored
            },
            #{}
        )
    ),
    ?assertEqual(
        {ok, <<"13">>},
        hb_converge:resolve(
            Msg,
            #{
                path => key_using_state_and_msg,
                msg_key => <<"3">> % Param message, with value to add
            },
            #{}
        )
    ),
    ?assertEqual(
        {ok, <<"1337">>},
        hb_converge:resolve(
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
            device => gen_handler_device(),
            test_key => <<"BAD">>
        },
    ?assertEqual(
        {ok, <<"HANDLER VALUE">>},
        hb_converge:resolve(Msg, test_key, #{})
    ).

device_with_default_handler_function_test() ->
    Msg =
        #{
            device => gen_default_device()
        },
    ?assertEqual(
        {ok, <<"STATE">>},
        hb_converge:resolve(Msg, state_key, #{})
    ),
    ?assertEqual(
        {ok, <<"DEFAULT">>},
        hb_converge:resolve(Msg, any_random_key, #{})
    ).

basic_get_test() ->
    Msg = #{ key1 => <<"value1">>, key2 => <<"value2">> },
    ?assertEqual(<<"value1">>, hb_converge:get(key1, Msg)),
    ?assertEqual(<<"value2">>, hb_converge:get(key2, Msg)),
    ?assertEqual(<<"value2">>, hb_converge:get(<<"key2">>, Msg)),
    ?assertEqual(<<"value2">>, hb_converge:get([<<"key2">>], Msg)).

recursive_get_test() ->
    Msg = #{ key1 => <<"value1">>, key2 => #{ key3 => <<"value3">> } },
    ?assertEqual(
        {ok, <<"value1">>},
        hb_converge:resolve(Msg, #{ path => key1 }, #{})
    ),
    ?assertEqual(<<"value1">>, hb_converge:get(key1, Msg)),
    ?assertEqual(
        {ok, <<"value3">>},
        hb_converge:resolve(Msg, #{ path => [key2, key3] }, #{})
    ),
    ?assertEqual(<<"value3">>, hb_converge:get([key2, key3], Msg)),
    ?assertEqual(<<"value3">>, hb_converge:get(<<"key2/key3">>, Msg)).

basic_set_test() ->
    Msg = #{ key1 => <<"value1">>, key2 => <<"value2">> },
    UpdatedMsg = hb_converge:set(Msg, #{ key1 => <<"new_value1">> }),
    ?event({set_key_complete, {key, key1}, {value, <<"new_value1">>}}),
    ?assertEqual(<<"new_value1">>, hb_converge:get(key1, UpdatedMsg)),
    ?assertEqual(<<"value2">>, hb_converge:get(key2, UpdatedMsg)).

get_with_device_test() ->
    Msg =
        #{
            device => generate_device_with_keys_using_args(),
            state_key => <<"STATE">>
        },
    ?assertEqual(<<"STATE">>, hb_converge:get(state_key, Msg)),
    ?assertEqual(<<"STATE">>, hb_converge:get(key_using_only_state, Msg)).

get_as_with_device_test() ->
    Msg =
        #{
            device => gen_handler_device(),
            test_key => <<"ACTUAL VALUE">>
        },
    ?assertEqual(
        <<"HANDLER VALUE">>,
        hb_converge:get(test_key, Msg)
    ),
    ?assertEqual(
        <<"ACTUAL VALUE">>,
        hb_converge:get(test_key, {as, dev_message, Msg})
    ).

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
    ?assertEqual(<<"STATE">>, hb_converge:get(state_key, Msg)),
    SetOnce = hb_converge:set(Msg, #{ state_key => <<"SET_ONCE">> }),
    ?assertEqual(1, hb_converge:get(set_count, SetOnce)),
    SetTwice = hb_converge:set(SetOnce, #{ state_key => <<"SET_TWICE">> }),
    ?assertEqual(2, hb_converge:get(set_count, SetTwice)),
    ?assertEqual(<<"STATE">>, hb_converge:get(state_key, SetTwice)).

deep_set_test() ->
    % First validate second layer changes are handled correctly.
    Msg0 = #{ a => #{ b => 1 } },
    ?assertMatch(#{ a := #{ b := 2 } },
        hb_converge:set(Msg0, [a, b], 2, #{})),
    % Now validate deeper layer changes are handled correctly.
    Msg = #{ a => #{ b => #{ c => 1 } } },
    ?assertMatch(#{ a := #{ b := #{ c := 2 } } },
        hb_converge:set(Msg, [a, b, c], 2, #{})).

deep_set_new_messages_test() ->
    % Test that new messages are created when the path does not exist.
    Msg0 = #{ a => #{ b => #{ c => 1 } } },
    Msg1 = hb_converge:set(Msg0, <<"d/e">>, 3, #{ hashpath => ignore }),
    Msg2 = hb_converge:set(Msg1, <<"d/f">>, 4, #{ hashpath => ignore }),
    ?assertMatch(
        #{ a := #{ b := #{ c := 1 } }, d := #{ e := 3, f := 4 } }, Msg2),
    Msg3 = hb_converge:set(
        Msg2,
        #{ 
            <<"z/a">> => 0,
            <<"z/b">> => 1,
            <<"z/y/x">> => 2
         },
        #{ hashpath => ignore }
    ),
    ?assertMatch(
        #{
            a := #{ b := #{ c := 1 } }, d := #{ e := 3, f := 4 },
            z := #{ a := 0, b := 1, y := #{ x := 2 } }
        },
        Msg3
    ).

deep_set_with_device_test() ->
    Device = #{
        set =>
            fun(Msg1, Msg2) ->
                % A device where the set function modifies the key
                % and adds a modified flag.
                {Key, Val} =
                    hd(maps:to_list(maps:without([path, priv], Msg2))),
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
    A = hb_converge:get(a, Outer),
    B = hb_converge:get(b, A),
    C = hb_converge:get(c, B),
    ?assertEqual(2, C),
    ?assertEqual(true, hb_converge:get(modified, Outer)),
    ?assertEqual(false, hb_converge:get(modified, A)),
    ?assertEqual(true, hb_converge:get(modified, B)).

device_exports_test() ->
	Msg = #{ device => dev_message },
	?assert(is_exported(Msg, dev_message, info, #{})),
	?assert(is_exported(Msg, dev_message, set, #{})),
	?assert(is_exported(Msg, dev_message, not_explicitly_exported, #{})),
	Dev = #{
		info => fun() -> #{ exports => [set] } end,
		set => fun(_, _) -> {ok, <<"SET">>} end
	},
	Msg2 = #{ device => Dev },
	?assert(is_exported(Msg2, Dev, info, #{})),
	?assert(is_exported(Msg2, Dev, set, #{})),
	?assert(not is_exported(Msg2, Dev, not_exported, #{})),
    Dev2 = #{
        info =>
            fun() ->
                #{
                    exports => [test1, <<"Test2">>],
                    handler =>
                        fun() ->
                            {ok, <<"Handler-Value">>}
                        end
                }
            end
    },
    Msg3 = #{ device => Dev2, <<"Test1">> => <<"BAD1">>, test3 => <<"GOOD3">> },
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(test1, Msg3)),
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(test2, Msg3)),
    ?assertEqual(<<"GOOD3">>, hb_converge:get(test3, Msg3)),
    ?assertEqual(<<"GOOD4">>,
        hb_converge:get(
            <<"Test4">>,
            hb_converge:set(Msg3, <<"Test4">>, <<"GOOD4">>, #{})
        )
    ),
    ?assertEqual(not_found, hb_converge:get(test5, Msg3)).

device_excludes_test() ->
    % Create a device that returns an identifiable message for any key, but also
    % sets excludes to [set], such that the message can be modified using the 
    % default handler.
    Dev = #{
        info =>
            fun() ->
                #{
                    excludes => [set],
                    handler => fun() -> {ok, <<"Handler-Value">>} end
                }
            end
    },
    Msg = #{ device => Dev, <<"Test-Key">> => <<"Test-Value">> },
    ?assert(is_exported(Msg, Dev, <<"Test-Key2">>, #{})),
    ?assert(not is_exported(Msg, Dev, set, #{})),
    ?assertEqual(<<"Handler-Value">>, hb_converge:get(<<"Test-Key2">>, Msg)),
    ?assertMatch(#{ <<"Test-Key2">> := 2 },
        hb_converge:set(Msg, <<"Test-Key2">>, 2, #{})).

denormalized_device_key_test() ->
	Msg = #{ <<"Device">> => dev_test },
	?assertEqual(dev_test, hb_converge:get(device, Msg)),
	?assertEqual(dev_test, hb_converge:get(<<"Device">>, Msg)),
	?assertEqual({module, dev_test},
		erlang:fun_info(
            element(3, message_to_fun(Msg, test_func, #{})),
            module
        )
    ).
