%%% @doc This module is the root of the device call logic of the 
%%% Converge Protocol in HyperBEAM.
%%% 
%%% At the implementation level, every message is simply a collection of keys,
%%% dictated by its `Device', that can be resolved in order to yield their
%%% values. Each key may return another message or a raw value:
%%% 
%%% 	`converge(Message1, Message2) -> {Status, Message3}'
%%% 
%%% Under-the-hood, `Converge(Message1, Message2)' leads to the evaluation of
%%% `DeviceMod:PathPart(Message1, Message2)', which defines the user compute
%%% to be performed. If `Message1' does not specify a device, `dev_message' is
%%% assumed. The key to resolve is specified by the `Path' field of the message.
%%% 
%%% After each output, the `HashPath' is updated to include the `Message2'
%%% that was executed upon it.
%%% 
%%% Because each message implies a device that can resolve its keys, as well
%%% as generating a merkle tree of the computation that led to the result,
%%% you can see Converge Protocol as a system for cryptographically chaining 
%%% the execution of `combinators'. See `docs/converge-protocol.md' for more 
%%% information about Converge.
%%% 
%%% The `Fun(Message1, Message2)' pattern is repeated throughout the HyperBEAM 
%%% codebase, sometimes with `MessageX' replaced with `MX' or `MsgX' for brevity.
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
%%% ```
%%%     DevMod:ExportedFunc : Key resolution functions. All are assumed to be
%%%                           device keys (thus, present in every message that
%%%                           uses it) unless specified by `DevMod:info()'.
%%%                           Each function takes a set of parameters
%%%                           of the form `DevMod:KeyHandler(Msg1, Msg2, Opts)'.
%%%                           Each of these arguments can be ommitted if not
%%%                           needed. Non-exported functions are not assumed
%%%                           to be device keys.
%%%
%%%     DevMod:info : Optional. Returns a map of options for the device. All 
%%%                   options are optional and assumed to be the defaults if 
%%%                   not specified. This function can accept a `Message1' as 
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
%%%                    the `dev_message' device, which contains general keys for 
%%%                    interacting with messages.
%%% 
%%%     info/default_mod : A different device module that should be used to
%%%                    handle all keys that are not explicitly implemented
%%%                    by the device. Defaults to the `dev_message' device.
%%% 
%%%     info/grouper : A function that returns the concurrency 'group' name for
%%%                    an execution. Executions with the same group name will
%%%                    be executed by sending a message to the associated process
%%%                    and waiting for a response. This allows you to control 
%%%                    concurrency of execution and to allow executions to share
%%%                    in-memory state as applicable. Default: A derivation of
%%%                    Msg1+Msg2. This means that concurrent calls for the same
%%%                    output will lead to only a single execution.
%%% 
%%%     info/worker : A function that should be run as the 'server' loop of
%%%                   the executor for interactions using the device.
%%% 
%%% The HyperBEAM resolver also takes a number of runtime options that change
%%% the way that the environment operates:
%%% 
%%% `update_hashpath':  Whether to add the `Msg2' to `HashPath' for the `Msg3'.
%%% 					Default: true.
%%% `add_key':          Whether to add the key to the start of the arguments.
%%% 					Default: `<not set>'.
%%% '''
-module(hb_converge).
%%% Main Converge API:
-export([resolve/2, resolve/3, resolve_many/2]).
-export([normalize_key/1, normalize_key/2, normalize_keys/1]).
-export([message_to_fun/3, message_to_device/2, load_device/2, find_exported_function/5]).
%%% Shortcuts and tools:
-export([info/2, keys/1, keys/2, keys/3, truncate_args/2]).
-export([get/2, get/3, get/4, get_first/2, get_first/3]).
-export([set/2, set/3, set/4, remove/2, remove/3]).
%%% Exports for tests in hb_converge_tests.erl:
-export([deep_set/4, is_exported/4]).
-include("include/hb.hrl").

-define(NON_RECURSIVE_OPTS, [add_key, force_message, cache_control]).

%% @doc Get the value of a message's key by running its associated device
%% function. Optionally, takes options that control the runtime environment. 
%% This function returns the raw result of the device function call:
%% `{ok | error, NewMessage}.'
%% The resolver is composed of a series of discrete phases:
%%      1: Normalization.
%%      2: Cache lookup.
%%      3: Validation check.
%%      4: Persistent-resolver lookup.
%%      5: Device lookup.
%%      6: Execution.
%%      7: Cryptographic linking.
%%      8: Result caching.
%%      9: Notify waiters.
%%     10: Fork worker.
%%     11: Recurse or terminate.

resolve(SingletonMsg, Opts) when is_map(SingletonMsg) ->
    resolve_many(hb_singleton:from(SingletonMsg), Opts).

resolve(Msg1, Path, Opts) when not is_map(Path) ->
    resolve(Msg1, #{ <<"path">> => Path }, Opts);
resolve(Msg1, Msg2, Opts) ->
    PathParts = hb_path:from_message(request, Msg2),
    ?event(converge_core, {stage, 1, prepare_multimessage_resolution, {path_parts, PathParts}}),
    MessagesToExec = [ Msg2#{ <<"path">> => Path } || Path <- PathParts ],
    ?event(converge_core, {stage, 1, prepare_multimessage_resolution, {messages_to_exec, MessagesToExec}}),
    resolve_many([Msg1 | MessagesToExec], Opts).

%% @doc Resolve a list of messages in sequence. Take the output of the first
%% message as the input for the next message. Once the last message is resolved,
%% return the result.
resolve_many([Msg3], _Opts) ->
    ?event(converge_core, {stage, 11, resolve_complete, Msg3}),
    {ok, Msg3};
resolve_many([Msg1, Msg2 | MsgList], Opts) ->
    ?event(converge_core, {stage, 0, resolve_many, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    case resolve_stage(1, Msg1, Msg2, Opts) of
        {ok, Msg3} ->
            ?event(converge_core,
                {
                    stage,
                    11,
                    resolved_message_of_many,
                    {msg3, Msg3},
                    {opts, Opts}
                }
            ),
            resolve_many([Msg3 | MsgList], Opts);
        Res ->
            ?event(converge_core, {stage, 11, resolve_many_terminating_early, Res}),
            Res
    end.

resolve_stage(1, Msg1, Msg2, Opts) when is_list(Msg1) ->
    % Normalize lists to numbered maps (base=1) if necessary.
    ?event(converge_core, {stage, 1, list_normalize}, Opts),
    resolve_stage(1,
        normalize_keys(Msg1),
        Msg2,
        Opts
    );
resolve_stage(1, Msg1, NonMapMsg2, Opts) when not is_map(NonMapMsg2) ->
    ?event(converge_core, {stage, 1, path_normalize}),
    resolve_stage(1, Msg1, #{ <<"path">> => NonMapMsg2 }, Opts);
resolve_stage(1, Msg1, #{ <<"path">> := {as, DevID, Msg2} }, Opts) ->
    % Set the device to the specified `DevID' and resolve the message.
    ?event(converge_core, {stage, 1, setting_device, {dev, DevID}}, Opts),
    Msg1b = set(Msg1, <<"device">>, DevID, Opts),
    ?event(converge_debug, {message_as, Msg1b, {executing_path, Msg2}}, Opts),
    % Recurse with the modified message. The hashpath will have been updated
    % to include the device ID, if requested. Simply return if the path is empty.
    case hb_path:from_message(request, Msg2) of
        undefined -> {ok, Msg1b};
        _ -> 
            ?event(converge_debug,
                {resolve_as_subpath,
                    {msg1, Msg1b},
                    {msg2, Msg2},
                    {opts, Opts}},
                Opts
            ),
            resolve(Msg1b, Msg2, Opts)
    end;
resolve_stage(1, RawMsg1, RawMsg2, Opts) ->
    % Normalize the path to a private key containing the list of remaining
    % keys to resolve.
    ?event(converge_core, {stage, 1, normalize}, Opts),
    Msg1 = normalize_keys(RawMsg1),
    Msg2 = normalize_keys(RawMsg2),
    resolve_stage(2, Msg1, Msg2, Opts);
resolve_stage(2, Msg1, Msg2, Opts) ->
    ?event(converge_core, {stage, 2, cache_lookup}, Opts),
    % Lookup request in the cache. If we find a result, return it.
    % If we do not find a result, we continue to the next stage,
    % unless the cache lookup returns `halt' (the user has requested that we 
    % only return a result if it is already in the cache).
    case hb_cache_control:maybe_lookup(Msg1, Msg2, Opts) of
        {ok, Msg3} ->
            ?event(converge_core, {stage, 2, cache_hit, {msg3, Msg3}, {opts, Opts}}),
            {ok, Msg3};
        {continue, NewMsg1, NewMsg2} ->
            resolve_stage(3, NewMsg1, NewMsg2, Opts);
        {error, CacheResp} -> {error, CacheResp}
    end;
resolve_stage(3, Msg1, Msg2, Opts) when not is_map(Msg1) or not is_map(Msg2) ->
    % Validation check: If the messages are not maps, we cannot find a key
    % in them, so return not_found.
    ?event(converge_core, {stage, 3, validation_check_type_error}, Opts),
    {error, not_found};
resolve_stage(3, Msg1, Msg2, Opts) ->
    ?event(converge_core, {stage, 3, validation_check}, Opts),
    % Validation check: Check if the message is valid.
    %Msg1Valid = (hb_message:signers(Msg1) == []) orelse hb_message:verify(Msg1),
    %Msg2Valid = (hb_message:signers(Msg2) == []) orelse hb_message:verify(Msg2),
    ?no_prod("Enable message validity checks!"),
    case {true, true} of
        _ -> resolve_stage(4, Msg1, Msg2, Opts);
        _ -> error_invalid_message(Msg1, Msg2, Opts)
    end;
resolve_stage(4, Msg1, Msg2, Opts) ->
    ?event(converge_core, {stage, 4, persistent_resolver_lookup}, Opts),
    % Persistent-resolver lookup: Search for local (or Distributed
    % Erlang cluster) processes that are already performing the execution.
    % Before we search for a live executor, we check if the device specifies 
    % a function that tailors the 'group' name of the execution. For example, 
    % the `dev_process' device 'groups' all calls to the same process onto
    % calls to a single executor. By default, `{Msg1, Msg2}' is used as the
    % group name.
    case hb_persistent:find_or_register(Msg1, Msg2, Opts) of
        {leader, ExecName} ->
            % We are the leader for this resolution. Continue to the next stage.
            case hb_opts:get(spawn_worker, false, Opts) of
                true -> ?event(worker_spawns, {will_become, ExecName});
                _ -> ok
            end,
            resolve_stage(5, Msg1, Msg2, ExecName, Opts);
        {wait, Leader} ->
            % There is another executor of this resolution in-flight.
            % Bail execution, register to receive the response, then
            % wait.
            case hb_persistent:await(Leader, Msg1, Msg2, Opts) of
                {error, leader_died} ->
                    ?event(
                        converge_core,
                        {leader_died_during_wait,
                            {leader, Leader},
                            {msg1, Msg1},
                            {msg2, Msg2},
                            {opts, Opts}
                        },
                        Opts
                    ),
                    % Re-try again if the group leader already finished it's work
                    resolve_stage(4, Msg1, Msg2, Opts);
                Res ->
                    % Now that we have the result, we can skip right to potential
                    % recursion (step 11).
                    Res
            end;
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
                    resolve_stage(5, Msg1, Msg2, GroupName, Opts);
                false ->
                    % We are not OK with infinite loops, so we raise an error.
                    error_infinite(Msg1, Msg2, Opts)
            end
    end.
resolve_stage(5, Msg1, Msg2, ExecName, Opts) ->
    ?event(converge_core, {stage, 5, device_lookup}),
    % Device lookup: Find the Erlang function that should be utilized to 
    % execute Msg2 on Msg1.
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
				{found_func_for_exec,
                    {key, Key},
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
				error_execution(
                    ExecName,
                    Msg2,
					loading_device,
					{Class, Exception, Stacktrace},
					Opts
				)
		end,
	resolve_stage(6, ResolvedFunc, Msg1, Msg2, ExecName, NewOpts).
resolve_stage(6, Func, Msg1, Msg2, ExecName, Opts) ->
    ?event(converge_core, {stage, 6, ExecName, execution}, Opts),
	% Execution.
	% First, determine the arguments to pass to the function.
	% While calculating the arguments we unset the add_key option.
	UserOpts1 =
        maps:without(
            ?NON_RECURSIVE_OPTS,
            Opts
        ),
    % Unless the user has explicitly requested recursive spawning, we
    % unset the spawn_worker option so that we do not spawn a new worker
    % for every resulting execution.
    UserOpts2 =
        case maps:get(spawn_worker, UserOpts1, false) of
            recursive -> UserOpts1;
            _ -> maps:remove(spawn_worker, UserOpts1)
        end,
	Args =
		case maps:get(add_key, Opts, false) of
			false -> [Msg1, Msg2, UserOpts2];
			Key -> [Key, Msg1, Msg2, UserOpts2]
		end,
    % Try to execute the function.
    Res = 
        try
            MsgRes =
                maybe_force_message(
                    apply(Func, truncate_args(Func, Args)),
                    Opts
                ),
            ?event(
                converge_result,
                {
                    result,
                    {exec_name, ExecName},
                    {msg1, Msg1},
                    {msg2, Msg2},
                    {msg3, MsgRes},
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
                        {exec_stacktrace, erlang:process_info(self(), backtrace)},
                        {opts, Opts}
                    }
                ),
                % If the function call fails, we raise an error in the manner
                % indicated by caller's `#Opts'.
                error_execution(
                    ExecName,
                    Msg2,
                    device_call,
                    {ExecClass, ExecException, ExecStacktrace},
                    Opts
                )
        end,
    resolve_stage(7, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(7, Msg1, Msg2, {ok, Msg3}, ExecName, Opts) when is_map(Msg3) ->
    ?event(converge_core, {stage, 7, ExecName, generate_hashpath}, Opts),
    % Cryptographic linking. Now that we have generated the result, we
    % need to cryptographically link the output to its input via a hashpath.
    resolve_stage(8, Msg1, Msg2,
        case hb_opts:get(hashpath, update, Opts#{ only => local }) of
            update ->
                Priv = hb_private:from_message(Msg3),
                HP = hb_path:hashpath(Msg1, Msg2, Opts),
                if not is_binary(HP) or not is_map(Priv) ->
                    throw({invalid_hashpath, {hp, HP}, {msg3, Msg3}});
                true ->
                    {ok, Msg3#{ <<"priv">> => Priv#{ <<"hashpath">> => HP } }}
                end;
            reset ->
                Priv = hb_private:from_message(Msg3),
                {ok, Msg3#{ <<"priv">> => maps:without([<<"hashpath">>], Priv) }};
            ignore ->
                Priv = hb_private:from_message(Msg3),
                if not is_map(Priv) ->
                    throw({invalid_private_message, {msg3, Msg3}});
                true ->
                    {ok, Msg3}
                end
        end,
        ExecName,
        Opts
    );
resolve_stage(7, Msg1, Msg2, {Status, Msg3}, ExecName, Opts) when is_map(Msg3) ->
    ?event(converge_core, {stage, 7, ExecName, abnormal_status_reset_hashpath}, Opts),
    ?event(hashpath, {resetting_hashpath_msg3, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    % Skip cryptographic linking and reset the hashpath if the result is abnormal.
    Priv = hb_private:from_message(Msg3),
    resolve_stage(
        8, Msg1, Msg2,
        {Status, Msg3#{ <<"priv">> => maps:without([<<"hashpath">>], Priv) }},
        ExecName, Opts);
resolve_stage(7, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(converge_core, {stage, 7, ExecName, non_map_result_skipping_hash_path}, Opts),
    % Skip cryptographic linking and continue if we don't have a map that can have
    % a hashpath at all.
    resolve_stage(8, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(8, Msg1, Msg2, {ok, Msg3}, ExecName, Opts) ->
    ?event(converge_core, {stage, 8, ExecName, result_caching}, Opts),
    % Result caching: Optionally, cache the result of the computation locally.
    hb_cache_control:maybe_store(Msg1, Msg2, Msg3, Opts),
    resolve_stage(9, Msg1, Msg2, {ok, Msg3}, ExecName, Opts);
resolve_stage(8, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(converge_core, {stage, 8, ExecName, abnormal_status_skip_caching}, Opts),
    % Skip result caching if the result is abnormal.
    resolve_stage(9, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(9, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(converge_core, {stage, 9, ExecName}, Opts),
    % Notify processes that requested the resolution while we were executing and
    % unregister ourselves from the group.
    hb_persistent:unregister_notify(ExecName, Msg2, Res, Opts),
    resolve_stage(10, Msg1, Msg2, Res, ExecName, Opts);
resolve_stage(10, _Msg1, _Msg2, {ok, Msg3} = Res, ExecName, Opts) ->
    ?event(converge_core, {stage, 10, ExecName, maybe_spawn_worker}, Opts),
    % Check if we should spawn a worker for the current execution
    case {is_map(Msg3), hb_opts:get(spawn_worker, false, Opts#{ prefer => local })} of
        {A, B} when (A == false) or (B == false) ->
            Res;
        {_, _} ->
            % Spawn a worker for the current execution
            WorkerPID = hb_persistent:start_worker(ExecName, Msg3, Opts),
            hb_persistent:forward_work(WorkerPID, Opts),
            Res
    end;
resolve_stage(10, _Msg1, _Msg2, OtherRes, ExecName, Opts) ->
    ?event(converge_core, {stage, 10, ExecName, abnormal_status_skip_spawning}, Opts),
    OtherRes.

%% @doc Catch all return if the message is invalid.
error_invalid_message(Msg1, Msg2, Opts) ->
    ?event(
        converge_core,
        {error, {type, invalid_message},
            {msg1, Msg1},
            {msg2, Msg2},
            {opts, Opts}
        },
        Opts
    ),
    {
        error,
        #{
            <<"status">> => 400,
            <<"body">> => <<"Request contains non-verifiable message.">>
        }
    }.

%% @doc Catch all return if we are in an infinite loop.
error_infinite(Msg1, Msg2, Opts) ->
    ?event(
        converge_core,
        {error, {type, infinite_recursion},
            {msg1, Msg1},
            {msg2, Msg2},
            {opts, Opts}
        },
        Opts
    ),
    ?trace(),
    {
        error,
        #{
            <<"status">> => 508,
            <<"body">> => <<"Request creates infinite recursion.">>
        }
    }.

error_invalid_intermediate_status(Msg1, Msg2, Msg3, RemainingPath, Opts) ->
    ?event(
        converge_core,
        {error, {type, invalid_intermediate_status},
            {msg2, Msg2},
            {msg3, Msg3},
            {remaining_path, RemainingPath},
            {opts, Opts}
        },
        Opts
    ),
    ?event(converge_result, 
        {intermediate_failure, {msg1, Msg1},
            {msg2, Msg2}, {msg3, Msg3},
            {remaining_path, RemainingPath}, {opts, Opts}}),
    {
        error,
        #{
            <<"status">> => 422,
            <<"body">> => Msg3,
            <<"key">> => maps:get(<<"path">>, Msg2, <<"Key unknown.">>),
            <<"remaining-path">> => RemainingPath
        }
    }.

%% @doc Handle an error in a device call.
error_execution(ExecGroup, Msg2, Whence, {Class, Exception, Stacktrace}, Opts) ->
    Error = {error, Whence, {Class, Exception, Stacktrace}},
    hb_persistent:unregister_notify(ExecGroup, Msg2, Error, Opts),
    ?event(converge_core, {handle_error, Error, {opts, Opts}}),
    case hb_opts:get(error_strategy, throw, Opts) of
        throw -> erlang:raise(Class, Exception, Stacktrace);
        _ -> Error
    end.

%% @doc Force the result of a device call into a message if the result is not
%% requested by the `Opts'.
maybe_force_message({Status, Res}, Opts) ->
    case hb_opts:get(force_message, false, Opts) and not is_map(Res) of
        true when is_list(Res) -> {Status, normalize_keys(Res)};
        true -> {Status, #{ <<"body">> => Res }};
        false -> {Status, Res}
    end.

%% @doc Shortcut for resolving a key in a message without its status if it is
%% `ok'. This makes it easier to write complex logic on top of messages while
%% maintaining a functional style.
%% 
%% Additionally, this function supports the `{as, Device, Msg}' syntax, which
%% allows the key to be resolved using another device to resolve the key,
%% while maintaining the tracability of the `HashPath' of the output message.
%% 
%% Returns the value of the key if it is found, otherwise returns the default
%% provided by the user, or `not_found' if no default is provided.
get(Path, Msg) ->
    get(Path, Msg, #{}).
get(Path, Msg, Opts) ->
    get(Path, Msg, not_found, Opts).
get(Path, {as, Device, Msg}, Default, Opts) ->
    get(
        Path,
        set(
            Msg,
            #{ <<"device">> => Device },
            internal_opts(Opts)
        ),
        Default,
        Opts
    );
get(Path, Msg, Default, Opts) ->
	case resolve(Msg, #{ <<"path">> => Path }, Opts#{ spawn_worker => false }) of
		{ok, Value} -> Value;
		{error, _} -> Default
	end.

%% @doc take a sequence of base messages and paths, then return the value of the
%% first message that can be resolved using a path.
get_first(Paths, Opts) -> get_first(Paths, not_found, Opts).
get_first([], Default, _Opts) -> Default;
get_first([{Base, Path}|Msgs], Default, Opts) ->
    case get(Path, Base, Opts) of
        not_found -> get_first(Msgs, Default, Opts);
        Value -> Value
    end.

%% @doc Shortcut to get the list of keys from a message.
keys(Msg) -> keys(Msg, #{}).
keys(Msg, Opts) -> keys(Msg, Opts, keep).
keys(Msg, Opts, keep) ->
    try lists:map(fun normalize_key/1, get(<<"keys">>, Msg, Opts))
    catch
        A:B:C ->
            throw({cannot_get_keys, {msg, Msg}, {opts, Opts}, {error, {A, B}}})
    end;
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
set(RawMsg1, RawMsg2, Opts) when is_map(RawMsg2) ->
    Msg1 = normalize_keys(RawMsg1),
    Msg2 = maps:without([<<"hashpath">>, <<"priv">>], normalize_keys(RawMsg2)),
    ?event(converge_internal, {set_called, {msg1, Msg1}, {msg2, Msg2}}, Opts),
    % Get the next key to set. 
    case keys(Msg2, internal_opts(Opts)) of
        [] -> Msg1;
        [Key|_] ->
            % Get the value to set. Use Converge by default, but fall back to
            % getting via `maps' if it is not found.
            Val =
                case get(Key, Msg2, internal_opts(Opts)) of
                    not_found -> maps:get(Key, Msg2);
                    Body -> Body
                end,
            ?event({got_val_to_set, {key, Key}, {val, Val}, {msg2, Msg2}}),
            % Next, set the key and recurse, removing the key from the Msg2.
            set(
                set(Msg1, Key, Val, internal_opts(Opts)),
                remove(Msg2, Key, internal_opts(Opts)),
                Opts
            )
    end.
set(Msg1, Key, Value, Opts) ->
    % For an individual key, we run deep_set with the key as the path.
    % This handles both the case that the key is a path as well as the case
    % that it is a single key.
    Path = hb_path:term_to_path_parts(Key),
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

%% @doc Call the device's `set' function.
device_set(Msg, Key, Value, Opts) ->
    Req =
        case Key of
            <<"path">> ->
                #{ <<"path">> => <<"set_path">>, <<"value">> => Value };
            _ ->
                #{ <<"path">> => <<"set">>, Key => Value }
        end,
	?event(
        converge_internal,
        {
            calling_device_set,
            {msg, Msg},
            {applying_set, Req}
        },
        Opts
    ),
	Res = hb_util:ok(
        resolve(
            Msg,
            Req,
            internal_opts(Opts)
        ),
        internal_opts(Opts)
    ),
	?event(
        converge_internal,
        {device_set_result, Res},
        Opts
    ),
	Res.

%% @doc Remove a key from a message, using its underlying device.
remove(Msg, Key) -> remove(Msg, Key, #{}).
remove(Msg, Key, Opts) ->
	hb_util:ok(
        resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => Key },
            internal_opts(Opts)
        ),
        Opts
    ).

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
			?event(converge_devices, {no_override_handler, {dev, Dev}, {key, Key}}),
			case {find_exported_function(Msg, Dev, Key, 3, Opts), Exported} of
				{{ok, Func}, true} ->
					% Case 3: The device has a function of the name `Key'.
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
    case dev_message:get(device, Msg) of
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
	case maps:get(normalize_key(Key), normalize_keys(Dev), not_found) of
		not_found -> not_found;
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
	try binary_to_existing_atom(normalize_key(Key), latin1) of
		KeyAtom -> find_exported_function(Msg, Mod, KeyAtom, Arity, Opts)
	catch _:_ -> not_found
	end;
find_exported_function(Msg, Mod, Key, Arity, Opts) ->
	case erlang:function_exported(Mod, Key, Arity) of
		true ->
			case is_exported(Msg, Mod, Key, Opts) of
				true -> {ok, fun Mod:Key/Arity};
				false -> not_found
			end;
		false ->
			find_exported_function(Msg, Mod, Key, Arity - 1, Opts)
	end.

%% @doc Check if a device is guarding a key via its `exports' list. Defaults to
%% true if the device does not specify an `exports' list. The `info' function is
%% always exported, if it exists. Elements of the `exludes' list are not
%% exported. Note that we check for info _twice_ -- once when the device is
%% given but the info result is not, and once when the info result is given.
%% The reason for this is that `info/3' calls other functions that may need to
%% check if a key is exported, so we must avoid infinite loops. We must, however,
%% also return a consistent result in the case that only the info result is
%% given, so we check for it in both cases.
is_exported(_Msg, _Dev, info, _Opts) -> true;
is_exported(Msg, Dev, Key, Opts) ->
	is_exported(info(Dev, Msg, Opts), Key).
is_exported(_, info) -> true;
is_exported(Info = #{ excludes := Excludes }, Key) ->
    case lists:member(normalize_key(Key), lists:map(fun normalize_key/1, Excludes)) of
        true -> false;
        false -> is_exported(maps:remove(excludes, Info), Key)
    end;
is_exported(#{ exports := Exports }, Key) ->
    lists:member(normalize_key(Key), lists:map(fun normalize_key/1, Exports));
is_exported(_Info, _Key) -> true.

%% @doc Convert a key to a binary in normalized form.
normalize_key(Key) -> normalize_key(Key, #{}).
normalize_key(Key, _Opts) when ?IS_ID(Key) -> Key;
normalize_key(Key, _Opts) when is_binary(Key) -> hb_util:to_lower(Key);
normalize_key(Key, _Opts) when is_atom(Key) -> atom_to_binary(Key);
normalize_key(Key, _Opts) when is_integer(Key) -> integer_to_binary(Key);
normalize_key(Key, _Opts) when is_list(Key) ->
    case hb_util:is_string_list(Key) of
        true -> normalize_key(list_to_binary(Key));
        false ->
            iolist_to_binary(
                lists:join(
                    <<"/">>,
                    lists:map(fun normalize_key/1, Key)
                )
            )
    end.

%% @doc Ensure that a message is processable by the Converge resolver: No lists.
normalize_keys(Msg1) when is_list(Msg1) ->
    normalize_keys(maps:from_list(
        lists:zip(
            lists:seq(1, length(Msg1)),
            Msg1
        )
    ));
normalize_keys(Map) when is_map(Map) ->
    maps:from_list(
        lists:map(
            fun({Key, Value}) when is_map(Value) ->
                {hb_converge:normalize_key(Key), Value};
            ({Key, Value}) ->
                {hb_converge:normalize_key(Key), Value}
            end,
            maps:to_list(Map)
        )
    );
normalize_keys(Other) -> Other.

%% @doc Load a device module from its name or a message ID.
%% Returns {ok, Executable} where Executable is the device module. On error,
%% a tuple of the form {error, Reason} is returned.
load_device(Map, _Opts) when is_map(Map) -> {ok, Map};
load_device(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
load_device(ID, Opts) when ?IS_ID(ID) ->
	case hb_opts:get(load_remote_devices) of
		true ->
			{ok, Msg} = hb_cache:read(maps:get(store, Opts), ID),
			Trusted =
				lists:any(
					fun(Signer) ->
						lists:member(Signer, hb_opts:get(trusted_device_signers))
					end,
					hb_util:ok(dev_message:attestors(Msg), Opts)
				),
			case Trusted of
				true ->
					RelBin = erlang:system_info(otp_release),
					case maps:get(<<"content-type">>, Msg, undefined) of
						<<"BEAM/", RelBin/bitstring>> ->
							ModNameBin = maps:get(<<"module-name">>, Msg, undefined),
							ModName = binary_to_atom(ModNameBin),
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
    NormKey =
        case is_atom(ID) of
            true -> ID;
            false -> normalize_key(ID)
        end,
    case maps:get(NormKey, hb_opts:get(preloaded_devices, #{}, Opts), unsupported) of
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

%% @doc The default device is the identity device, which simply returns the
%% value associated with any key as it exists in its Erlang map. It should also
%% implement the `set' key, which returns a `Message3' with the values changed
%% according to the `Message2' passed to it.
default_module() -> dev_message.

%% @doc The execution options that are used internally by the converge module
%% when calling itself.
internal_opts(Opts) ->
    maps:merge(Opts, #{
        topic => converge_internal,
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        spawn_worker => false
    }).
