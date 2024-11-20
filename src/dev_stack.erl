-module(dev_stack).
-export([info/0, from_process/1, create/1, create/2, create/3]).
-export([boot/2, execute/2, execute/3, call/4]).
-include_lib("eunit/include/eunit.hrl").

%%% @moduledoc A device that contains a stack of other devices, which it runs
%%% upon input messages in the order of their keys. A stack maintains and passes
%%% forward a state (expressed as a message) as it progresses through devices,
%%% in a similar manner to a `fold` operation.
%%% 
%%% For example, a stack of devices as follows:
%%%
%%%  	Device -> Stack
%%%  	Device-Stack/1/Name -> Add-One-Device
%%%		Device-Stack/2/Name -> Add-Two-Device
%%% 
%%% When called with the message:
%%% 
%%% 	#{ Path = "FuncName", binary => <<"0">> }
%%% 
%%% Will produce the output:
%%% 
%%% 	#{ Path = "FuncName", binary => <<"3">> }
%%% 	{ok, #{ bin => <<"3">> }}
%%% 
%%% The key that is called upon the device stack is the same key that is used
%%% upon the devices that are contained within it. For example, in the above
%%% scenario we resolve FuncName on the stack, leading FuncName to be called on
%%% Add-One-Device and Add-Two-Device.
%%% 
%%% A device stack responds to special tags upon responses as follows:
%%% 
%%% 	`skip`: Skips the rest of the device stack for the current pass.
%%% 	`pass`: Causes the stack to increment its pass number and re-execute
%%% 	the stack from the first device, maintaining the state accumulated so
%%% 	far.
%%% 
%%% In all cases, the device stack will return the accumulated state to the
%%% caller as the result of the call to the stack.
%%% 
%%% The dev_stack adds additional metadata to the message in order to track
%%% the state of its execution as it progresses through devices. These keys
%%% are as follows:
%%% 
%%% 	`slot`: The number of times the stack has been executed upon different
%%% 	messages.
%%% 	`pass`: The number of times the stack has reset and re-executed from the
%%% 	first device for the current message.
%%% 
%%% All counters used by the stack are initialized to 1.
%%% 
%%% Additionally, as implemented in HyperBEAM, the device stack will honor a
%%% number of options that are passed to it as environment variables (as its
%%% third `Opts` argument). Each of these options is also passed through to the
%%% devices contained within the stack during execution. These options include:
%%% 
%%% 	Error-Strategy: Determines how the stack handles errors from devices.
%%% 	See `maybe_error/5` for more information.
%%% 	Allow-Multipass: Determines whether the stack is allowed to automatically
%%% 	re-execute from the first device when the `pass` tag is returned. See
%%% 	`maybe_pass/3` for more information.

-include("include/hb.hrl").
-hb_debug(print).

info() ->
    #{
        handler => fun resolve/4
    }.

%% @doc Ensures that all devices in the stack have their executable versions
%% loaded and ready to run, then return a list of the executables, tagged with
%% their device number. Device stacks are cached in the message so that this
%% function only needs to resolve the stack once unless it changes.
executable_devices(Message1, Opts) ->
	{ok, StackMsg} = hb_pam:resolve(Message1, <<"Device-Stack">>, Opts),
	case maps:get(priv_stack_cache, hb_pam:private(StackMsg), #{}) of
		StackMsg -> StackMsg;
		_Else -> stack_to_executable_devices(StackMsg, Opts)
	end.

%% @doc Underlying resolution mechanism for converting a stack message to a
%% list of executable devices.
stack_to_executable_devices(StackMsg, Opts) ->
	lists:map(
		fun(DevNum, DevMsg) ->
			{ok, DevName} = hb_pam:resolve(DevMsg, <<"Name">>, Opts),
			case hb_pam:device_id_to_executable(DevName) of
				{ok, Executable} -> 
					hb_pam:resolve(
						set,
						DevMsg,
						#{ priv_executable => Executable }
					);
				Else ->
					throw(
						{
							could_not_load_device,
							DevNum,
							DevName,
							{unexpected_result, Else}
						}
					)
			end
		end,
		hb_util:message_to_numbered_list(StackMsg)
	).

%% @doc The main device stack execution engine.
%% Call the execute function on each device in the stack, then call the
%% finalize function on the resulting state.
resolve(FuncName, Message1, Message2, Opts) ->
	
	ExecutableDevices = executable_devices(StackMsg),
	{ok, }
		do_call(
			ExecutableDevices,
			Message1#{
				results => #{},
				errors => [],
				pass => 1
			},
			FuncName,
			Message2,
			Opts
		).

do_call([], S, _FuncName, _Opts) ->
    {ok, S};
do_call(AllDevs = [Dev = {_N, DevExec}|Devs], FuncName, Message2, Opts) ->
    ?event(
		{
			DevExec,
			FuncName,
			{slot, maps:get(slot, S, "[no_slot]")},
			{pass, maps:get(pass, S, "[no_pass_num]")}
		}
	),
    case hb_pam:resolve(DevExec, FuncName, ArgPrefix ++ [S, DevS, Params], Opts) of
        no_match ->
            do_call(Devs, S, FuncName, Opts);
        {skip, NewS} when is_map(NewS) ->
            {ok, NewS};
        {skip, NewS, NewPrivS} when is_map(NewS) ->
            {ok, update(NewS, Dev, NewPrivS)};
        {ok, NewS} when is_map(NewS) ->
            do_call(Devs, NewS, FuncName, Opts);
        {ok, NewS, NewPrivS} when is_map(NewS) ->
            do_call(Devs, update(NewS, Dev, NewPrivS), FuncName, Opts);
        {ok, NewS} when is_record(NewS, tx) ->
            do_call(Devs, S#{ results => NewS }, FuncName, Opts);
        {ok, NewS, NewPrivS} when is_record(NewS, tx) ->
            do_call(Devs, update(S#{ results => NewS }, Dev, NewPrivS), FuncName, Opts);
        {pass, NewS} when is_map(NewS) ->
            maybe_pass(NewS, FuncName, Opts);
        {pass, NewS, NewPrivS} when is_map(NewS) ->
            maybe_pass(update(NewS, Dev, NewPrivS), FuncName, Opts);
        {error, Info} ->
            maybe_error(AllDevs, S, FuncName, Opts, Info);
        Unexpected ->
            maybe_error(AllDevs, S, FuncName, Opts, {unexpected_result, Unexpected})
    end.

maybe_error([{N, DevExec, _DevS, _Params}|Devs], S = #{ errors := Errs }, FuncName, Opts, Info) ->
    case maps:get(error_strategy, Opts, stop) of
        stop -> {error, N, DevExec, Info};
        throw -> throw({error_running_dev, N, DevExec, Info});
        continue ->
            do_call(
                Devs,
                S#{ errors := Errs ++ [{N, DevExec, Info}]},
                FuncName,
                Opts
            );
        ignore -> do_call(Devs, S, FuncName, Opts)
    end.

maybe_pass(NewS = #{ pass := Pass }, FuncName, Opts) ->
    case maps:get(pass, Opts, allowed) of
        disallowed ->
            % If we cannot handle repassing automatically, return the rest of
            % the device stack to the caller, as well as the new state.
            {pass, NewS};
        allowed ->
            #{ devices := NewDevs } = NewS,
            do_call(NewDevs, NewS#{ pass => Pass + 1 }, FuncName, Opts)
    end.

%%% Tests

generate_append_device(Str) ->
	#{
		test =>
			fun(M = #{ bin := Bin }) ->
				{ok, << Bin/binary, Str/bitstring >>}
			end
	}.

simple_stack_execute_test() ->
	Msg = #{
		<<"Device">> => ?MODULE,
		<<"Device-Stack">> =>
			#{
				<<"1">> => generate_append_device("1"),
				<<"2">> => generate_append_device("2")
			}
	},
	?assertEqual(
		{ok, #{ bin => <<"12">> }},
		hb_pam:resolve(Msg, test)
	).