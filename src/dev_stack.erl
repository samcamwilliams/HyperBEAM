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
%%% number of options that are passed to it as keys in the message. Each of
%%% these options is also passed through to the devices contained within the
%%% stack during execution. These options include:
%%% 
%%% 	Error-Strategy: Determines how the stack handles errors from devices.
%%% 	See `maybe_error/5` for more information.
%%% 	Allow-Multipass: Determines whether the stack is allowed to automatically
%%% 	re-execute from the first device when the `pass` tag is returned. See
%%% 	`maybe_pass/3` for more information.
%%% 
%%% Under-the-hood, dev_stack uses a `default` handler to resolve all calls to
%%% devices, aside `set/2` which it calls itself to mutate the message's `device`
%%% key in order to change which device is currently being executed. This method
%%% allows dev_stack to ensure that the message's HashPath is always correct,
%%% even as it delegates calls to other devices. An example flow for a `dev_stack`
%%% execution is as follows:
%%% 
%%% 	/Msg1/AlicesExcitingKey ->
%%% 		dev_stack:execute ->
%%% 			/Msg1/Set?device=/Device-Stack/1 ->
%%% 			/Msg2/AlicesExcitingKey ->
%%% 			/Msg3/Set?device=/Device-Stack/2 ->
%%% 			/Msg4/AlicesExcitingKey
%%% 			... ->
%%% 			/MsgN/Set?device=[This-Device] ->
%%% 		returns {ok, /MsgN+1} ->
%%% 	/MsgN+1
%%%
%%% In this example, the `device` key is mutated a number of times, but the
%%% resulting HashPath remains correct and verifiable.

-include("include/hb.hrl").
-hb_debug(print).

info() ->
    #{ handler => fun router/4 }.

%% @doc The device stack key router. Sends the request to `resolve_stack`,
%% except for `set/2` which is handled by the default implementation in
%% `dev_message`. Note that the argument order here for execution deviates
%% from the normal `Message1, Key, Message2` order used by `hb_pam:resolve/4`.
%% This is due to the key variable being prepended by `hb_pam` when the router
%% is called. Elsewhere in this module, the normal `Message1, Key, Message2`
%% order is used.
router(set, Message1, Message2, Opts) ->
	dev_message:set(Message1, Message2, Opts);
router(Key, Message1, Message2, Opts) ->
	{ok, InitDevMsg} = hb_pam:resolve(Message1, <<"Device">>, Opts),
	case resolve_stack(Key, Message1, Message2, Opts) of
		{ok, Result} when is_map(Result) ->
			{ok, hb_pam:set(Result, <<"Device">>, InitDevMsg, Opts)};
		Else -> Else
	end.

%% @doc Return Message1, transformed such that the device named `Key` from the
%% `Device-Stack` key in the message takes the place of the original `Device`
%% key. This transformation allows dev_stack to correctly track the HashPath
%% of the message as it delegates execution to devices contained within it.
transform_device(Message1, Key, Opts) ->
	case hb_pam:resolve(Message1, [<<"Device-Stack">>, Key], Opts) of
		{ok, DevMsg} ->
			{ok,
				hb_pam:set(
					Message1,
					#{ <<"Device">> => DevMsg },
					Opts
				)
			};
		_ -> not_found
	end.

%% @doc The main device stack execution engine. See the `moduledoc` for more
%% information.
resolve_stack(Message1, Key, Message2, Opts) ->
	resolve_stack(Message1, Key, Message2, 1, Opts).
resolve_stack(Message1, Key, Message2, DevNum, Opts) ->
	case transform_device(Message1, integer_to_binary(DevNum), Opts) of
		{ok, Message3} ->
			?event({stack_executing_device, DevNum, Message3}),
			case hb_pam:resolve(Message3, Key, Message2, Opts) of
				{ok, Message4} when is_map(Message4) ->
					resolve_stack(Message4, Key, Message2, DevNum + 1, Opts);
				{skip, Message4} when is_map(Message4) ->
					{ok, Message4};
				{pass, Message4} when is_map(Message4) ->
					case hb_pam:resolve(Message4, pass, Opts) of
						{ok, <<"Allow">>} ->
							?event({stack_repassing, DevNum, Message4}),
							resolve_stack(
								hb_pam:set(Message4, pass, 1, Opts),
								Key,
								Message2,
								1,
								Opts
							);
						_ ->
							maybe_error(
								Message1,
								Key,
								Message2,
								DevNum + 1,
								Opts,
								{pass_not_allowed, Message4}
							)
					end;
				{error, Info} ->
					maybe_error(Message1, Key, Message2, DevNum + 1, Opts, Info);
				Unexpected ->
					maybe_error(
						Message1,
						Key,
						Message2,
						DevNum + 1,
						Opts,
						{unexpected_pam_result, Unexpected}
					)
			end;
		not_found ->
			?event({stack_execution_finished, DevNum, Message1}),
			{ok, Message1}
	end.

maybe_error(Message1, Key, Message2, DevNum, Info, Opts) ->
    case hb_pam:get(Message1, <<"Error-Strategy">>, Opts) of
        <<"Stop">> ->
			{error, {stack_call_failed, Message1, Key, Message2, DevNum, Info}};
        <<"Throw">> ->
			throw({error_running_dev, Message1, Key, Message2, DevNum, Info});
        <<"Continue">> ->
			?event({continue_stack_execution_after_error, Message1, Key, Info}),
            resolve_stack(
                hb_pam:set(Message1,
					[
						<<"Errors">>,
						hb_pam:get(Message1, id, Opts),
						hb_pam:get(Message1, <<"Pass">>, Opts),
						DevNum,
						hb:debug_fmt(Info)
					],
					Opts
				),
                Key,
				Message2,
                DevNum + 1,
                Opts
            );
        <<"Ignore">> ->
			?event({ignoring_stack_error, Message1, Key, Info}),
            resolve_stack(
                Message1,
                Key,
                Message2,
                DevNum + 1,
                Opts
            )
    end.

%%% Tests

generate_append_device(Str) ->
	#{
		test =>
			fun(#{ bin := Bin }) ->
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