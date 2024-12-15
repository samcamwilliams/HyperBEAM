-module(dev_stack).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").

%%% @doc A device that contains a stack of other devices, which it runs
%%% upon input messages in the order of their keys. A stack maintains and passes
%%% forward a state (expressed as a message) as it progresses through devices,
%%% in a similar manner to a `fold' operation.
%%%
%%% For example, a stack of devices as follows:
%%% ```
%%% Device -> Stack
%%% Device-Stack/1/Name -> Add-One-Device
%%% Device-Stack/2/Name -> Add-Two-Device'''
%%% 
%%% When called with the message:
%%% ```
%%% #{ Path = "FuncName", binary => <<"0">> }'''
%%% 
%%% Will produce the output:
%%%  ```
%%% #{ Path = "FuncName", binary => <<"3">> }
%%% {ok, #{ bin => <<"3">> }}'''
%%%
%%% The key that is called upon the device stack is the same key that is used
%%% upon the devices that are contained within it. For example, in the above
%%% scenario we resolve FuncName on the stack, leading FuncName to be called on
%%% Add-One-Device and Add-Two-Device.
%%%
%%% A device stack responds to special tags upon responses as follows:
%%%
%%% `skip': Skips the rest of the device stack for the current pass.
%%% 
%%% `pass': Causes the stack to increment its pass number and re-execute
%%%  the stack from the first device, maintaining the state accumulated so
%%%  far.
%%%
%%% In all cases, the device stack will return the accumulated state to the
%%% caller as the result of the call to the stack.
%%%
%%% The dev_stack adds additional metadata to the message in order to track
%%% the state of its execution as it progresses through devices. These keys
%%% are as follows:
%%%
%%% `slot': The number of times the stack has been executed upon different
%%% messages.
%%% 
%%% `pass': The number of times the stack has reset and re-executed from the
%%% first device for the current message.
%%%
%%% All counters used by the stack are initialized to 1.
%%%
%%% Additionally, as implemented in HyperBEAM, the device stack will honor a
%%% number of options that are passed to it as keys in the message. Each of
%%% these options is also passed through to the devices contained within the
%%% stack during execution. These options include:
%%%
%%% Error-Strategy: Determines how the stack handles errors from devices.
%%% See `maybe_error/5' for more information.
%%% 
%%% Allow-Multipass: Determines whether the stack is allowed to automatically
%%% re-execute from the first device when the `pass' tag is returned. See
%%% `maybe_pass/3' for more information.
%%%
%%% Under-the-hood, dev_stack uses a `default' handler to resolve all calls to
%%% devices, aside `set/2' which it calls itself to mutate the message's `device'
%%% key in order to change which device is currently being executed. This method
%%% allows dev_stack to ensure that the message's HashPath is always correct,
%%% even as it delegates calls to other devices. An example flow for a `dev_stack'
%%% execution is as follows:
%%%```
%%% 	/Msg1/AlicesExcitingKey ->
%%% 		dev_stack:execute ->
%%% 			/Msg1/Set?device=/Device-Stack/1 ->
%%% 			/Msg2/AlicesExcitingKey ->
%%% 			/Msg3/Set?device=/Device-Stack/2 ->
%%% 			/Msg4/AlicesExcitingKey
%%% 			... ->
%%% 			/MsgN/Set?device=[This-Device] ->
%%% 		returns {ok, /MsgN+1} ->
%%% 	/MsgN+1'''
%%%
%%% In this example, the `device' key is mutated a number of times, but the
%%% resulting HashPath remains correct and verifiable.

-include("include/hb.hrl").

info(_) ->
    #{
		handler => fun router/4
	}.

%% @doc The device stack key router. Sends the request to `resolve_stack',
%% except for `set/2' which is handled by the default implementation in
%% `dev_message'.
router(keys, Message1, Message2, _Opts) ->
	?event({keys_called, {msg1, Message1}, {msg2, Message2}}),
	dev_message:keys(Message1);
router(set, Message1, Message2, Opts) ->
	?event({set_called, {msg1, Message1}, {msg2, Message2}}),
	dev_message:set(Message1, Message2, Opts);
router(transform, Message1, _Message2, Opts) ->
	transformer_message(Message1, Opts);
router(Key, Message1, Message2, Opts) ->
	?event({router_called, {key, Key}, {msg1, Message1}, {msg2, Message2}}),
	{ok, InitDevMsg} = dev_message:get(<<"Device">>, Message1, Opts),
	?event({got_device_key, InitDevMsg}),
	case resolve_stack(Message1, Key, Message2, Opts) of
		{ok, Result} when is_map(Result) ->
			?event({router_result, ok, Result}),
			dev_message:set(Result, #{<<"Device">> => InitDevMsg}, Opts);
		Else ->
			?event({router_result, unexpected, Else}),
			Else
	end.


%% @doc Return a message which, when given a key, will transform the message
%% such that the device named `Key' from the `Device-Stack' key in the message
%% takes the place of the original `Device' key. This allows users to call
%% a single device from the stack:
%%
%% 	/Msg1/AlicesExcitingStack/Transform/DeviceName/keyInDevice ->
%% 		keyInDevice executed on DeviceName against Msg1.
transformer_message(Msg1, Opts) ->
	?event({creating_transformer, {for, Msg1}}),
	{ok, 
		Msg1#{
			device => #{
				info =>
					fun() ->
						#{
							handler =>
								fun(Key, MsgX1) ->
									transform(MsgX1, Key, Opts)
								end
						}
					end,
				type => <<"Stack-Transformer">>
			}
		}
	}.

%% @doc Return Message1, transformed such that the device named `Key' from the
%% `Device-Stack' key in the message takes the place of the original `Device'
%% key. This transformation allows dev_stack to correctly track the HashPath
%% of the message as it delegates execution to devices contained within it.
transform(Msg1, Key, Opts) ->
	% Get the device stack message from Msg1.
	case dev_message:get(<<"Device-Stack">>, Msg1, Opts) of
		{ok, StackMsg} ->
			% Find the requested key in the device stack.
			case hb_converge:resolve(StackMsg, #{ path => Key }, Opts) of
				{ok, DevMsg} ->
					% Set the:
					% - Device key to the device we found.
					% - `/Device-Stack/Previous' key to the device we are
					%   replacing.
					?event({activating_device, DevMsg}),
					dev_message:set(
						Msg1,
						#{
							<<"Device">> => DevMsg,
							<<"Device-Stack">> =>
								StackMsg#{ <<"Previous">> =>
									hb_util:ok(
										dev_message:get(
											<<"Device">>,
											Msg1,
											Opts
										)
									)
								}
						},
						Opts
					);
				_ ->
					?event({no_device_key, Key}),
					not_found
			end;
		_ -> throw({error, no_valid_device_stack})
	end.

%% @doc The main device stack execution engine. See the moduledoc for more
%% information.
resolve_stack(Message1, Key, Message2, Opts) ->
    resolve_stack(Message1, Key, Message2, 1, Opts).
resolve_stack(Message1, Key, Message2, DevNum, Opts) ->
	?event(
		{stack_transform,
			DevNum,
			{key, Key},
			{message1, Message1},
			{message2, Message2}
		}
	),
	case transform(Message1, integer_to_binary(DevNum), Opts) of
		{ok, Message3} ->
			?event({stack_execute, DevNum, Message3}),
			case hb_converge:resolve(Message3, Message2, Opts) of
				{ok, Message4} when is_map(Message4) ->
					?event({result, ok, DevNum, Message4}),
					resolve_stack(Message4, Key, Message2, DevNum + 1, Opts);
				{skip, Message4} when is_map(Message4) ->
					?event({result, skip, DevNum, Message4}),
					{ok, Message4};
				{pass, Message4} when is_map(Message4) ->
					case hb_converge:resolve(Message4, pass, Opts) of
						{ok, <<"Allow">>} ->
							?event({result, pass, allowed, DevNum, Message4}),
							?no_prod("Must update the pass number."),
							resolve_stack(
								dev_message:set(Message4, #{ pass =>
									1 }, Opts),
								Key,
								Message2,
								1,
								Opts
							);
						_ ->
							?event(
								{result,
									pass,
									not_allowed,
									DevNum,
									Message4
								}
							),
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
					?event({result, error, DevNum, Info}),
					maybe_error(
						Message1,
						Key,
						Message2,
						DevNum + 1,
						Opts,
						Info
					);
				Unexpected ->
					?event({result, unexpected, DevNum, Unexpected}),
					maybe_error(
						Message1,
						Key,
						Message2,
						DevNum + 1,
						Opts,
						{unexpected_result, Unexpected}
					)
			end;
		not_found ->
			?event({execution_complete, DevNum, Message1}),
			{ok, Message1}
	end.

maybe_error(Message1, Key, Message2, DevNum, Info, Opts) ->
    case hb_converge:to_key(hb_converge:get(<<"Error-Strategy">>, Message1, Opts)) of
        stop ->
			{error, {stack_call_failed, Message1, Key, Message2, DevNum, Info}};
        throw ->
			throw({error_running_dev, Message1, Key, Message2, DevNum, Info});
        continue ->
			?event({continue_stack_execution_after_error, Message1, Key, Info}),
            resolve_stack(
                hb_converge:set(Message1,
                    [
                        <<"Errors">>,
                        hb_converge:get(id, Message1, Opts),
                        hb_converge:get(pass, Message1, Opts),
                        DevNum,
                        hb_util:debug_fmt(Info)
                    ],
                    Opts
                ),
                Key,
                Message2,
                DevNum + 1,
                Opts
            );
        ignore ->
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

generate_append_device(Separator) ->
	generate_append_device(Separator, ok).
generate_append_device(Separator, Status) ->
	#{
		append =>
			fun(M1 = #{ result := Existing }, #{ bin := New }) ->
				?event({appending, {existing, Existing}, {new, New}}),
				{Status, M1#{ result =>
					<< Existing/binary, Separator/binary, New/binary>>
				}}
			end
	}.

%% @doc Test that the transform function can be called correctly internally
%% by other functions in the module.
transform_internal_call_device_test() ->
	AppendDev = generate_append_device(<<"_">>),
	Msg1 =
		#{
			device => <<"Stack/1.0">>,
			<<"Device-Stack">> =>
				#{
					<<"1">> => AppendDev,
					<<"2">> => <<"Message/1.0">>
				}
		},
	?assertMatch(
		<<"Message/1.0">>,
		hb_converge:get(
			<<"Device">>,
			element(2, transform(Msg1, <<"2">>, #{}))
		)
	).

%% @doc Ensure we can generate a transformer message that can be called to
%% return a version of msg1 with only that device attached.
transform_external_call_device_test() ->
	Msg1 = #{
		device => <<"Stack/1.0">>,
		<<"Device-Stack">> =>
			#{
				<<"Make-Cool">> =>
					#{
						info =>
							fun() ->
								#{
									handler =>
										fun(keys, MsgX1) ->
											{ok, maps:keys(MsgX1)};
										(Key, MsgX1) ->
											{ok, Value} =
												dev_message:get(Key, MsgX1),
											dev_message:set(
												MsgX1,
												#{ Key =>
													<< Value/binary, "-Cool">>
												},
												#{}
											)
										end
								}
							end,
						suffix => <<"-Cool">>
					}
			},
		value => <<"Super">>
	},
	?assertMatch(
		{ok, #{ value := <<"Super-Cool">> }},
		hb_converge:resolve(Msg1, #{
			path => <<"/Transform/Make-Cool/Value">>
		})
	).

example_device_for_stack_test() ->
	% Test the example device that we use for later stack tests, such that
	% we know that an error later is actually from the stack, and not from
	% the example device.
	?assertMatch(
		{ok, #{ result := <<"1_2">> }},
		hb_converge:resolve(
			#{ device => generate_append_device(<<"_">>), result => <<"1">> },
			#{ path => append, bin => <<"2">> },
			#{}
		)
	).

simple_stack_execute_test() ->
	Msg = #{
		device => <<"Stack/1.0">>,
		<<"Device-Stack">> =>
			#{
				<<"1">> => generate_append_device(<<"!D1!">>),
				<<"2">> => generate_append_device(<<"_D2_">>)
			},
		result => <<"INIT">>
	},
	?event({stack_executing, test, {explicit, Msg}}),
	?assertMatch(
		{ok, #{ result := <<"INIT!D1!2_D2_2">> }},
		hb_converge:resolve(Msg, #{ path => append, bin => <<"2">> })
	).

many_devices_test() ->
	Msg = #{
		device => <<"Stack/1.0">>,
		<<"Device-Stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>),
				<<"2">> => generate_append_device(<<"+D2">>),
				<<"3">> => generate_append_device(<<"+D3">>),
				<<"4">> => generate_append_device(<<"+D4">>),
				<<"5">> => generate_append_device(<<"+D5">>),
				<<"6">> => generate_append_device(<<"+D6">>),
				<<"7">> => generate_append_device(<<"+D7">>),
				<<"8">> => generate_append_device(<<"+D8">>)
			},
		result => <<"INIT">>
	},
	?assertMatch(
		{ok,
			#{
				result :=
					<<"INIT+D12+D22+D32+D42+D52+D62+D72+D82">>
			}
		},
		hb_converge:resolve(Msg, #{ path => append, bin => <<"2">> })
	).

reinvocation_test() ->
	Msg = #{
		device => <<"Stack/1.0">>,
		<<"Device-Stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>),
				<<"2">> => generate_append_device(<<"+D2">>)
			},
		result => <<"INIT">>
	},
	Res1 = hb_converge:resolve(Msg, #{ path => append, bin => <<"2">> }),
	?assertMatch(
		{ok, #{ result := <<"INIT+D12+D22">> }},
		Res1
	),
	{ok, Msg2} = Res1,
	Res2 = hb_converge:resolve(Msg2, #{ path => append, bin => <<"3">> }),
	?assertMatch(
		{ok, #{ result := <<"INIT+D12+D22+D13+D23">> }},
		Res2
	).

skip_test() ->
	Msg1 = #{
		device => <<"Stack/1.0">>,
		<<"Device-Stack">> =>
			#{
				<<"1">> => generate_append_device(<<"+D1">>, skip),
				<<"2">> => generate_append_device(<<"+D2">>)
			},
		result => <<"INIT">>
	},
	?assertMatch(
		{ok, #{ result := <<"INIT+D12">> }},
		hb_converge:resolve(
			Msg1,
			#{ path => append, bin => <<"2">> }
		)
	).