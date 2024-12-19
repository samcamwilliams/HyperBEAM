%%% @doc A device that executes a WASM image on messages using the Memory-64 
%%% preview standard. In the backend, this device uses `beamr`: An Erlang wrapper 
%%% for WAMR, the WebAssembly Micro Runtime.
%%% 
%%% The device has the following requirements and interface:
%%% ```
%%%     M1/Init ->
%%%         Assumes:
%%%             M1/Process
%%%             M1/Process/Image
%%%         Generates:
%%%             /priv/WASM/Port
%%%             /priv/WASM/Handler
%%%             /priv/WASM/Invoke-stdlib
%%%         Side-effects:
%%%             Creates a WASM executor loaded in memory of the HyperBEAM node.
%%% 
%%%     M1/Computed ->
%%%         Assumes:
%%%             M1/priv/WASM/Port
%%%             M1/Process
%%%             M2/Message
%%%         Generates:
%%%             /Results/WASM/Type
%%%             /Results/WASM/Body
%%%         Side-effects:
%%%             Calls the WASM executor with the message and process.'''
-module(dev_wasm).
-export([init/3, computed/3, terminate/3]).
-export([checkpoint/3, checkpoint_uses/3]).
-export([init_wasm_state/1]).
-include("include/hb.hrl").

%% @doc Boot a WASM image on the image stated in the `Process/Image' field of
%% the message.
init(M1, _M2, Opts) ->
    ImageID = hb_converge:get(<<"Process/WASM-Image">>, M1, Opts),
    {ok, Image} =
        hb_cache:read(
            hb_opts:get(store, no_viable_store, Opts),
            ImageID
        ),
    % Start the WASM executor.
    {ok, Port, _ImportMap, _Exports} = hb_beamr:start(Image#tx.data),
    % Apply the checkpoint if it is in the initial state.
    case hb_converge:get(<<"WASM/State">>, M1, Opts) of
        not_found ->
            M1;
        Checkpoint ->
            ?event(wasm_checkpoint_found),
            ?event({wasm_deserializing, byte_size(Checkpoint)}),
            % Apply the checkpoint to the WASM state.
            hb_beamr:deserialize(Port, Checkpoint#tx.data),
            ?event(wasm_deserialized)
    end,
    init_wasm_state(M1, Port, Opts).

init_wasm_state(Port) ->
    init_wasm_state(#{}, Port, #{}).
init_wasm_state(M1, Port, Opts) ->
    % Set the WASM port, handler, and standard library invokation function.
    {ok,
        hb_private:set(M1,
            #{
                <<"WASM/Port">> => Port,
                <<"WASM/Handler">> => <<"handle">>,
                <<"WASM/Invoke-stdlib">> => fun invoke_stdlib/3
            },
            Opts
        )
    }.

%% @doc Call the WASM executor with a message that has been prepared by a prior
%% pass.
computed(M1, _M2, Opts) ->
    case hb_converge:get(pass, M1, Opts) of
        1 ->
            % Extract the WASM port, func, params, and standard library
            % invokation from the message and apply them with the WASM executor.
            {ResType, Res, MsgAfterExecution} =
                hb_beamr:call(
                    M1,
                    hb_private:get(<<"WASM/Port">>, M1, Opts),
                    hb_converge:get(<<"WASM/Handler">>, M1, Opts),
                    hb_converge:get(<<"WASM/Params">>, M1, Opts),
                    hb_private:get(<<"WASM/Invoke-stdlib">>, M1, Opts),
                    Opts
                ),
            {ok,
                hb_converge:set(MsgAfterExecution,
                    #{
                        <<"Results/WASM/Type">> => ResType,
                        <<"Results/WASM/Body">> => Res
                    }
                )
            };
        _ -> {ok, M1}
    end.

%% @doc Serialize the WASM state to a message.
checkpoint(M1, _M2, Opts) ->
    Port = hb_private:get(<<"priv/WASM/Port">>, M1, Opts),
    SaveKeys = hb_converge:get(<<"Checkpoint-Keys">>, M1, Opts),
    {ok, Serialized} = hb_beamr:serialize(Port),
    {ok, hb_converge:set(M1,
        #{
            <<"WASM/State">> => Serialized,
            <<"Checkpoint-Keys">> => [ <<"WASM/State">> | SaveKeys ]
        }
    )}.

%% @doc Add the necessary state for WASM to resume to the list of keys.
checkpoint_uses(M1, _M2, Opts) ->
    SaveKeys = hb_converge:get(<<"Checkpoint-Keys">>, M1, Opts),
    {ok, hb_converge:set(M1,
        #{
            <<"Checkpoint-Keys">> => [ <<"WASM/State">> | SaveKeys ]
        }
    )}.

%% @doc Tear down the WASM executor.
terminate(M1, _M2, Opts) ->
    ?event(terminate_called_on_dev_wasm),
    Port = hb_private:get(<<"priv/WASM/Port">>, M1, Opts),
    hb_beamr:stop(Port),
    {ok, hb_private:set(M1,
        #{
            <<"WASM/Port">> => undefined
        },
        Opts
    )}.

%% @doc Handle standard library calls by looking up the function in the
%% message and calling it. Calls the stub function if the function is not
%% found in the message.
invoke_stdlib(M1, M2, Opts) ->
    Port = hb_converge:get(port, M2, Opts#{ hashpath => ignore }),
    ModName = hb_converge:get(module, M2, Opts#{ hashpath => ignore }),
    FuncName = hb_converge:get(func, M2, Opts#{ hashpath => ignore }),
    Args = hb_converge:get(args, M2, Opts#{ hashpath => ignore }),
    Sig = hb_converge:get(signature, M2, Opts#{ hashpath => ignore }),
    MaybeFunc =
        hb_private:get(
            <<"priv/WASM/stdlib/", ModName/bitstring, "/", FuncName/bitstring>>,
            M1,
            Opts
        ),
    case MaybeFunc of
        not_found ->
            lib(M1, Port, Args, ModName, FuncName, Sig, Opts);
        Func ->
            {arity, Arity} = erlang:fun_info(Func, arity),
            ApplicationTerms =
                lists:sublist(
                    [M1, Port, Args, ModName, FuncName, Sig, Opts],
                    Arity
                ),
            erlang:apply(Func, ApplicationTerms)
    end.

%% @doc Log the call to the standard library as an event, and write the
%% call details into the message.
lib(M1, _Port, Args, Module, Func, Signature, Opts) ->
    ?event({unimplemented_dev_wasm_call, Module, Func, Args, Signature}),
    M3 = hb_converge:set(
        M1,
        #{<<"Results/WASM/Unimplemented-Calls">> =>
            [
                #{
                    <<"Module">> => Module,
                    <<"Func">> => Func,
                    <<"Args">> => Args,
                    <<"Signature">> => Signature
                }
            | hb_converge:get(<<"Results/WASM/Unimplemented-Calls">>, M1, Opts)
            ]
        }
    ),
    {M3, [0]}.
