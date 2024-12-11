-module(dev_wasm).
-export([init/3, execute/3, terminate/3]).
-export([checkpoint/3, checkpoint_uses/3]).
-include("include/hb.hrl").

%%% A device that executes a WASM image on messages using the Memory-64 preview
%%% standard. In the backend, this device uses `beamr`: An Erlang wrapper for
%%% WAMR, the WebAssembly Micro Runtime.

%% @doc Boot a WASM image on the image stated in the `Process/Image` field of
%% the message.
init(M1, _M2, Opts) ->
    ImageID = hb_converge:get(<<"Process/Image">>, M1, Opts),
    {ok, Image} =
        hb_cache:read_message(
            hb_opts:get(store, no_viable_store, Opts),
            ImageID
        ),
    % Start the WASM executor.
    {ok, Port, _ImportMap, _Exports} = hb_beamr:start(Image#tx.data),
    % Apply the checkpoint if it is in the initial state.
    case hb_converge:get(<<"WASM/State">>, M1, Opts) of
        undefined ->
            M1;
        Checkpoint ->
            ?event(wasm_checkpoint_found),
            ?event({wasm_deserializing, byte_size(Checkpoint)}),
            % Apply the checkpoint to the WASM state.
            hb_beamr:deserialize(Port, Checkpoint#tx.data),
            ?event(wasm_deserialized)
    end,
    % Set the WASM port, handler, and standard library invokation function.
    {ok,
        hb_converge:set(M1,
            #{
                <<"priv/WASM/Port">> => Port,
                <<"priv/WASM/Handler">> => <<"handle">>,
                <<"priv/WASM/Invoke-stdlib">> => fun invoke_stdlib/7
            }
        )
    }.

%% @doc Call the WASM executor with a message that has been prepared by a prior
%% pass.
execute(M1, _M2, Opts) ->
    case hb_converge:get(pass, M1, Opts) of
        1 ->
            % Extract the WASM port, func, params, and standard library invokation
            % from the message and apply them with the WASM executor.
            {ResType, Res, MsgAfterExecution} =
                hb_beamr:call(
                    M1,
                    hb_converge:get(<<"priv/WASM/Port">>, M1, Opts),
                    hb_converge:get(<<"WASM/Handler">>, M1, Opts),
                    hb_converge:get(<<"WASM/Params">>, M1, Opts),
                    hb_converge:get(<<"priv/WASM/Invoke-stdlib">>, M1, Opts),
                    Opts
                ),
            {ok,
                hb_converge:set(MsgAfterExecution,
                    #{
                        <<"Results/WASM/Type">> => ResType,
                        <<"Results/WASM/Value">> => Res
                    }
                )
            };
        _ -> {ok, M1}
    end.

%% @doc Serialize the WASM state to a message.
checkpoint(M1, _M2, Opts) ->
    Port = hb_converge:get(<<"priv/WASM/Port">>, M1, Opts),
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
    Port = hb_converge:get(<<"priv/WASM/Port">>, M1, Opts),
    hb_beamr:stop(Port),
    {ok, hb_converge:set(M1,
        #{
            <<"priv/WASM/Port">> => undefined
        }
    )}.

%% @doc Handle standard library calls by looking up the function in the
%% message and calling it.
invoke_stdlib(M1, Port, ModName, FuncName, Args, Sig, Opts) ->
    Library =
        hb_converge:get(
            <<"priv/WASM/stdlib/", ModName/bitstring, "/", FuncName/bitstring>>,
            M1,
            hb_converge:get(<<"priv/WASM/Opts">>, M1, #{ hashpath => ignore })
        ),
    case maps:get({ModName, FuncName}, Library, undefined) of
        undefined ->
            lib(M1, Port, Args, ModName, FuncName, Sig, Opts);
        Func ->
            {arity, Arity} = erlang:fun_info(Func, arity),
            ApplicationTerms =
                lists:sublist(
                    [S, Port, Args, ModName, FuncName, Sig, Opts],
                    Arity
                ),
            erlang:apply(Func, ApplicationTerms)
    end.

lib(M1, _Port, Args, Module, Func, Signature, _Opts) ->
    ?event({unimplemented_dev_wasm_call, Module, Func, Args, Signature}),
    {M1, [0]}.