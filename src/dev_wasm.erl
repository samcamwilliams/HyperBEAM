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
%%%             /priv/WASM/Import-Resolver
%%%         Side-effects:
%%%             Creates a WASM executor loaded in memory of the HyperBEAM node.
%%% 
%%%     M1/Computed ->
%%%         Assumes:
%%%             M1/priv/WASM/Port
%%%             M1/priv/WASM/Import-Resolver
%%%             M1/Process
%%%             M2/Message
%%%         Generates:
%%%             /Results/WASM/Type
%%%             /Results/WASM/Body
%%%         Side-effects:
%%%             Calls the WASM executor with the message and process.'''
-module(dev_wasm).
-export([init/3, compute/3, import/3, terminate/3]).
-export([wasm_state/3]).
%%% Test API:
-export([store_wasm_image/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Boot a WASM image on the image stated in the `Process/Image' field of
%% the message.
init(M1, _M2, Opts) ->
    ImageID = 
        case hb_converge:get(<<"WASM-Image">>, M1, Opts) of
            not_found ->
                hb_converge:get(<<"Process/WASM-Image">>, M1, Opts);
            X -> X
        end,
    ?event({getting_wasm_image, ImageID}),
    {ok, ImageMsg} =
        hb_cache:read(
            ImageID,
            Opts
        ),
    % Start the WASM executor.
    {ok, Port, _ImportMap, _Exports} =
        hb_beamr:start(hb_converge:get(<<"Body">>, ImageMsg, Opts)),
    % Apply the checkpoint if it is in the initial state.
    case hb_converge:get(<<"WASM/State">>, M1, Opts) of
        not_found -> do_nothing;
        Checkpoint ->
            ?event(wasm_checkpoint_found),
            ?event({wasm_deserializing, byte_size(Checkpoint)}),
            % Apply the checkpoint to the WASM state.
            hb_beamr:deserialize(Port, Checkpoint#tx.data),
            ?event(wasm_deserialized)
    end,
    % Set the WASM port, handler, and standard library invokation function.
    {ok,
        hb_private:set(M1,
            #{
                <<"WASM/Port">> => Port,
                <<"WASM/Import-Resolver">> => fun default_import_resolver/3
            },
            Opts
        )
    }.

%% @doc Take a BEAMR import call and resolve it using `hb_converge`.
default_import_resolver(Msg1, Msg2, Opts) ->
    #{
        port := Port,
        module := Module,
        func := Func,
        args := Args,
        signature := Signature
    } = Msg2,
    {ok, Msg3} =
        hb_converge:resolve(
            Msg1,
            #{
                path => import,
                priv => #{ wasm => #{ port => Port } },
                module => list_to_binary(Module),
                func => list_to_binary(Func),
                args => Args,
                func_sig => Signature
            },
            Opts
        ),
    NextState = hb_converge:get(state, Msg3, Opts),
    Response = hb_converge:get(wasm_response, Msg3, Opts),
    {ok, Response, NextState}.

%% @doc Call the WASM executor with a message that has been prepared by a prior
%% pass.
compute(M1, M2, Opts) ->
    case hb_converge:get(pass, M1, Opts) of
        X when X == 1 orelse X == not_found ->
            % Extract the WASM port, func, params, and standard library
            % invokation from the message and apply them with the WASM executor.
            ?event({calling_wasm_executor, {msg1, M1}, {msg2, M2}}),
            WASMFunction =
                case hb_converge:get(<<"Message/WASM-Function">>, M2, Opts) of
                    not_found ->
                        hb_converge:get(<<"WASM-Function">>, M1, Opts);
                    Func -> Func
                end,
            WASMParams =
                case hb_converge:get(<<"Message/WASM-Params">>, M2, Opts) of
                    not_found ->
                        hb_converge:get(<<"WASM-Params">>, M1, Opts);
                    Params -> Params
                end,
            {ResType, Res, MsgAfterExecution} =
                hb_beamr:call(
                    hb_private:get(<<"WASM/Port">>, M1, Opts),
                    WASMFunction,
                    WASMParams,
                    hb_private:get(<<"WASM/Import-Resolver">>, M1, Opts),
                    M1,
                    Opts
                ),
            {ok,
                hb_converge:set(MsgAfterExecution,
                    #{
                        <<"Results/WASM/Type">> => ResType,
                        <<"Results/WASM/Output">> => Res
                    }
                )
            };
        _ -> {ok, M1}
    end.

%% @doc Serialize the WASM state to a message.
wasm_state(M1, _M2, Opts) ->
    Port = hb_private:get(<<"priv/WASM/Port">>, M1, Opts),
    {ok, Serialized} = hb_beamr:serialize(Port),
    {ok, Serialized}.

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

%% @doc Handle standard library calls by:
%% 1. Adding the right prefix to the path from BEAMR.
%% 2. Adding the state to the message at the stdlib path.
%% 3. Resolving the adjusted-path-Msg2 against the added-state-Msg1.
%% 4. If it succeeds, return the new state from the message.
%% 5. If it fails with `not_found', call the stub handler.
import(Msg1, Msg2, Opts) ->
    ModName = hb_converge:get(<<"Module">>, Msg2, Opts),
    FuncName = hb_converge:get(<<"Func">>, Msg2, Opts),
    AdjustedPath = [ <<"WASM">>, <<"stdlib">>, ModName, FuncName ],
    AdjustedMsg2 = Msg2#{ path => AdjustedPath },
    %?event({invoking_stdlib, {explicit, Msg1}, {explicit, Msg2}}),
    % 1. Adjust the path to the stdlib.
    % 2. Add the current state to the message at the stdlib path.
    AdjustedMsg1 =
        hb_converge:set(
            Msg1,
            lists:droplast(AdjustedPath) ++ [<<"State">>],
            Msg1,
            Opts
        ),
    %?event({path_adjusted, AdjustedMsg2}),
    %?event({state_added_msg1, AdjustedMsg1}),
    % 3. Resolve the adjusted path against the added state.
    case hb_converge:resolve(AdjustedMsg1, AdjustedMsg2, Opts) of
        {ok, Res} ->
            % 4. Success. Return.
            {ok, Res};
        {error, not_found} ->
            ?event(stdlib_not_found),
            % 5. Failure. Call the stub handler.
            undefined_import_stub(Msg1, Msg2, Opts)
    end.

%% @doc Log the call to the standard library as an event, and write the
%% call details into the message.
undefined_import_stub(Msg1, Msg2, Opts) ->
    ?event({unimplemented_dev_wasm_call, {msg1, Msg1}, {msg2, Msg2}}),
    Msg3 = hb_converge:set(
        Msg1,
        #{<<"Results/WASM/Unimplemented-Calls">> =>
            [
                Msg2
            |
                case hb_converge:get(
                    <<"Results/WASM/Undefined-Calls">>,
                    Msg1,
                    Opts
                ) of
                    not_found -> [];
                    X -> X
                end
            ]
        }
    ),
    {ok, #{ state => Msg3, wasm_response => [0] }}.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    hb:init().

store_wasm_image(Image) ->
    {ok, Bin} = file:read_file(Image),
    Msg = #{ <<"Body">> => Bin },
    {ok, ID} = hb_cache:write(Msg, #{}),
    #{
        device => <<"WASM-64/1.0">>,
        <<"WASM-Image">> => ID
    }.

test_run_wasm(File, Func, Params, AdditionalMsg) ->
    init(),
    Msg0 = store_wasm_image(File),
    {ok, Msg1} = hb_converge:resolve(Msg0, <<"Init">>, #{}),
    ?event({after_init, Msg1}),
    Msg2 =
        maps:merge(
            Msg1,
            hb_converge:set(
                #{
                    <<"WASM-Function">> => Func,
                    <<"WASM-Params">> => Params
                },
                AdditionalMsg,
                #{ hashpath => ignore }
            )
        ),
    ?event({after_setup, Msg2}),
    {ok, StateRes} = hb_converge:resolve(Msg2, <<"Compute">>, #{}),
    ?event({after_resolve, StateRes}),
    hb_converge:resolve(StateRes, <<"Results/WASM/Output">>, #{}).

basic_execution_test() ->
    ?assertEqual(
        {ok, [120.0]},
        test_run_wasm("test/test.wasm", <<"fac">>, [5.0], #{})
    ).

basic_execution_64_test() ->
    ?assertEqual(
        {ok, [120.0]},
        test_run_wasm("test/test-64.wasm", <<"fac">>, [5.0], #{})
    ).

imported_function_test() ->
    ?assertEqual(
        {ok, [32]},
        test_run_wasm(
            "test/pow_calculator.wasm",
            <<"pow">>,
            [2, 5],
            #{
                <<"WASM/stdlib/my_lib">> =>
                    #{ device => <<"Test-Device/1.0">> }
            }
        )
    ).