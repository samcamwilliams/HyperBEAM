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
-export([wasm_state/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Boot a WASM image on the image stated in the `Process/Image' field of
%% the message.
init(M1, _M2, Opts) ->
    ImageID = hb_converge:get(<<"WASM-Image">>, M1, Opts),
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
                <<"WASM/Invoke-stdlib">> => fun invoke_stdlib/3
            },
            Opts
        )
    }.

%% @doc Call the WASM executor with a message that has been prepared by a prior
%% pass.
computed(M1, _M2, Opts) ->
    case hb_converge:get(pass, M1, Opts) of
        X when X == 1 orelse X == not_found ->
            % Extract the WASM port, func, params, and standard library
            % invokation from the message and apply them with the WASM executor.
            {ResType, Res, MsgAfterExecution} =
                hb_beamr:call(
                    hb_private:get(<<"WASM/Port">>, M1, Opts),
                    hb_converge:get(<<"WASM-Function">>, M1, Opts),
                    hb_converge:get(<<"WASM-Params">>, M1, Opts),
                    hb_private:get(<<"WASM/Invoke-stdlib">>, M1, Opts),
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

%% @doc Handle standard library calls by looking up the function in the
%% message and calling it. Calls the stub function if the function is not
%% found in the message.
invoke_stdlib(Msg1, Msg2, Opts) ->
    [ModName, FuncName] =
        hb_converge:get(path, Msg2, Opts#{ hashpath => ignore }),
    MaybeFunc =
        hb_private:get(
            <<"priv/WASM/stdlib/", ModName/bitstring, "/", FuncName/bitstring>>,
            Msg1,
            Opts
        ),
    case MaybeFunc of
        not_found ->
            lib(Msg1, Msg2, Opts);
        Func ->
            erlang:apply(
                Func,
                hb_converge:truncate_args(
                    Func,
                    [Msg1, Msg2, Opts]
                )
            )
    end.

%% @doc Log the call to the standard library as an event, and write the
%% call details into the message.
lib(Msg1, Msg2, Opts) ->
    ?event({unimplemented_dev_wasm_call, {msg1, Msg1}, {msg2, Msg2}}),
    Msg3 = hb_converge:set(
        Msg1,
        #{<<"Results/WASM/Unimplemented-Calls">> =>
            [
                Msg2
            |
                hb_converge:get(
                    <<"Results/WASM/Undefined-Calls">>,
                    Msg1,
                    [],
                    Opts
                )
            ]
        }
    ),
    {ok, #{ state => Msg3, wasm_response => [0] }}.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    hb:init().

generate_basic_wasm_message(Image) ->
    {ok, Bin} = file:read_file(Image),
    Msg = #{ <<"Body">> => Bin },
    {ok, ID} = hb_cache:write(Msg, #{}),
    #{
        device => <<"WASM-64/1.0">>,
        <<"WASM-Image">> => ID
    }.

basic_execution_test() ->
    init(),
    Msg0 = generate_basic_wasm_message("test/test.wasm"),
    {ok, Msg1} =
        hb_converge:resolve(
            Msg0#{
                <<"WASM-Function">> => <<"fac">>,
                <<"WASM-Params">> => [5.0]
            },
            <<"Init">>,
            #{}
        ),
    ?event({after_init, Msg1}),
    {ok, Res} = hb_converge:resolve(Msg1, <<"Computed">>, #{}),
    ?event({after_computed, Res}),
    ?assertEqual(
        [120.0],
        hb_converge:get(<<"Results/WASM/Output">>, Res)
    ).

imported_function_test() ->
    init(),
    % Load the WASM module.
    Msg0 = generate_basic_wasm_message("test/pow_calculator.wasm"),
    % Add the import function to the private stdlib.
    Msg1 =
        hb_private:set(
            Msg0,
            #{
                <<"WASM/stdlib/my_lib/mul">> =>
                    fun hb_beamr:test_pow_import_function/2
            },
            #{}
        ),
    % Resolve the `Init` message.
    {ok, Msg2} =
        hb_converge:resolve(
            Msg1#{
                <<"WASM-Function">> => <<"pow">>,
                <<"WASM-Params">> => [2, 5]
            },
            <<"Init">>,
            #{}
        ),
    ?event({after_init, Msg2}),
    % Resolve the `Computed` message.
    {ok, Res} = hb_converge:resolve(Msg2, <<"Computed">>, #{}),
    ?event({after_computed, Res}),
    ?assertEqual([32], hb_converge:get(<<"Results/WASM/Output">>, Res)).
