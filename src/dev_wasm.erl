%%% @doc A device that executes a WASM image on messages using the Memory-64 
%%% preview standard. In the backend, this device uses `beamr`: An Erlang wrapper 
%%% for WAMR, the WebAssembly Micro Runtime.
%%% 
%%% The device has the following requirements and interface:
%%% ```
%%%     M1/Init ->
%%%         Assumes:
%%%             M1/Process
%%%             M1/[Prefix]/Image
%%%         Generates:
%%%             /priv/WASM/Instance
%%%             /priv/WASM/Import-Resolver
%%%         Side-effects:
%%%             Creates a WASM executor loaded in memory of the HyperBEAM node.
%%% 
%%%     M1/Compute ->
%%%         Assumes:
%%%             M1/priv/WASM/Instance
%%%             M1/priv/WASM/Import-Resolver
%%%             M1/Process
%%%             M2/Message
%%%             M2/Message/WASM-Function OR M1/WASM-Function
%%%             M2/Message/WASM-Params OR M1/WASM-Params
%%%         Generates:
%%%             /Results/WASM/Type
%%%             /Results/WASM/Body
%%%         Side-effects:
%%%             Calls the WASM executor with the message and process.
%%%     M1/WASM/State ->
%%%         Assumes:
%%%             M1/priv/WASM/Instance
%%%         Generates:
%%%             Raw binary WASM state
%%% '''
-module(dev_wasm).
-export([info/2, init/3, compute/3, import/3, terminate/3, snapshot/3, normalize/3]).
%%% API for other devices:
-export([instance/3]).
%%% Test API:
-export([cache_wasm_image/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export all functions aside the `instance/3' function.
info(_Msg1, _Opts) ->
    #{
        exclude => [instance]
    }.

%% @doc Boot a WASM image on the image stated in the `Process/Image' field of
%% the message.
init(M1, M2, Opts) ->
    ?event(running_init),
    % Where we should read initial parameters from.
    InPrefix = dev_stack:input_prefix(M1, M2, Opts),
    % Where we should read/write our own state to.
    Prefix = dev_stack:prefix(M1, M2, Opts),
    ?event({in_prefix, InPrefix}),
    ImageBin =
        case hb_converge:get(<<InPrefix/binary, "/Image">>, M1, Opts) of
            not_found ->
                throw(
                    {
                        wasm_init_error,
                        <<
                            "No viable image found in ",
                            InPrefix/binary,
                            "/Image."
                        >>,
                        {msg1, M1}
                    }
                );
            ImageID when ?IS_ID(ImageID) ->
                ?event({getting_wasm_image, ImageID}),
                {ok, ImageMsg} = hb_cache:read(ImageID, Opts),
                hb_converge:get(<<"Body">>, ImageMsg, Opts);
            ImageMsg when is_map(ImageMsg) ->
                ?event(wasm_image_message_directly_provided),
                hb_converge:get(<<"Body">>, ImageMsg, Opts);
            Image when is_binary(Image) ->
                ?event(wasm_image_binary_directly_provided),
                Image
        end,
    % Start the WASM executor.
    {ok, Instance, _Imports, _Exports} = hb_beamr:start(ImageBin),
    % Set the WASM Instance, handler, and standard library invokation function.
    ?event({setting_wasm_instance, Instance, {prefix, Prefix}}),
    {ok,
        hb_private:set(M1,
            #{
                <<Prefix/binary, "/Instance">> => Instance,
                <<Prefix/binary, "/Import-Resolver">> =>
                    fun default_import_resolver/3
            },
            Opts
        )
    }.

%% @doc Take a BEAMR import call and resolve it using `hb_converge`.
default_import_resolver(Msg1, Msg2, Opts) ->
    #{
        instance := WASM,
        module := Module,
        func := Func,
        args := Args,
        func_sig := Signature
    } = Msg2,
    Prefix = dev_stack:prefix(Msg1, Msg2, Opts),
    {ok, Msg3} =
        hb_converge:resolve(
            hb_private:set(
                Msg1,
                #{ <<Prefix/binary, "/Instance">> => WASM },
                Opts
            ),
            #{
                path => import,
                module => list_to_binary(Module),
                func => list_to_binary(Func),
                args => Args,
                func_sig => list_to_binary(Signature)
            },
            Opts
        ),
    NextState = hb_converge:get(state, Msg3, Opts),
    Response = hb_converge:get(results, Msg3, Opts),
    {ok, Response, NextState}.

%% @doc Call the WASM executor with a message that has been prepared by a prior
%% pass.
compute(RawM1, M2, Opts) ->
    % Normalize the message to have an open WASM instance, but no literal `State`.
    % The hashpath is not updated during this process. This allows us to take
    % two different messages and get the same result:
    % - A message with a `State' key but no WASM instance in `priv/`.
    % - A message with a WASM instance in `priv/` but no `State' key.
    {ok, M1} = normalize(RawM1, M2, Opts),
    ?event(running_compute),
    Prefix = dev_stack:prefix(M1, M2, Opts),
    case hb_converge:get(pass, M1, Opts) of
        X when X == 1 orelse X == not_found ->
            % Extract the WASM Instance, func, params, and standard library
            % invokation from the message and apply them with the WASM executor.
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
            ?event(
                {
                    calling_wasm_executor,
                    {prefix, Prefix},
                    {m1, M1},
                    {m2, M2},
                    {priv, hb_private:from_message(M1)}
                }
            ),
            {ResType, Res, MsgAfterExecution} =
                hb_beamr:call(
                    instance(M1, M2, Opts),
                    WASMFunction,
                    WASMParams,
                    hb_private:get(<<Prefix/binary, "/Import-Resolver">>, M1, Opts),
                    M1,
                    Opts
                ),
            {ok,
                hb_converge:set(MsgAfterExecution,
                    #{
                        <<"Results/", Prefix/binary, "/Type">> => ResType,
                        <<"Results/", Prefix/binary, "/Output">> => Res
                    }
                )
            };
        _ -> {ok, M1}
    end.

%% @doc Normalize the message to have an open WASM instance, but no literal
%% `State' key. Ensure that we do not change the hashpath during this process.
normalize(RawM1, M2, Opts) ->
    ?event({normalize_raw_m1, RawM1}),
    M3 = 
        case instance(RawM1, M2, Opts) of
            not_found ->
                DeviceKey =
                    case hb_converge:get(<<"Device-Key">>, RawM1, Opts) of
                        not_found -> [];
                        Key -> [Key]
                    end,
                ?event(debug,
                    {no_instance_attempting_to_get_snapshot,
                        {msg1, RawM1}, {device_key, DeviceKey}
                    }
                ),
                Memory = 
                    hb_converge:get(
                        [<<"Snapshot">>] ++ DeviceKey ++ [<<"body">>],
                        {as, dev_message, RawM1},
                        Opts
                    ),
                case Memory of
                    not_found -> throw({error, no_wasm_instance_or_});
                    State ->
                        ?event(debug, {state_found, {state, State}}),
                        {ok, M1} = init(RawM1, State, Opts),
                        Res = hb_beamr:deserialize(instance(M1, M2, Opts), State),
                        ?event(debug, {wasm_deserialized, {result, Res}}),
                        M1
                end;
            _ ->
                ?event(debug, wasm_instance_found_not_deserializing),
                RawM1
        end,
    dev_message:set(M3, #{ <<"Snapshot">> => unset }, Opts).

%% @doc Serialize the WASM state to a binary.
snapshot(M1, M2, Opts) ->
    ?event(debug, generating_snapshot),
    Instance = instance(M1, M2, Opts),
    {ok, Serialized} = hb_beamr:serialize(Instance),
    {ok,
        #{
            body => Serialized
        }
    }.

%% @doc Tear down the WASM executor.
terminate(M1, M2, Opts) ->
    ?event(terminate_called_on_dev_wasm),
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Instance = instance(M1, M2, Opts),
    hb_beamr:stop(Instance),
    {ok, hb_private:set(M1,
        #{
            <<Prefix/binary, "/Instance">> => unset
        },
        Opts
    )}.

%% @doc Get the WASM instance from the message. Note that this function is exported
%% such that other devices can use it, but it is excluded from calls from Converge
%% resolution directly.
instance(M1, M2, Opts) ->
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Path = <<Prefix/binary, "/Instance">>,
    ?event({searching_for_instance, Path, M1}),
    hb_private:get(Path, M1, Opts#{ hashpath => ignore }).

%% @doc Handle standard library calls by:
%% 1. Adding the right prefix to the path from BEAMR.
%% 2. Adding the state to the message at the stdlib path.
%% 3. Resolving the adjusted-path-Msg2 against the added-state-Msg1.
%% 4. If it succeeds, return the new state from the message.
%% 5. If it fails with `not_found', call the stub handler.
import(Msg1, Msg2, Opts) ->
    % 1. Adjust the path to the stdlib.
    ModName = hb_converge:get(<<"Module">>, Msg2, Opts),
    FuncName = hb_converge:get(<<"Func">>, Msg2, Opts),
    Prefix = dev_stack:prefix(Msg1, Msg2, Opts),
    AdjustedPath =
        <<
            Prefix/binary,
            "/stdlib/",
            ModName/binary,
            "/",
            FuncName/binary
        >>,
    StatePath =
        <<
            Prefix/binary,
            "/stdlib/",
            ModName/binary,
            "/State"
        >>,
    AdjustedMsg2 = Msg2#{ path => AdjustedPath },
    % 2. Add the current state to the message at the stdlib path.
    AdjustedMsg1 =
        hb_converge:set(
            Msg1,
            #{ StatePath => Msg1 },
            Opts#{ hashpath => ignore }
        ),
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
    Prefix = dev_stack:prefix(Msg1, Msg2, Opts),
    UndefinedCallsPath =
        <<"State/Results/", Prefix/binary, "/Undefined-Calls">>,
    Msg3 = hb_converge:set(
        Msg1,
        #{
            UndefinedCallsPath =>
                [
                    Msg2
                |
                    case hb_converge:get(UndefinedCallsPath, Msg1, Opts) of
                        not_found -> [];
                        X -> X
                    end
                ]
        }
    ),
    {ok, #{ state => Msg3, results => [0] }}.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    hb:init().

init_test() ->
    init(),
    Msg = cache_wasm_image("test/test.wasm"),
    {ok, Msg1} = hb_converge:resolve(Msg, <<"Init">>, #{}),
    ?event({after_init, Msg1}),
    Priv = hb_private:from_message(Msg1),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_converge:resolve(Priv, <<"Instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_converge:resolve(Priv, <<"Import-Resolver">>, #{})
    ).

input_prefix_test() ->
    init(),
    #{ image := ImageID } = cache_wasm_image("test/test.wasm"),
    Msg1 =
        #{
            <<"Device">> => <<"WASM-64/1.0">>,
            <<"Input-Prefix">> => <<"Test-In">>,
            <<"Test-In">> => #{ <<"Image">> => ImageID }
        },
    {ok, Msg2} = hb_converge:resolve(Msg1, <<"Init">>, #{}),
    ?event({after_init, Msg2}),
    Priv = hb_private:from_message(Msg2),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_converge:resolve(Priv, <<"Instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_converge:resolve(Priv, <<"Import-Resolver">>, #{})
    ).

%% @doc Test that realistic prefixing for a `dev_process` works --
%% including both inputs (from `Process/`) and outputs (to the 
%% Device-Key) work
process_prefixes_test() ->
    init(),
    Msg1 =
        #{
            <<"Device">> => <<"WASM-64/1.0">>,
            <<"Output-Prefix">> => <<"WASM">>,
            <<"Input-Prefix">> => <<"Process">>,
            <<"Process">> => cache_wasm_image("test/test.wasm")
        },
    {ok, Msg3} = hb_converge:resolve(Msg1, <<"Init">>, #{}),
    ?event({after_init, Msg3}),
    Priv = hb_private:from_message(Msg3),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_converge:resolve(Priv, <<"WASM/Instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_converge:resolve(Priv, <<"WASM/Import-Resolver">>, #{})
    ).

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
                <<"stdlib/my_lib">> =>
                    #{ device => <<"Test-Device/1.0">> }
            }
        )
    ).

benchmark_test() ->
    BenchTime = 0.5,
    init(),
    Msg0 = cache_wasm_image("test/test-64.wasm"),
    {ok, Msg1} = hb_converge:resolve(Msg0, <<"Init">>, #{}),
    Msg2 =
        maps:merge(
            Msg1,
            hb_converge:set(
                #{
                    <<"WASM-Function">> => <<"fac">>,
                    <<"WASM-Params">> => [5.0]
                },
                #{ hashpath => ignore }
            )
        ),
    Iterations =
        hb:benchmark(
            fun() ->
                hb_converge:resolve(Msg2, <<"Compute">>, #{})
            end,
            BenchTime
        ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_util:eunit_print(
        "Evaluated ~p WASM messages through Converge in ~p seconds (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 5),
    ok.

state_export_and_restore_test() ->
    init(),
    % Generate a WASM message. We use the pow_calculator because it has a 
    % reasonable amount of memory to work with.
    Msg0 = cache_wasm_image("test/pow_calculator.wasm"),
    {ok, Msg1} = hb_converge:resolve(Msg0, <<"Init">>, #{}),
    Msg2 =
        maps:merge(
            Msg1,
            Extras = #{
                <<"WASM-Function">> => <<"pow">>,
                <<"WASM-Params">> => [2, 2],
                <<"stdlib">> =>
                    #{
                        <<"my_lib">> =>
                            #{ device => <<"Test-Device/1.0">> }
                    }
            }
        ),
    ?event({after_setup, Msg2}),
    % Compute a computation and export the state.
    {ok, Msg3a} = hb_converge:resolve(Msg2, <<"Compute">>, #{}),
    ?assertEqual([4], hb_converge:get(<<"Results/Output">>, Msg3a, #{})),
    {ok, State} = hb_converge:resolve(Msg3a, <<"Snapshot">>, #{}),
    ?event({state_res, State}),
    % Restore the state without calling Init.
    NewMsg1 = maps:merge(Msg0, Extras#{ <<"Snapshot">> => State }),
    ?assertEqual(
        {ok, [4]},
        hb_converge:resolve(NewMsg1, <<"Compute/Results/Output">>, #{})
    ).

%%% Test helpers

cache_wasm_image(Image) ->
    {ok, Bin} = file:read_file(Image),
    Msg = #{ <<"Body">> => Bin },
    {ok, ID} = hb_cache:write(Msg, #{}),
    #{
        device => <<"WASM-64/1.0">>,
        image => ID
    }.

test_run_wasm(File, Func, Params, AdditionalMsg) ->
    init(),
    Msg0 = cache_wasm_image(File),
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
    hb_converge:resolve(StateRes, <<"Results/Output">>, #{}).