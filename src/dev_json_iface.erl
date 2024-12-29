%%% @doc A device that provides a way for WASM execution to interact with
%%% the HyperBEAM (and AO) systems, using JSON as a shared data representation.
%%% 
%%% The interface is easy to use. It works as follows:
%%% 
%%% 1. The device is given a message that contains a process definition, WASM
%%%    environment, and a message that contains the data to be processed,
%%%    including the image to be used in part of `execute{pass=1}'.
%%% 2. The device is called with `execute{pass=2}', which reads the result of
%%%    the process execution from the WASM environment and adds it to the
%%%    message.
%%%
%%% The device has the following requirements and interface:
%%%     ```
%%%     M1/Computed when /Pass == 1 ->
%%%         Assumes:
%%%             M1/priv/WASM/Instance
%%%             M1/Process
%%%             M2/Message
%%%             M2/Assignment/Block-Height
%%%         Generates:
%%%             /WASM/Handler
%%%             /WASM/Params
%%%         Side-effects:
%%%             Writes the process and message as JSON representations into the
%%%             WASM environment.
%%% 
%%%     M1/Computed when M2/Pass == 2 ->
%%%         Assumes:
%%%             M1/priv/WASM/Instance
%%%             M2/Results
%%%             M2/Process
%%%         Generates:
%%%             /Results/Outbox
%%%             /Results/Data'''

-module(dev_json_iface).
-export([init/3, compute/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Initialize the device.
init(M1, _M2, _Opts) ->
    {ok, hb_converge:set(M1, #{<<"WASM-Function">> => <<"handle">>})}.

%% @doc On first pass prepare the call, on second pass get the results.
compute(M1, M2, Opts) ->
    case hb_converge:get(<<"Pass">>, M1, Opts) of
        1 -> prep_call(M1, M2, Opts);
        2 -> results(M1, M2, Opts);
        _ -> {ok, M1}
    end.

%% @doc Prepare the WASM environment for execution by writing the process string and
%% the message as JSON representations into the WASM environment.
prep_call(M1, M2, Opts) ->
    Instance = hb_private:get(<<"priv/WASM/Instance">>, M1, Opts),
    Process = hb_converge:get(<<"Process">>, M1, Opts, #{ hashpath => ignore }),
    Assignment = hb_converge:get(<<"Assignment">>, M2, Opts#{ hashpath => ignore }),
    Message = hb_converge:get(<<"Message">>, M2, Opts#{ hashpath => ignore }),
    Image = hb_converge:get(<<"Process/WASM-Image">>, M1, Opts),
    BlockHeight = hb_converge:get(<<"Block-Height">>, Assignment, Opts),
    RawMsgJson =
        ar_bundles:item_to_json_struct(
            hb_message:message_to_tx(Message)
        ),
    {Props} = RawMsgJson,
    MsgJson = jiffy:encode({
        Props ++
            [
                {<<"Module">>, Image},
                {<<"Block-Height">>, BlockHeight}
            ]
    }),
    {ok, MsgJsonPtr} = hb_beamr_io:write_string(Instance, MsgJson),
    ProcessJson =
        jiffy:encode(
            {
                [
                    {<<"Process">>,
                        ar_bundles:item_to_json_struct(
                            hb_message:message_to_tx(Process)
                        )
                    }
                ]
            }
        ),
    {ok, ProcessJsonPtr} = hb_beamr_io:write_string(Instance, ProcessJson),
    {ok,
        hb_converge:set(
            M1,
            #{
                <<"WASM-Function">> => <<"handle">>,
                <<"WASM-Params">> => [MsgJsonPtr, ProcessJsonPtr]
            },
            Opts
        )
    }.

%% @doc Read the computed results out of the WASM environment, assuming that
%% the environment has been set up by `prep_call/3' and that the WASM executor
%% has been called with `computed{pass=1}'.
results(M1, _M2, Opts) ->
    Instance = hb_private:get(<<"priv/WASM/Instance">>, M1, Opts),
    Type = hb_converge:get(<<"Results/WASM/Type">>, M1, Opts),
    Proc = hb_converge:get(<<"Process">>, M1, Opts),
    case hb_converge:to_key(Type) of
        error ->
            {error,
                hb_converge:set(
                    M1,
                    #{
                        <<"Outbox">> => undefined,
                        <<"Results">> => 
                            #{
                                <<"Body">> => <<"WASM execution error.">>
                            }
                    },
                    Opts
                )
            };
        ok ->
            [Ptr] = hb_converge:get(<<"Results/WASM/Output">>, M1, Opts),
            {ok, Str} = hb_beamr_io:read_string(Instance, Ptr),
            try jiffy:decode(Str, [return_maps]) of
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    {ok, Data, Messages} = normalize_results(Resp),
                    Output = 
                        hb_converge:set(
                            M1,
                            #{
                                <<"Results/Outbox">> =>
                                    maps:from_list([
                                        {MessageNum, preprocess_results(Msg, Proc, Opts)}
                                    ||
                                        {MessageNum, Msg} <-
                                            lists:zip(
                                                lists:seq(1, length(Messages)),
                                                Messages
                                            )
                                    ]),
                                <<"Results/Data">> => Data
                            },
                            Opts
                        ),
                    {ok, Output}
            catch
                _:_ ->
                    {error,
                        hb_converge:set(
                            M1,
                            #{
                                <<"Results/Outbox">> => undefined,
                                <<"Results/Body">> =>
                                    <<"JSON error parsing WASM result output.">>
                            },
                            Opts
                        )
                    }
            end
    end.

%% @doc Normalize the results of an evaluation.
normalize_results(
    #{ <<"Output">> := #{<<"data">> := Data}, <<"Messages">> := Messages }) ->
    {ok, Data, Messages};
normalize_results(#{ <<"Error">> := Error }) ->
    {ok, Error, []}.

%% @doc After the process returns messages from an evaluation, the
%% signing node needs to add some tags to each message and spawn such that
%% the target process knows these messages are created by a process.
preprocess_results(Msg, Proc, Opts) ->
    RawTags = maps:get(<<"Tags">>, Msg, []),
    TagList =
        [
            {maps:get(<<"name">>, Tag), maps:get(<<"value">>, Tag)}
        ||
            Tag <- RawTags ],
    Tags = maps:from_list(TagList),
    FilteredMsg =
        maps:without(
            [<<"From-Process">>, <<"From-Image">>, <<"Anchor">>, <<"Tags">>],
            Msg
        ),
    maps:merge(
        maps:from_list(
            lists:map(
                fun({Key, Value}) ->
                    {hb_converge:to_key(Key), Value}
                end,
                maps:to_list(FilteredMsg)
            )
        ),
        Tags#{
            <<"From-Process">> => hb_converge:get(id, Proc, Opts),
            <<"From-Image">> => hb_converge:get(<<"Image">>, Proc, Opts)
        }
    ).

%%% Tests

test_init() ->
    application:ensure_all_started(hb).

generate_stack(File) ->
    test_init(),
    Wallet = hb:wallet(),
    Msg0 = dev_wasm:store_wasm_image(File),
    Image = hb_converge:get(<<"Image">>, Msg0, #{}),
    Msg1 = Msg0#{
        device => <<"Stack/1.0">>,
        <<"Device-Stack">> =>
            [
                <<"WASI/1.0">>,
                <<"JSON-Iface/1.0">>,
                <<"WASM-64/1.0">>,
                <<"Multipass/1.0">>
            ],
        <<"Input-Prefixes">> =>
            [
                <<"Process">>,
                <<"Process">>,
                <<"Process">>,
                <<"Process">>
            ],
        <<"Output-Prefixes">> =>
            [
                <<"WASM">>,
                <<"WASM">>,
                <<"WASM">>,
                <<"WASM">>
            ],
        <<"Passes">> => 2,
        <<"Stack-Keys">> => [<<"Init">>, <<"Compute">>],
        <<"Process">> => 
            hb_message:sign(#{
                <<"Type">> => <<"Process">>,
                <<"Image">> => Image,
                <<"Scheduler">> => hb:address(),
                <<"Authority">> => hb:address()
            }, Wallet)
    },
    {ok, Msg2} = hb_converge:resolve(Msg1, <<"Init">>, #{}),
    Msg2.

generate_aos_msg(ProcID, Code) ->
    Wallet = hb:wallet(),
    #{
        path => <<"Compute">>,
        <<"Message">> => 
            hb_message:sign(#{
                <<"Action">> => <<"Eval">>,
                data => Code,
                target => ProcID
            }, Wallet),
        <<"Assignment">> =>
            hb_message:sign(#{ <<"Block-Height">> => 1 }, Wallet)
    }.

basic_aos_call_test() ->
    Msg = generate_stack("test/aos-2-pure-xs.wasm"),
    Proc = hb_converge:get(<<"Process">>, Msg, #{ hashpath => ignore }),
    ProcID = hb_converge:get(id, Proc, #{}),
    {ok, Msg3} =
        hb_converge:resolve(
            Msg,
            generate_aos_msg(ProcID, <<"return 1+1">>),
            #{}
        ),
    Data = hb_converge:get(<<"Results/Data">>, Msg3, #{}),
    ?assertEqual(<<"2">>, Data).

aos_stack_benchmark_test_() ->
    {timeout, 20, fun() ->
        BenchTime = 3,
        RawWASMMsg = generate_stack("test/aos-2-pure-xs.wasm"),
        Proc = hb_converge:get(<<"Process">>, RawWASMMsg, #{ hashpath => ignore }),
        ProcID = hb_converge:get(id, Proc, #{}),
        {ok, Initialized} =
        hb_converge:resolve(
            RawWASMMsg,
            generate_aos_msg(ProcID, <<"return 1">>),
            #{}
        ),
        Msg = generate_aos_msg(ProcID, <<"return 1+1">>),
        Iterations =
            hb:benchmark(
                fun() -> hb_converge:resolve(Initialized, Msg, #{}) end,
                BenchTime
            ),
        hb_util:eunit_print(
            "Evaluated ~p AOS messages (minimal stack) in ~p sec (~.2f msg/s)",
            [Iterations, BenchTime, Iterations / BenchTime]
        ),
        ?assert(Iterations > 10),
        ok
    end}.
