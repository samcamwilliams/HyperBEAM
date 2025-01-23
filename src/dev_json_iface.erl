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
%%%             M1/priv/wasm/instance
%%%             M1/Process
%%%             M2/Message
%%%             M2/Assignment/Block-Height
%%%         Generates:
%%%             /wasm/handler
%%%             /wasm/params
%%%         Side-effects:
%%%             Writes the process and message as JSON representations into the
%%%             WASM environment.
%%% 
%%%     M1/Computed when M2/Pass == 2 ->
%%%         Assumes:
%%%             M1/priv/wasm/instance
%%%             M2/Results
%%%             M2/Process
%%%         Generates:
%%%             /Results/Outbox
%%%             /Results/Data'''

-module(dev_json_iface).
-export([init/3, compute/3]).
%%% Test helper exports:
-export([generate_stack/1, generate_stack/2, generate_aos_msg/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Initialize the device.
init(M1, _M2, _Opts) ->
    {ok, hb_converge:set(M1, #{<<"wasm-function">> => <<"handle">>})}.

%% @doc On first pass prepare the call, on second pass get the results.
compute(M1, M2, Opts) ->
    case hb_converge:get(<<"pass">>, M1, Opts) of
        1 -> prep_call(M1, M2, Opts);
        2 -> results(M1, M2, Opts);
        _ -> {ok, M1}
    end.

%% @doc Prepare the WASM environment for execution by writing the process string and
%% the message as JSON representations into the WASM environment.
prep_call(M1, M2, Opts) ->
    ?event({prep_call, M1, M2, Opts}),
    Instance = hb_private:get(<<"priv/wasm/instance">>, M1, Opts),
    Process = hb_converge:get(<<"process">>, M1, Opts#{ hashpath => ignore }),
    Assignment = hb_converge:get(<<"assignment">>, M2, Opts#{ hashpath => ignore }),
    Message = hb_converge:get(<<"message">>, M2, Opts#{ hashpath => ignore }),
    Image = hb_converge:get(<<"process/image">>, M1, Opts),
    BlockHeight = hb_converge:get(<<"block-height">>, Assignment, Opts),
    RawMsgJson =
        ar_bundles:item_to_json_struct(
            hb_message:convert(Message, tx, converge, #{})
        ),
    {Props} = RawMsgJson,
    MsgProps =
        normalize_props(
            Props ++
                [
                    {<<"Module">>, Image},
                    {<<"Block-Height">>, BlockHeight}
                ]
        ),
    MsgJson = jiffy:encode({MsgProps}),
    {ok, MsgJsonPtr} = hb_beamr_io:write_string(Instance, MsgJson),
    ProcessProps =
        normalize_props(
            [
                {<<"Process">>,
                    ar_bundles:item_to_json_struct(
                                hb_message:convert(Process, tx, converge, #{})
                            )
                        }
            ]
        ),
    ProcessJson = jiffy:encode({ProcessProps}),
    {ok, ProcessJsonPtr} = hb_beamr_io:write_string(Instance, ProcessJson),
    {ok,
        hb_converge:set(
            M1,
            #{
                <<"wasm-function">> => <<"handle">>,
                <<"wasm-params">> => [MsgJsonPtr, ProcessJsonPtr]
            },
            Opts
        )
    }.

%% @doc Normalize the properties of a message to begin with a capital letter for
%% backwards compatibility with AOS.
normalize_props(Props) ->
    lists:map(
        fun({<<"Tags">>, Values}) ->
            {<<"Tags">>,
                lists:map(
                    fun({[{name, Name}, {value, Value}]}) ->
                        {
                            [
                                {name, header_case_string(Name)},
                                {value, Value}
                            ]
                        }
                    end,
                    Values
                )
            };
        ({Key, Value}) ->
            {header_case_string(Key), Value}
        end,
        Props
    ).

header_case_string(Key) ->
    ?event({header_casing, Key}),
    NormKey = hb_converge:normalize_key(Key),
    Words = string:lexemes(NormKey, "-"),
    ?event({words, Words}),
    TitleCaseWords =
        lists:map(
            fun binary_to_list/1,
            lists:map(
                fun string:titlecase/1,
                Words
            )
        ),
    TitleCaseKey = list_to_binary(string:join(TitleCaseWords, "-")),
    ?event({titlecase, TitleCaseKey}),
    TitleCaseKey.

%% @doc Read the computed results out of the WASM environment, assuming that
%% the environment has been set up by `prep_call/3' and that the WASM executor
%% has been called with `computed{pass=1}'.
results(M1, _M2, Opts) ->
    Instance = hb_private:get(<<"priv/wasm/instance">>, M1, Opts),
    Type = hb_converge:get(<<"results/wasm/type">>, M1, Opts),
    Proc = hb_converge:get(<<"process">>, M1, Opts),
    case hb_converge:normalize_key(Type) of
        <<"error">> ->
            {error,
                hb_converge:set(
                    M1,
                    #{
                        <<"outbox">> => undefined,
                        <<"results">> => 
                            #{
                                <<"body">> => <<"WASM execution error.">>
                            }
                    },
                    Opts
                )
            };
        <<"ok">> ->
            [Ptr] = hb_converge:get(<<"results/wasm/output">>, M1, Opts),
            {ok, Str} = hb_beamr_io:read_string(Instance, Ptr),
            try jiffy:decode(Str, [return_maps]) of
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    {ok, Data, Messages} = normalize_results(Resp),
                    Output = 
                        hb_converge:set(
                            M1,
                            #{
                                <<"results/outbox">> =>
                                    maps:from_list([
                                        {MessageNum, preprocess_results(Msg, Proc, Opts)}
                                    ||
                                        {MessageNum, Msg} <-
                                            lists:zip(
                                                lists:seq(1, length(Messages)),
                                                Messages
                                            )
                                    ]),
                                <<"results/data">> => Data
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
                                <<"results/outbox">> => undefined,
                                <<"results/body">> =>
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
normalize_results(#{ <<"error">> := Error }) ->
    {ok, Error, []}.

%% @doc After the process returns messages from an evaluation, the
%% signing node needs to add some tags to each message and spawn such that
%% the target process knows these messages are created by a process.
preprocess_results(Msg, Proc, Opts) ->
    NormMsg = hb_converge:normalize_keys(Msg),
    RawTags = maps:get(<<"tags">>, NormMsg, []),
    TagList =
        [
            {maps:get(<<"name">>, Tag), maps:get(<<"value">>, Tag)}
        ||
            Tag <- RawTags ],
    Tags = maps:from_list(TagList),
    FilteredMsg =
        maps:without(
            [<<"from-process">>, <<"from-image">>, <<"anchor">>, <<"tags">>],
            NormMsg
        ),
    maps:merge(
        maps:from_list(
            lists:map(
                fun({Key, Value}) ->
                    {hb_converge:normalize_key(Key), Value}
                end,
                maps:to_list(FilteredMsg)
            )
        ),
        Tags#{
            <<"from-process">> => hb_converge:get(id, Proc, Opts),
            <<"from-image">> => hb_converge:get(<<"image">>, Proc, Opts)
        }
    ).

%%% Tests

test_init() ->
    application:ensure_all_started(hb).

generate_stack(File) ->
    generate_stack(File, <<"WASM">>).
generate_stack(File, Mode) ->
    test_init(),
    Wallet = hb:wallet(),
    Msg0 = dev_wasm:cache_wasm_image(File),
    Image = hb_converge:get(<<"image">>, Msg0, #{}),
    Msg1 = Msg0#{
        <<"device">> => <<"Stack@1.0">>,
        <<"device-stack">> =>
            [
                <<"WASI@1.0">>,
                <<"JSON-Iface@1.0">>,
                <<"WASM-64@1.0">>,
                <<"Multipass@1.0">>
            ],
        <<"input-prefix">> => <<"process">>,
        <<"output-prefix">> => <<"wasm">>,
        <<"passes">> => 2,
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"process">> => 
            hb_message:sign(#{
                <<"type">> => <<"Process">>,
                <<"image">> => Image,
                <<"scheduler">> => hb:address(),
                <<"authority">> => hb:address()
            }, Wallet)
    },
    {ok, Msg2} = hb_converge:resolve(Msg1, <<"init">>, #{}),
    Msg2.

generate_aos_msg(ProcID, Code) ->
    Wallet = hb:wallet(),
    #{
        <<"path">> => <<"compute">>,
        <<"message">> => 
            hb_message:sign(#{
                <<"Action">> => <<"Eval">>,
                <<"Data">> => Code,
                <<"Target">> => ProcID
            }, Wallet),
        <<"assignment">> =>
            hb_message:sign(#{ <<"block-height">> => 1 }, Wallet)
    }.

basic_aos_call_test() ->
    Msg = generate_stack("test/aos-2-pure-xs.wasm"),
    Proc = hb_converge:get(<<"process">>, Msg, #{ hashpath => ignore }),
    ProcID = hb_converge:get(id, Proc, #{}),
    {ok, Msg3} =
        hb_converge:resolve(
            Msg,
            generate_aos_msg(ProcID, <<"return 1+1">>),
            #{}
        ),
    ?event({res, Msg3}),
    Data = hb_converge:get(<<"results/data">>, Msg3, #{}),
    ?assertEqual(<<"2">>, Data).

aos_stack_benchmark_test_() ->
    {timeout, 20, fun() ->
        BenchTime = 3,
        RawWASMMsg = generate_stack("test/aos-2-pure-xs.wasm"),
        Proc = hb_converge:get(<<"process">>, RawWASMMsg, #{ hashpath => ignore }),
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
