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
%%%             M1/priv/WASM/Port
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
%%%             M1/priv/WASM/Port
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
    ?event(running_init),
    {ok, hb_converge:set(M1, #{<<"WASM-Function">> => <<"handle">>})}.

%% @doc On first pass prepare the call, on second pass get the results.
compute(M1, M2, Opts) ->
    ?event(running_compute),
    case hb_converge:get(<<"Pass">>, M1, Opts) of
        1 -> prep_call(M1, M2, Opts);
        2 -> results(M1, M2, Opts);
        _ -> {ok, M1}
    end.

%% @doc Prepare the WASM environment for execution by writing the process string and
%% the message as JSON representations into the WASM environment.
prep_call(M1, M2, Opts) ->
    ?event({prep_call, {msg1, M1}, {msg2, M2}, {opts, Opts}}),
    Port = hb_private:get(<<"priv/WASM/Port">>, M1, Opts),
    Process = hb_converge:get(<<"Process">>, M1, Opts),
    Assignment = hb_converge:get(<<"Assignment">>, M2, Opts),
    Message = hb_converge:get(<<"Message">>, M2, Opts),
    Image = hb_converge:get(<<"Image">>, Message, Opts),
    BlockHeight = hb_converge:get(<<"Block-Height">>, Assignment, Opts),
    RawMsgJson =
        ar_bundles:item_to_json_struct(hb_message:message_to_tx(Message)),
    % {Props} = jiffy:decode(RawMsgJson),
    {Props} = RawMsgJson,
    MsgJson = jiffy:encode({
        Props ++
            [
                {<<"Module">>, Image},
                {<<"Block-Height">>, BlockHeight}
            ]
    }),
    {ok, MsgJsonPtr} = hb_beamr_io:write_string(Port, MsgJson),
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
    {ok, ProcessJsonPtr} = hb_beamr_io:write_string(Port, ProcessJson),
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
    Port = hb_private:get(<<"priv/WASM/Port">>, M1, Opts),
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
            {ok, Str} = hb_beamr_io:read_string(Port, Ptr),
            try jiffy:decode(Str, [return_maps]) of
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    % TODO: Handle all JSON interface output types.
                    #{
                        <<"Output">> := #{<<"data">> := Data},
                        <<"Messages">> := Messages
                    } = Resp,
                    Res = 
                        hb_converge:set(
                            M1,
                            #{
                                <<"Results/Outbox">> =>
                                    maps:from_list([
                                        {MessageNum, preprocess_results(Msg, Proc, Opts)}
                                    ||
                                        {MessageNum, Msg} <-
                                            lists:zip(lists:seq(1, length(Messages)), Messages)
                                    ]),
                                <<"Results/Data">> => Data
                            },
                            Opts
                        ),
                    {ok, Res}
            catch
                _:_ ->
                    {error,
                        hb_converge:set(
                            M1,
                            #{
                                <<"Results/Outbox">> => undefined,
                                <<"Results/Body">> => <<"JSON error parsing WASM result output.">>
                            },
                            Opts
                        )
                    }
            end
    end.

%% NOTE: After the process returns messages from an evaluation, the signing unit needs to add
%% some tags to each message, and spawn to help the target process know these messages are created
%% by a process.
preprocess_results(Msg, Proc, Opts) ->
    ?event({preprocess_results, {msg, Msg}, {proc, Proc}, {opts, Opts}}),
    RawTags = maps:get(<<"Tags">>, Msg, []),
    TagList = [ {maps:get(<<"name">>, Tag), maps:get(<<"value">>, Tag)} || Tag <- RawTags ],
    Tags = maps:from_list(TagList),
    ?event({preprocess_results, {tags, Tags}}),
    FilteredMsg =
        maps:without([<<"From-Process">>, <<"From-Image">>, <<"Anchor">>, <<"Tags">>], Msg),
    maps:merge(
        FilteredMsg,
        Tags#{
            <<"From-Process">> => hb_util:id(Proc, signed),
            <<"From-Image">> => hb_converge:get(<<"WASM-Image">>, Proc, Opts)
        }
    ).

%%% Tests

test_init() ->
    application:ensure_all_started(hb).

generate_stack(File) ->
    test_init(),
    Wallet = hb:wallet(),
    Msg0 = dev_wasm:store_wasm_image(File),
    Msg1 = Msg0#{
        device => <<"Stack/1.0">>,
        <<"Device-Stack">> =>
            [
                <<"WASI/1.0">>,
                <<"JSON-Iface/1.0">>,
                <<"WASM-64/1.0">>,
                <<"Multipass/1.0">>
            ],
        <<"Passes">> => 2,
        <<"Stack-Keys">> => [<<"Init">>, <<"Compute">>],
        <<"Process">> => 
            hb_message:sign(#{
                <<"Type">> => <<"Process">>,
                <<"WASM-Image">> => <<"test/aos-2-pure-xs.wasm">>,
                <<"Scheduler">> => hb:address(),
                <<"Authority">> => hb:address()
            }, Wallet)
    },
    ?event({msg1, Msg1}),
    {ok, Msg2} = hb_converge:resolve(Msg1, <<"Init">>, #{}),
    ?event({msg2, Msg2}),
    Msg2.

generate_aos_msg(Code) ->
    Wallet = hb:wallet(),
    #{
        path => <<"Compute">>,
        <<"Message">> => 
            hb_message:sign(#{
                <<"Action">> => <<"Eval">>,
                data => Code
            }, Wallet),
        <<"Assignment">> =>
            hb_message:sign(#{ <<"Block-Height">> => 1 }, Wallet)
    }.

basic_aos_call_test() ->
    Msg = generate_stack("test/aos-2-pure-xs.wasm"),
    {ok, Msg3} =
        hb_converge:resolve(
            Msg,
            generate_aos_msg("return 1+1"),
            #{}
        ),
    ?event({msg3, hb_converge:get(<<"Results">>, Msg3, #{})}),
    ?assertEqual(<<"2">>, hb_converge:get(<<"Results/Data">>, Msg3, #{})).