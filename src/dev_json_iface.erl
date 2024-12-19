%%% @doc A device that provides a way for WASM execution to interact with
%%% the HyperBEAM (and AO) systems, using JSON as a shared data representation.
%%% 
%%% The interface is extremely basic. It works as follows:
%%% 
%%% 1. The device is given a message that contains a process definition, WASM
%%%    environment, and a message that contains the data to be processed,
%%%    including the image to be used in part of `execute{pass=1}'.
%%% 2. The device is called with `execute{pass=2}', which reads the result of
%%%    the process execution from the WASM environment and returns it as a
%%%    message.
%%%
%%% The device has the following requirements and interface:
%%%     ```
%%%     M1/Computed when M2/Pass == 1 ->
%%%         Assumes:
%%%             M1/priv/WASM/Port
%%%             M1/Process
%%%             M2/Message
%%%             M2/Message/Image
%%%             M2/Image
%%%             M2/Block-Height
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
-export([computed/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc On first pass prepare the call, on second pass get the results.
computed(M1, M2, Opts) ->
    case hb_converge:get(pass, M1, M2) of
        1 -> prep_call(M1, M2, Opts);
        2 -> results(M1, M2, Opts);
        _ -> {ok, M1}
    end.

%% @doc Prepare the WASM environment for execution by writing the process string and
%% the message as JSON representations into the WASM environment.
prep_call(M1, M2, Opts) ->
    Port = hb_converge:get(<<"priv/WASM/Port">>, M1, Opts),
    Process = hb_converge:get(<<"Process">>, M1, Opts),
    Assignment = hb_converge:get(<<"Assignment">>, M2, Opts),
    Message = hb_converge:get(<<"Message">>, M2, Opts),
    Image = hb_converge:get(<<"Image">>, Message, Opts),
    BlockHeight = hb_converge:get(<<"Block-Height">>, Assignment, Opts),
    RawMsgJson =
        ar_bundles:item_to_json_struct(hb_message:message_to_tx(Message)),
    {Props} = jiffy:decode(RawMsgJson),
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
                <<"WASM/Handler">> => <<"handle">>,
                <<"WASM/Params">> => [MsgJsonPtr, ProcessJsonPtr]
            },
            Opts
        )
    }.

%% @doc Read the computed results out of the WASM environment, assuming that
%% the environment has been set up by `prep_call/3' and that the WASM executor
%% has been called with `computed{pass=1}'.
results(M1, M2, Opts) ->
    Port = hb_converge:get(<<"priv/WASM/Port">>, M1, Opts),
    Type = hb_converge:get(<<"Results/WASM/Type">>, M2, Opts),
    Proc = hb_converge:get(<<"Process">>, M2, Opts),
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
            [Ptr] = hb_converge:get(<<"WASM/Results/Body">>, M1, Opts),
            {ok, Str} = hb_beamr_io:read_string(Port, Ptr),
            Wallet = hb_opts:get(wallet, no_viable_wallet, Opts),
            try jiffy:decode(Str, [return_maps]) of
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    % TODO: Handle all JSON interface output types.
                    #{
                        <<"Output">> := #{<<"data">> := Data},
                        <<"Messages">> := Messages
                    } = Resp,
                    hb_converge:set(
                        M1,
                        #{
                            <<"Results/Outbox">> =>
                                [
                                    {
                                        list_to_binary(integer_to_list(MessageNum)),
                                        ar_bundles:sign_item(
                                            ar_bundles:json_struct_to_item(
                                                preprocess_results(Msg, Proc, Opts)
                                            ),
                                            Wallet
                                        )
                                    }
                                ||
                                    {MessageNum, Msg} <-
                                        lists:zip(lists:seq(1, length(Messages)), Messages)
                                ],
                            <<"Results/Data">> => Data
                        },
                        Opts
                    )
            catch
                _:_ ->
                    hb_converge:set(
                        M1,
                        #{
                            <<"Results/Outbox">> => undefined,
                            <<"Results/Body">> => <<"JSON error parsing WASM result output.">>
                        },
                        Opts
                    )
            end
    end.

%% NOTE: After the process returns messages from an evaluation, the signing unit needs to add
%% some tags to each message, and spawn to help the target process know these messages are created
%% by a process.
preprocess_results(Msg, Proc, Opts) ->
    {ok, FilteredMsg} =
        hb_converge:resolve(Msg,
            #{
                path => remove,
                items => [<<"From-Process">>, <<"From-Image">>, <<"Anchor">>]
            },
            Opts
        ),
    hb_converge:set(
        FilteredMsg,
        #{
            <<"From-Process">> => hb_util:id(Proc, signed),
            <<"From-Image">> => element(2, lists:keyfind(<<"Image">>, 1, Proc#tx.tags))
        }
    ).
