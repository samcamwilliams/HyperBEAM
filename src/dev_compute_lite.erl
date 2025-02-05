%%% @doc Simple wrapper module that enables compute on remote machines,
%%% implementing the JSON-Iface. This can be used either as a standalone, to 
%%% bring trusted results into the local node, or as the `Execution-Device` of
%%% an AO process.
-module(dev_compute_lite).
-export([compute/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

compute(Msg1, Msg2, Opts) ->
    OutputPrefix = dev_stack:prefix(Msg1, Msg2, Opts),
    MessageToCompute = hb_converge:get(<<"body">>, Msg2, Opts),
    MessageID = hb_message:id(MessageToCompute, all),
    ProcessID =
        hb_converge:get_first(
            [
                {Msg1, <<"process/id">>},
                {Msg2, <<"process-id">>}
            ],
            Opts
        ),
    {ok, Res} = do_compute(ProcessID, MessageID, Opts),
    hb_converge:set(Msg1, <<OutputPrefix/binary, "/results">>, Res, Opts).

%% @doc Execute computation on a remote machine via relay and the JSON-Iface.
do_compute(ProcID, MsgID, Opts) ->
    Res = 
        hb_converge:resolve(#{ <<"device">> => <<"relay@1.0">> }, #{
            <<"path">> => <<"call">>,
            <<"relay-path">> =>
                <<
                    "/result/",
                    MsgID/binary,
                    "?process-id=",
                    ProcID/binary
                >>
            },
            Opts
        ),
    ?event(debug, {res, Res}),
    {ok, Response} = Res,
    JSONRes = hb_converge:get(<<"body">>, Response, Opts),
    ?event(debug, {json_res, JSONRes}),
    dev_json_iface:json_to_message(JSONRes, Opts).

%%% Tests

compute_test() ->
    case hb_opts:get(run_remote_tests, true) of
        false ->
            {skip, "Remote dependent test"};
        true ->
            do_compute(
                <<"QIFgbqEmk5MyJy01wuINfcRP_erGNNbhqHRkAQjxKgg">>,
                <<"N6D1IaUCE50R-CiwILa_vxPy32rgwDaU-FkoP3VW3S8">>,
                #{
                    routes => [
                        #{
                            <<"template">> => <<"/result/.*">>,
                            <<"node">> =>
                                #{
                                    <<"prefix">> =>
                                        <<"https://cu5767.ao-testnet.xyz">>
                                }
                        }
                    ]
                }
            )
    end.
