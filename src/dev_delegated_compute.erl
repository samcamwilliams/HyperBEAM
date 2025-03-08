%%% @doc Simple wrapper module that enables compute on remote machines,
%%% implementing the JSON-Iface. This can be used either as a standalone, to 
%%% bring trusted results into the local node, or as the `Execution-Device' of
%%% an AO process.
-module(dev_delegated_compute).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Initialize or normalize the compute-lite device. For now, we don't
%% need to do anything special here.
init(Msg1, _Msg2, _Opts) ->
    {ok, Msg1}.
normalize(Msg1, _Msg2, _Opts) -> {ok, Msg1}.
snapshot(Msg1, _Msg2, _Opts) -> {ok, Msg1}.

compute(Msg1, Msg2, Opts) ->
    RawProcessID = dev_process:process_id(Msg1, #{}, Opts),
    Slot = hb_converge:get(<<"slot">>, Msg2, Opts),
    ?event(push, {compute_lite_called, {process_id, RawProcessID}, {slot, Slot}}),
    OutputPrefix = dev_stack:prefix(Msg1, Msg2, Opts),
    ProcessID =
        case RawProcessID of
            not_found ->
                hb_converge:get(<<"process-id">>, Msg2, Opts);
            ProcID ->
                ProcID
        end,
    {ok, JSONRes} = do_compute(ProcessID, Slot, Opts),
    ?event(push, {compute_lite_res, {process_id, ProcessID}, {slot, Slot}, {json_res, JSONRes}}),
    {ok, Msg} = dev_json_iface:json_to_message(JSONRes, Opts),
    {ok,
        hb_converge:set(
            Msg1,
            #{
                <<OutputPrefix/binary, "/results">> => Msg,
                <<OutputPrefix/binary, "/results/json">> =>
                    #{
                        <<"content-type">> => <<"application/json">>,
                        <<"body">> => JSONRes
                    }
            },
            Opts
        )
    }.

%% @doc Execute computation on a remote machine via relay and the JSON-Iface.
do_compute(ProcID, Slot, Opts) ->
    ?event({do_compute_called, {process_id, ProcID}, {slot, Slot}}),
    Res = 
        hb_converge:resolve(#{ <<"device">> => <<"relay@1.0">> }, #{
            <<"path">> => <<"call">>,
            <<"relay-path">> =>
                <<
                    "/result/",
                    (integer_to_binary(Slot))/binary,
                    "?process-id=",
                    ProcID/binary
                >>
            },
            Opts
        ),
    ?event({res, Res}),
    {ok, Response} = Res,
    JSONRes = hb_converge:get(<<"body">>, Response, Opts),
    ?event({json_res, JSONRes}),
    {ok, JSONRes}.