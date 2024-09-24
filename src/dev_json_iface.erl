-module(dev_json_iface).
-export([init/2, execute/2, uses/0, stdlib/6]).

-include("include/ao.hrl").

uses() -> all.

init(_Port, S) ->
    {ok, S#{ stdlib := fun stdlib/6, json_iface := #{} }}.

execute(M, S = #{ pass := 1, json_iface := IfaceS }) ->
    {ok, S#{
        call := prep_call(M, S),
        json_iface := IfaceS#{ stdout => [] } }
    };
execute(_M, S = #{ pass := 2 }) ->
    {ok, result(S) }.

prep_call(M, #{ wasm := Port, process := Process }) ->
    MsgJson = jiffy:encode(M),
    MsgJsonSize = byte_size(MsgJson),
    {ok, MsgJsonPtr} = cu_beamr:malloc(Port, MsgJsonSize),
    cu_beamr:write(Port, MsgJsonPtr, MsgJson),
    EnvJson = jiffy:encode({[{<<"Process">>, ar_bundle:tx_to_json_struct(Process)}]}),
    {ok, EnvJsonPtr} = cu_beamr:malloc(Port, byte_size(EnvJson)),
    cu_beamr:write(Port, EnvJsonPtr, EnvJson),
    {call, "json_parse", [MsgJsonPtr, EnvJsonPtr]}.

result(S = #{ wasm := Port, result := Res, json_iface := #{ stdout := Stdout } }) ->
    ao:c({stdout, iolist_to_binary(Stdout)}),
    case Res of
        {error, Res} ->
            S#{
                outbox := [],
                result := #tx { tags = [{<<"Result">>, <<"Error">>}], data = Res }
            };
        {ok, [Ptr]} ->
            case jiffy:decode(read_string(Port, Ptr), [return_maps]) of
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    #{<<"Output">> := #{ <<"data">> := Data }} = Resp,
                    #{<<"Output">> := #{ <<"outbox">> := Msgs }} = Resp,
                    S#{
                        outbox := [ ar_bundle:json_struct_to_tx(Msg) || Msg <- Msgs ],
                        result := #tx { tags = [{<<"Result">>, <<"OK">>}], data = Data }
                    };
                #{<<"ok">> := false} ->
                    S#{
                        outbox := [],
                        result := #tx { tags = [{<<"Result">>, <<"Error">>}], data = <<"JSON Parse Error">> }
                    }
            end
    end.

stdlib(Port, ModName, FuncName, Args, Sig, S = #{ json_iface := IfaceS }) ->
    {Res, IfaceS2} = lib(IfaceS, Port, ModName, FuncName, Args, Sig),
    {Res, S#{ json_iface := IfaceS2 }}.

lib(S = #{ stdout := Stdout }, Port, "wasi_snapshot_preview1","fd_write", [Fd, Ptr, Vecs, RetPtr], _Signature) ->
    ao:c({fwd_write, Fd, Ptr, Vecs, RetPtr}),
    {ok, VecData} = cu_beamr:read_iovecs(Port, Ptr, Vecs),
    BytesWritten = byte_size(VecData),
    error_logger:info_report(VecData),
    NewStdio =
        case Stdout of
            undefined ->
                [VecData];
            Existing ->
                Existing ++ [VecData]
        end,
    % Set return pointer to number of bytes written
    cu_beamr:write(Port, RetPtr, <<BytesWritten:64/little-unsigned-integer>>),
    {S#{ stdout := NewStdio }, [0]};
lib(_Port, _Module, "clock_time_get", _Args, _Signature, S) ->
    ao:c({stub_called, wasi_clock_time_get, 1}),
    {S, [1]};
lib(_Port, _Module, _Func, _Args, _Signature, S) ->
    {S, [0]}.

read_string(Port, Offset) ->
    {ok, iolist_to_binary(do_read_string(Port, Offset, 8))}.

do_read_string(Port, Offset, ChunkSize) ->
    {ok, Data} = cu_beamr:read(Port, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(Port, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.