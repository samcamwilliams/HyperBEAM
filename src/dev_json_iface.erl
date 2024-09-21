-module(dev_json_iface).
-export([init/2, execute/2, uses/0, stdlib/6]).

-record(state, {
    stdout = []
}).

uses() -> all.

init(Port, S) ->
    {ok, S#{ stdlib := fun stdlib/6, json_iface := #state{} }}.

execute(M, S#{ pass := 1, json_iface := IfaceS }) ->
    {ok, S#{ call := prep_call(M, S), json_iface := IfaceS#state{ stdout := [] } }};
execute(M, S#{ pass := 2, json_iface := IfaceS }) ->
    {ok, result(S) }.

prep_call(M, S#{ wasm = Port }) ->
    MsgJson = jiffy:encode(M),
    MsgJsonSize = byte_size(MsgJson),
    {ok, MsgJsonPtr} = cu_beamr:malloc(Port, MsgJsonSize),
    cu_beamr:write(Port, MsgJsonPtr, MsgJson),
    EnvJson = jiffy:encode({[{<<"Process">>, ar_bundle:tx_to_json_struct(S#{ process })}]}),
    {ok, EnvJsonPtr} = cu_beamr:malloc(Port, byte_size(EnvJson)),
    cu_beamr:write(Port, EnvJsonPtr, EnvJson),
    {call, "json_parse", [MsgJsonPtr, EnvJsonPtr]}.

result(S#{ result := Res, json_iface := IfaceS }) ->
    ao:c({stdout, iolist_to_binary(IfaceS#state.stdout)}),
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

stdlib(Port, ModName, FuncName, Args, Sig, S#{ json_iface := IfaceS }) ->
    {Res, IfaceS2} = lib(Port, ModName, FuncName, Args, Sig, IfaceS),
    {Res, S#{ json_iface := IfaceS2 }}.

lib(Port, "wasi_snapshot_preview1","fd_write", [Fd, Ptr, Vecs, RetPtr], _Signature, S) ->
    ao:c({fwd_write, Fd, Ptr, Vecs, RetPtr}),
    {ok, VecData} = read_iovecs(Port, Ptr, Vecs),
    BytesWritten = byte_size(VecData),
    error_logger:info_report(VecData),
    NewStdio =
        case S#{ stdout } of
            undefined ->
                [VecData];
            Existing ->
                Existing ++ [VecData]
        end,
    % Set return pointer to number of bytes written
    write(Port, RetPtr, <<BytesWritten:64/little-unsigned-integer>>),
    {[0], S#{ stdout := NewStdio }};
lib(_Port, _Module, "clock_time_get", _Args, _Signature, S) ->
    ao:c({stub_called, wasi_clock_time_get, 1}),
    {[1], S};
lib(_Port, _Module, _Func, _Args, _Signature, S) ->
    {[0], S}.

read_string(Port, Offset) ->
    {ok, iolist_to_binary(do_read_string(Port, Offset, 8))}.

do_read_string(Port, Offset, ChunkSize) ->
    {ok, Data} = cu_beamr:read(Port, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(Port, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.