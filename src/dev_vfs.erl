-module(dev_vfs).
-export([init/1, execute/3, stdout/1]).
-include("include/hb.hrl").

-record(fd, {
    index :: non_neg_integer(),
    filename :: binary(),
    data :: binary(),
    offset :: non_neg_integer()
}).

-define(INIT_VFS,
    #{
        <<"dev">> => #{
            <<"stdin">> => #{
                index => 0,
                data => <<>>,
                offset => 0
            },
            <<"stdout">> => #{
                index => 1,
                data => <<>>,
                offset => 0
            },
            <<"stderr">> => #{
                index => 2,
                data => <<>>,
                offset => 0
            }
        }
    }
).

init(M1, _M2, Opts) ->
    Lib =
        hb_converge:set(
            hb_converge:get(<<"priv_lib">>, M1, Opts),
            #{
                {"wasi_snapshot_preview1", "path_open"} =>
                    fun path_open/3,
                {"wasi_snapshot_preview1", "fd_write"} =>
                    fun fd_write/3,
                {"wasi_snapshot_preview1", "fd_read"} =>
                    fun fd_read/3
            },
            Opts
        ),
    {ok,
        hb_converge:set(
            M1,
            <<"VFS">>,
            ?INIT_VFS,
            Opts
        )
    }.

execute(M1 = #{ pass := 1 }, M2, Opts) ->
    MsgToProc = hb_converge:get(<<"Message">>, M2, Opts),
    JSON =
        ar_bundles:serialize(
            hb_message:message_to_tx(MsgToProc),
            json
        ),
    ?event(setting_stdin_to_message),
    {ok,
        hb_converge:set(
            M1,
            <<"vfs/dev/stdin/data">>,
            JSON,
            Opts
        )
    };
execute(M1 = #{ pass := 2, vfs := FDs }, _M2, Opts) ->
    {ok, 
        hb_converge:set(
            M1,
            <<"Results/vfs">>,
            maps:from_list(
                [
                    {Filename, Data}
                ||
                    #{
                        filename := Filename,
                        data := Data
                    } <- maps:values(FDs)
                ]
            ),
            Opts
        )
    };
execute(M1, _M2, _Opts) ->
    {ok, M1}.

%% @doc Return the stdout buffer from a state message.
stdout(M) ->
    hb_converge:get(<<"vfs/1/data">>, M).

path_open(M, Port, [FDPtr, LookupFlag, PathPtr|_]) ->
    FDs = hb_converge:get(<<"File-Descriptors">>, M),
    ?event({path_open, FDPtr, LookupFlag, PathPtr}),
    Path = hb_beamr_io:read_string(Port, PathPtr),
    ?event({path_open, Path}),
    File =
        case hb_converge:get(<<"vfs/", Path/binary>>, M) of
            not_found ->
                #{
                    index => maps:size(FDs) + 1,
                    filename => Path,
                    data => <<>>,
                    offset => 0
                };
            File ->
                File
        end,
    {
        hb_converge:set(
            M,
            <<"vfs/", Path/binary>>,
            #{
                index => File#fd.index,
                data => File#fd.data,
                offset => File#fd.offset
            },
            Opts
        ),
        [0, hb_converge:get(<<"Index">>, File)]
    }.

fd_write(S, Port, [FD, Ptr, Vecs, RetPtr]) ->
    fd_write(S, Port, [FD, Ptr, Vecs, RetPtr], 0);
fd_write(S, Port, Args) ->
    ?event({fd_write, Port, Args}),
    {S, [0]}.
fd_write(S, Port, [_, _Ptr, 0, RetPtr], BytesWritten) ->
    hb_beamr_io:write(
        Port,
        RetPtr,
        <<BytesWritten:64/little-unsigned-integer>>
    ),
    {S, [0]};
fd_write(S = #{ vfs := FDs }, Port, [FD, Ptr, Vecs, RetPtr], BytesWritten) ->
    File = maps:get(FD, FDs),
    {VecPtr, Len} = parse_iovec(Port, Ptr),
    {ok, Data} = hb_beamr_io:read(Port, VecPtr, Len),
    Before = binary:part(File#fd.data, 0, File#fd.offset),
    After = binary:part(File#fd.data, File#fd.offset, byte_size(File#fd.data) - File#fd.offset),
    NewFile = File#fd{
        data = <<Before/binary, Data/binary, After/binary>>,
        offset = File#fd.offset + byte_size(Data)
    },
    fd_write(
        S#{vfs => maps:put(FD, NewFile, FDs)},
        Port,
        [FD, Ptr + 16, Vecs - 1, RetPtr],
        BytesWritten + byte_size(Data)
    ).

fd_read(S, Port, Args) ->
    fd_read(S, Port, Args, 0).
fd_read(S, Port, [FD, _VecsPtr, 0, RetPtr], BytesRead) ->
    ?event({{completed_read, FD, BytesRead}}),
    hb_beamr_io:write(Port, RetPtr, <<BytesRead:64/little-unsigned-integer>>),
    {S, [0]};
fd_read(S = #{ vfs := FDs }, Port, [FD, VecsPtr, NumVecs, RetPtr], BytesRead) ->
    ?event({fd_read, FD, VecsPtr, NumVecs, RetPtr}),
    File = maps:get(FD, FDs),
    {VecPtr, Len} = parse_iovec(Port, VecsPtr),
    {FileBytes, NewFile} = get_bytes(File, Len),
    ok = hb_beamr_io:write(Port, VecPtr, FileBytes),
    fd_read(
        S#{vfs => maps:put(FD, NewFile, FDs)},
        Port,
        [FD, VecsPtr + 16, NumVecs - 1, RetPtr],
        BytesRead + byte_size(FileBytes)
    ).

get_bytes(#fd { data = Data, offset = Offset }, Size) when is_binary(Data) ->
    AvailableSize = min(Size, byte_size(Data) - Offset),
    Bin = binary:part(Data, Offset, AvailableSize),
    {Bin, #fd { data = Data, offset = Offset + byte_size(Bin) }};
get_bytes(File = #fd { data = Function }, Size) ->
    {Bin, NewFile} = Function(File, Size),
    {Bin, NewFile}.

parse_iovec(Port, Ptr) ->
    {ok, VecStruct} = hb_beamr_io:read(Port, Ptr, 16),
    <<BinPtr:64/little-unsigned-integer, Len:64/little-unsigned-integer>> = VecStruct,
    {BinPtr, Len}.