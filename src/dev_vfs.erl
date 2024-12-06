-module(dev_vfs).
-export([init/1, execute/2, stdout/1]).
-include("include/hb.hrl").

-record(fd, {
    index :: non_neg_integer(),
    filename :: binary(),
    data :: binary(),
    offset :: non_neg_integer()
}).

-define(INIT_VFS,
    #{
        0 => #fd{
            index = 0,
            filename = <<"/dev/stdin">>,
            data = <<>>,
            offset = 0
        },
        1 => #fd{
            index = 1,
            filename = <<"/dev/stdout">>,
            data = <<>>,
            offset = 0
        },
        2 => #fd{
            index = 2,
            filename = <<"/dev/stderr">>,
            data = <<>>,
            offset = 0
        }
    }
).

init(S) ->
    Lib = maps:merge(maps:get(library, S, #{}), #{
        {"wasi_snapshot_preview1", "path_open"} => fun path_open/3,
        {"wasi_snapshot_preview1", "fd_write"} => fun fd_write/3,
        {"wasi_snapshot_preview1", "fd_read"} => fun fd_read/3
    }),
    {ok, S#{library => Lib, vfs => ?INIT_VFS}}.

execute(M, S = #{ pass := 1, vfs := FDs }) ->
    #tx { data = Data } = maps:get(<<"Message">>, M#tx.data),
    ?event({setting_stdin_to_message, byte_size(Data)}),
    {ok,
        S#{
            vfs =>
                maps:put(
                    0,
                    (maps:get(0, FDs))#fd{ data = Data },
                    FDs
                )
        }
    };
execute(_M, S = #{ pass := 2, vfs := FDs }) ->
    {ok, S#{
        results =>
            maps:merge(
                maps:get(results, S, #{}),
                maps:from_list(
                    [
                        {Filename, ar_bundles:normalize(#tx{data = Data})}
                    ||
                        #fd{
                            filename = Filename,
                            data = Data
                        } <- maps:values(FDs)
                    ]
                )
            )
        }
    };
execute(_M, S) ->
    {ok, S}.

%% @doc Return the stdout buffer from a state message.
stdout(#{ vfs := #{ 1 := #fd { data = Data } } }) ->
    Data.

path_open(S = #{ vfs := FDs }, Port, [FDPtr, LookupFlag, PathPtr|_]) ->
    ?event({path_open, FDPtr, LookupFlag, PathPtr}),
    Path = hb_beamr_io:read_string(Port, PathPtr),
    ?event({path_open, Path}),
    File =
        maps:get(
            hd(maps:keys(
                maps:filter(
                    fun(_, #fd{filename = FN}) -> FN =/= Path end,
                    FDs
                )
            )),
            FDs,
            #fd {
                index = maps:size(FDs) + 1,
                filename = Path,
                data = <<>>,
                offset = 0
            }
        ),
    {S#{vfs => maps:put(File#fd.index, File, FDs)}, [0, File#fd.index]}.

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