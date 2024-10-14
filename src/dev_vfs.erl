-module(dev_vfs).
-export([init/2, execute/2]).
-include("include/ao.hrl").

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

init(S, Params) ->
    ?c({vfs_init, Params}),
    Lib = maps:merge(maps:get(library, S, #{}), #{
        {wasi_snapshot_preview1, "path_open"} => fun path_open/3,
        {wasi_snapshot_preview1, "fd_write"} => fun fd_write/3,
        {wasi_snapshot_preview1, "fd_read"} => fun fd_read/3
    }),
    {ok, S#{library => Lib, vfs => ?INIT_VFS}}.

execute(_M, S = #{ pass := 2, vfs := FDs }) ->
    ?c({adding_vfs_to_result, S}),
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

path_open(S = #{ vfs := FDs }, Port, [FDPtr, LookupFlag, PathPtr|_]) ->
    ?c({path_open, FDPtr, LookupFlag, PathPtr}),
    Path = cu_beamr_io:read_string(Port, PathPtr),
    ?c({path_open, Path}),
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
    fd_write(S, Port, [FD, Ptr, Vecs, RetPtr], 0).
fd_write(S, Port, [_, _Ptr, 0, RetPtr], BytesWritten) ->
    cu_beamr_io:write(
        Port,
        RetPtr,
        <<BytesWritten:64/little-unsigned-integer>>
    ),
    {S, [0]};
fd_write(S = #{ vfs := FDs }, Port, [FD, Ptr, Vecs, RetPtr], BytesWritten) ->
    File = maps:get(FD, FDs),
    ?c({fd_write, FD, Ptr, Vecs, BytesWritten}),
    {Ptr, Len} = parse_iovec(Port, Ptr),
    {ok, Data} = cu_beamr_io:read(Port, Ptr, Len),
    Before = binary:part(File#fd.data, 0, File#fd.offset),
    After = binary:part(File#fd.data, File#fd.offset, byte_size(File#fd.data) - File#fd.offset),
    NewFile = File#fd{
        data = <<Before/binary, Data/binary, After/binary>>,
        offset = File#fd.offset + byte_size(Data)
    },
    ?c({fd_write, File#fd.filename, BytesWritten, Ptr}),
    fd_write(
        S#{vfs => maps:put(FD, NewFile, FDs)},
        Port,
        [FD, Ptr + 16, Vecs - 1, RetPtr],
        BytesWritten + byte_size(Data)
    ).

fd_read(S, Port, Args) ->
    fd_read(S, Port, Args, 0).
fd_read(S, _Port, [FD, _Ptr, 0], BytesRead) ->
    ?c({{completed_read, FD, BytesRead}}),
    {S, [0]};
fd_read(S = #{ vfs := FDs }, Port, [FD, Ptr, Vecs], BytesRead) ->
    ?c({fd_read, FD, Ptr, Vecs}),
    File = maps:get(FD, FDs),
    {VecPtr, Len} = parse_iovec(Port, Ptr),
    {FileBytes, NewFile} = get_bytes(File, Len),
    ok = cu_beamr_io:write(Port, VecPtr, FileBytes),
    fd_read(
        S#{vfs => maps:put(FD, NewFile, FDs)},
        Port,
        [FD, VecPtr + 16, Vecs - 1],
        BytesRead + byte_size(FileBytes)
    ).

get_bytes(#fd { data = Data, offset = Offset }, Size) when is_binary(Data) ->
    Bin = binary:part(Data, Offset, Size),
    {Bin, #fd { data = Data, offset = Offset + byte_size(Bin) }};
get_bytes(File = #fd { data = Function }, Size) ->
    {Bin, NewFile} = Function(File, Size),
    {Bin, NewFile}.

parse_iovec(Port, Ptr) ->
    {ok, VecStruct} = cu_beamr_io:read(Port, Ptr, 16),
    <<BinPtr:64/little-unsigned-integer, Len:64/little-unsigned-integer>> = VecStruct,
    {BinPtr, Len}.

write_file_test() ->
    {Proc, Msg} = cu_process:generate_test_data(<<"print(file:read(5))">>),
    cu_process_test:run(Proc, Msg).
