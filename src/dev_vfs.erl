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

fd_write(S = #{ vfs := FDs }, Port, [FD, Ptr, Vecs, RetPtr]) ->
    File = maps:get(FD, FDs),
    {ok, Data} = cu_beamr_io:read_iovecs(Port, Ptr, Vecs),
    BytesWritten = byte_size(Data),
    cu_beamr_io:write(
        Port,
        RetPtr,
        <<BytesWritten:64/little-unsigned-integer>>
    ),
    ?c({fd_write, File#fd.filename, BytesWritten, Ptr}),
    NewFile = File#fd{
        data = <<(File#fd.data)/binary, Data/binary>>,
        offset = File#fd.offset + byte_size(Data)
    },
    {
        S#{
            vfs => maps:put(NewFile#fd.index, NewFile, FDs)
        },
        [0]
    }.

fd_read(S = #{ vfs := FDs }, Port, [FD, Ptr, Vecs]) ->
    File = maps:get(FD, FDs),
    {ok, Data} =
        cu_beamr_io:read_iovecs(
            Port,
            Ptr,
            Vecs,
            binary:part(
                File#fd.data,
                File#fd.offset,
                byte_size(File#fd.data) - File#fd.offset
            )
        ),
    ?c({fd_read, File#fd.filename, byte_size(Data), Ptr}),
    {S, [0]}.
