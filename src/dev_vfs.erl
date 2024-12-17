-module(dev_vfs).
-export([init/3, execute/3, stdout/1]).
-include("include/hb.hrl").

%%% @moduledoc A virtual filesystem device.
%%% Implements a file-system-as-map structure, which is traversible externally.
%%% Each file is a binary, and each directory is a Converge message.
%%% Additionally, this module adds a series of WASI-preview-1 combatible 
%%% functions for accessing the filesystem.

-define(INIT_VFS,
    #{
        <<"dev">> => #{
            <<"stdin">> => #{
                data => <<>>,
                offset => 0
            },
            <<"stdout">> => #{
                data => <<>>,
                offset => 0
            },
            <<"stderr">> => #{
                data => <<>>,
                offset => 0
            }
        }
    }
).

-define(INIT_FDS,
    #{
        0 => #{
            <<"Filename">> => <<"/dev/stdin">>,
            <<"offset">> => 0
        },
        1 => #{
            <<"Filename">> => <<"/dev/stdout">>,
            <<"offset">> => 0
        },
        2 => #{
            <<"Filename">> => <<"/dev/stderr">>,
            <<"offset">> => 0
        }
    }
).

%% @doc On-boot, initialize the virtual file system with:
%% - Empty stdio files
%% - WASI-preview-1 compatible functions for accessing the filesystem
%% - File descriptors for those files.
init(M1, _M2, Opts) ->
    MsgWithLib =
        hb_converge:set(
            M1,
            #{
                <<"priv/WASM/stdlib/wasi_snapshot_preview1/path_open">> =>
                    fun path_open/3,
                <<"priv/WASM/stdlib/wasi_snapshot_preview1/fd_write">> =>
                    fun fd_write/3,
                <<"priv/WASM/stdlib/wasi_snapshot_preview1/fd_read">> =>
                    fun fd_read/3
            },
            Opts
        ),
    MsgWithFDs =
        hb_converge:set(
            MsgWithLib,
            <<"File-Descriptors">>,
            ?INIT_FDS,
            Opts
        ),
    CompleteMsg =
        hb_converge:set(
            MsgWithFDs,
            <<"VFS">>,
            ?INIT_VFS,
            Opts
        ),
    {ok, CompleteMsg}.

%% @doc Encode the input message for inclusion in the VFS.
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
            <<"vfs">>,
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

%% @doc Adds a file descriptor to the state message.
path_open(M, Port, [FDPtr, LookupFlag, PathPtr|_]) ->
    FDs = hb_converge:get(<<"File-Descriptors">>, M),
    ?event({path_open, FDPtr, LookupFlag, PathPtr}),
    Path = hb_beamr_io:read_string(Port, PathPtr),
    ?event({path_open, Path}),
    #{
        index := Index,
        data := Data,
        offset := Offset
    } =
        case hb_converge:get(<<"vfs/", Path/binary>>, M) of
            not_found ->
                #{
                    index => maps:size(FDs) + 1,
                    filename => Path,
                    data => <<>>,
                    offset => 0
                };
            F -> F
        end,
    {
        hb_converge:set(
            M,
            <<"vfs/", Path/binary>>,
            #{
                index => Index,
                data => Data,
                offset => Offset
            }
        ),
        [0, Index]
    }.

%% @doc WASM stdlib implementation of `fd_write`, using the WASI-p1 standard
%% interface.
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
fd_write(S, Port, [FD, Ptr, Vecs, RetPtr], BytesWritten) ->
    FD = hb_converge:get(<<"File-Descriptors/", FD/binary>>, S),
    Filename = hb_converge:get(<<"filename">>, FD),
    {VecPtr, Len} = parse_iovec(Port, Ptr),
    {ok, Data} = hb_beamr_io:read(Port, VecPtr, Len),
    Before =
        binary:part(
            OrigData = hb_converge:get(<<"data">>, FD),
            0,
            hb_converge:get(<<"offset">>, FD)
        ),
    After =
        binary:part(
            OrigData,
            hb_converge:get(<<"offset">>, FD),
            byte_size(OrigData) - hb_converge:get(<<"offset">>, FD)
        ),
    NewFile =
        hb_converge:set(
            FD,
            #{
                <<"data">> => <<Before/binary, Data/binary, After/binary>>,
                <<"offset">> =>
                    hb_converge:get(<<"offset">>, FD) + byte_size(Data)
            }
        ),
    fd_write(
        hb_converge:set(S, <<"vfs/", Filename/binary>>, NewFile),
        Port,
        [FD, Ptr + 16, Vecs - 1, RetPtr],
        BytesWritten + byte_size(Data)
    ).

%% @doc Read from a file using the WASI-p1 standard interface.
fd_read(S, Port, Args) ->
    fd_read(S, Port, Args, 0).
fd_read(S, Port, [FD, _VecsPtr, 0, RetPtr], BytesRead) ->
    ?event({{completed_read, FD, BytesRead}}),
    hb_beamr_io:write(Port, RetPtr,
        <<BytesRead:64/little-unsigned-integer>>),
    {S, [0]};
fd_read(S, Port, [FD, VecsPtr, NumVecs, RetPtr], BytesRead) ->
    ?event({fd_read, FD, VecsPtr, NumVecs, RetPtr}),
    FD = hb_converge:get(<<"File-Descriptors/", FD/binary>>, S),
    Filename = hb_converge:get(<<"filename">>, FD),
    {VecPtr, Len} = parse_iovec(Port, VecsPtr),
    {FileBytes, NewFile} = get_bytes(FD, Len),
    ok = hb_beamr_io:write(Port, VecPtr, FileBytes),
    fd_read(
        hb_converge:set(S, <<"vfs/", Filename/binary>>, NewFile),
        Port,
        [FD, VecsPtr + 16, NumVecs - 1, RetPtr],
        BytesRead + byte_size(FileBytes)
    ).

%% @doc Find a file in the VFS from its name or file descriptor.
get_file(M, Filename) when is_binary(Filename) ->
    hb_converge:get(<<"vfs/", Filename/binary>>, M);
get_file(M, FD) when is_integer(FD) ->
    hb_converge:get(<<"File-Descriptors/", FD/binary>>, M).

%% @doc Fetch bytes from the file, and update the file message with the new
%% offset.
get_bytes(FileMsg, Size) ->
    Data = hb_converge:get(<<"data">>, FileMsg),
    Offset = hb_converge:get(<<"offset">>, FileMsg),
    AvailableSize = min(Size, byte_size(Data) - Offset),
    Bin = binary:part(Data, Offset, AvailableSize),
    {Bin, hb_converge:set(FileMsg, <<"offset">>, Offset + byte_size(Bin))}.


%% @doc Parse an iovec in WASI-preview-1 format.
parse_iovec(Port, Ptr) ->
    {ok, VecStruct} = hb_beamr_io:read(Port, Ptr, 16),
    <<
        BinPtr:64/little-unsigned-integer,
        Len:64/little-unsigned-integer
    >> = VecStruct,
    {BinPtr, Len}.
