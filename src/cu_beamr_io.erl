-module(cu_beamr_io).
-export([read/3, write/3]).
-export([read_string/2, read_iovecs/3, write_string/2]).
-export([malloc/2, free/2]).

write(Port, Offset, Data) ->
    Port ! {self(), {command, term_to_binary({write, Offset, Data})}},
    receive
        ok ->
            ok;
        Error ->
            Error
    end.

write_string(Port, Data) when is_list(Data) ->
    write_string(Port, iolist_to_binary(Data));
write_string(Port, Data) ->
    DataSize = byte_size(Data) + 1,
    String = <<Data/bitstring, 0:8>>,
    case malloc(Port, DataSize) of
        {ok, Ptr} ->
            case write(Port, Ptr, String) of
                ok ->
                    {ok, Ptr};
                Error ->
                    free(Port, Ptr),
                    Error
            end;
        Error ->
            Error
    end.

read(Port, Offset, Size) ->
    Port ! {self(), {command, term_to_binary({read, Offset, Size})}},
    receive
        {ok, Result} ->
            {ok, Result};
        Error ->
            Error
    end.

read_string(Port, Offset) ->
    {ok, iolist_to_binary(do_read_string(Port, Offset, 8))}.

do_read_string(Port, Offset, ChunkSize) ->
    {ok, Data} = read(Port, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(Port, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.

read_iovecs(Port, Ptr, Vecs) ->
    ao:c({read_iovecs_started, Port, Ptr, Vecs}),
    Bin = iolist_to_binary(do_read_iovecs(Port, Ptr, Vecs)),
    {ok, Bin}.

do_read_iovecs(_Port, _Ptr, 0) -> [];
do_read_iovecs(Port, Ptr, Vecs) ->
    {ok, VecStruct} = read(Port, Ptr, 16),
    <<BinPtr:64/little-unsigned-integer, Len:64/little-unsigned-integer>> = VecStruct,
    {ok, VecData} = read(Port, BinPtr, Len),
    [ VecData | do_read_iovecs(Port, Ptr + 16, Vecs - 1) ].

malloc(Port, Size) ->
    case cu_beamr:call(Port, "malloc", [Size]) of
        {ok, [0]} ->
            ao:c({malloc_failed, Size}),
            {error, malloc_failed};
        {ok, [Ptr]} ->
            ao:c({malloc_success, Ptr, Size}),
            {ok, Ptr};
        {error, Error} ->
            {error, Error}
    end.

free(Port, Ptr) ->
    case cu_beamr:call(Port, "free", [Ptr]) of
        {ok, Res} ->
            ao:c({free_result, Res}),
            ok;
        {error, Error} ->
            {error, Error}
    end.