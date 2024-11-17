-module(hb_beamr_io).
-export([size/1, read/3, write/3]).
-export([read_string/2, write_string/2]).
-export([malloc/2, free/2]).

-include("include/hb.hrl").

size(Port) ->
    Port ! {self(), {command, term_to_binary({size})}},
    receive
        {execution_result, Size} ->
            {ok, Size}
    end.

write(Port, Offset, Data) when is_binary(Data) ->
    ?event(writing_to_mem),
    Port ! {self(), {command, term_to_binary({write, Offset, Data})}},
    ?event(mem_written),
    receive
        ok -> ok
    end.

write_string(Port, Data) when is_list(Data) ->
    write_string(Port, iolist_to_binary(Data));
write_string(Port, Data) when is_binary(Data) ->
    DataSize = byte_size(Data) + 1,
    String = <<Data/bitstring, 0:8>>,
    case malloc(Port, DataSize) of
        {ok, Ptr} ->
            case write(Port, Ptr, String) of
                ok -> {ok, Ptr}
            end;
        Error -> Error
    end.

read(Port, Offset, Size) ->
    Port ! {self(), {command, term_to_binary({read, Offset, Size})}},
    receive
        {execution_result, Result} -> {ok, Result}
    end.

read_string(Port, Offset) ->
    {ok, iolist_to_binary(do_read_string(Port, Offset, 8))}.

do_read_string(Port, Offset, ChunkSize) ->
    {ok, Data} = read(Port, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(Port, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.

malloc(Port, Size) ->
    case hb_beamr:call(Port, "malloc", [Size]) of
        {ok, [0]} ->
            ?event({malloc_failed, Size}),
            {error, malloc_failed};
        {ok, [Ptr]} ->
            ?event({malloc_success, Ptr, Size}),
            {ok, Ptr};
        {error, Error} ->
            {error, Error}
    end.

free(Port, Ptr) ->
    case hb_beamr:call(Port, "free", [Ptr]) of
        {ok, Res} ->
            ?event({free_result, Res}),
            ok;
        {error, Error} ->
            {error, Error}
    end.