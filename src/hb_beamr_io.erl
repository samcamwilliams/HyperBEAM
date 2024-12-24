%%% @doc Simple interface for memory management for Beamr instances.
%%% It allows for reading and writing to memory, as well as allocating and 
%%% freeing memory by calling the WASM module's exported malloc and free
%%% functions.
%%% 
%%% Unlike the majority of HyperBEAM modules, this module takes a defensive
%%% approach to type checking, breaking from the conventional Erlang style, 
%%% such that failures are caught in the Erlang-side of functions rather than
%%% in the C/WASM-side.

-module(hb_beamr_io).
-export([size/1, read/3, write/3]).
-export([read_string/2, write_string/2]).
-export([malloc/2, free/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Get the size (in bytes) of the native memory allocated in the Beamr
%% instance. Note that WASM memory can never be reduced once granted to an
%% instance (although it can, of course, be reallocated _inside_ the 
%% environment).
size(Port) when is_port(Port) ->
    Port ! {self(), {command, term_to_binary({size})}},
    receive
        {execution_result, Size} ->
            {ok, Size}
    end.

%% @doc Write a binary to the Beamr instance's native memory at a given offset.
write(Port, Offset, Data)
        when is_port(Port)
        andalso is_binary(Data)
        andalso is_integer(Offset) ->
    ?event(writing_to_mem),
    Port ! {self(), {command, term_to_binary({write, Offset, Data})}},
    ?event(mem_written),
    receive
        ok -> ok;
        {error, Error} -> {error, Error}
    end.

%% @doc Simple helper function to allocate space for (via malloc) and write a
%% string to the Beamr instance's native memory. This can be helpful for easily
%% pushing a string into the instance, such that the resulting pointer can be
%% passed to exported functions from the instance.
%% Assumes that the input is either an iolist or a binary, adding a null byte
%% to the end of the string.
write_string(Port, Data) when is_list(Data) ->
    write_string(Port, iolist_to_binary(Data));
write_string(Port, Data) when is_port(Port) andalso is_binary(Data) ->
    DataSize = byte_size(Data) + 1,
    String = <<Data/bitstring, 0:8>>,
    case malloc(Port, DataSize) of
        {ok, Ptr} ->
            case write(Port, Ptr, String) of
                ok -> {ok, Ptr};
                {error, Error} -> {error, Error}
            end;
        Error -> Error
    end.

%% @doc Read a binary from the Beamr instance's native memory at a given offset
%% and of a given size.
read(Port, Offset, Size)
        when is_port(Port)
        andalso is_integer(Offset)
        andalso is_integer(Size) ->
    ?event({read_request, {port, Port}, {location, Offset}, {size, Size}}),
    Port ! {self(), {command, term_to_binary({read, Offset, Size})}},
    ?event(read_req_sent),
    receive
        {execution_result, Result} ->
            ?event(
                {read_result,
                    {port, Port},
                    {location, Offset},
                    {size, Size},
                    {result, Result}}),
            {ok, Result};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Simple helper function to read a string from the Beamr instance's native
%% memory at a given offset. Memory is read by default in chunks of 8 bytes,
%% but this can be overridden by passing a different chunk size. Strings are 
%% assumed to be null-terminated.
read_string(Port, Offset) -> read_string(Port, Offset, 8).
read_string(Port, Offset, ChunkSize)
        when is_port(Port)
        andalso is_integer(Offset)
        andalso is_integer(ChunkSize) ->
    {ok, iolist_to_binary(do_read_string(Port, Offset, ChunkSize))}.

do_read_string(Port, Offset, ChunkSize) ->
    {ok, Data} = read(Port, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(Port, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.

%% @doc Allocate space for (via an exported malloc function from the WASM) in 
%% the Beamr instance's native memory.
malloc(Port, Size) when is_port(Port) andalso is_integer(Size) ->
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

%% @doc Free space allocated in the Beamr instance's native memory via a
%% call to the exported free function from the WASM.
free(Port, Ptr) when is_port(Port) andalso is_integer(Ptr) ->
    case hb_beamr:call(Port, "free", [Ptr]) of
        {ok, Res} ->
            ?event({free_result, Res}),
            ok;
        {error, Error} ->
            {error, Error}
    end.

%%% Tests

%% @doc Test writing memory in and out of bounds.
write_test() ->
    % Load the `test-print` WASM module, which has a simple print function.
    % We do not call the function here, but instead check that we can write
    % to its memory. It has a single page (65,536 bytes) of memory.
    {ok, File} = file:read_file("test/test-print.wasm"),
    {ok, Port, _Imports, _Exports} = hb_beamr:start(File),
    % Check that we can write memory inside the bounds of the WASM module.
    ?assertEqual(ok, write(Port, 0, <<"Hello, World!">>)),
    % Check that we can safely handle out-of-bounds writes.
    ?assertMatch({error, _}, write(Port, 1000000, <<"Bad hello world!">>)).

%% @doc Test reading memory in and out of bounds.
read_test() ->
    % Our `test-print` module is hand-written in WASM, so we know that it
    % has a `Hello, World!` string at precisely offset 66.
    {ok, File} = file:read_file("test/test-print.wasm"),
    {ok, Port, _Imports, _Exports} = hb_beamr:start(File),
    % Check that we can read memory inside the bounds of the WASM module.
    ?assertEqual({ok, <<"Hello, World!">>}, read(Port, 66, 13)),
    % Check that we can safely handle out-of-bounds reads.
    ?assertMatch({error, _}, read(Port, 1000000, 13)).

%% @doc Test allocating and freeing memory.
malloc_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, Port, _Imports, _Exports} = hb_beamr:start(File),
    % Check that we can allocate memory inside the bounds of the WASM module.
    ?assertMatch({ok, _}, malloc(Port, 100)),
    % Check that we can safely handle out-of-bounds allocations.
    % The WASM module has a maximum of 259 pages (16MB) of memory, so we
    % should not be able to allocate more than that.
    ?assertMatch({error, _}, malloc(Port, 128 * 1024 * 1024)).

%% @doc Write and read strings to memory.
string_write_and_read_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, Port, _Imports, _Exports} = hb_beamr:start(File),
    {ok, Ptr} = write_string(Port, <<"Hello, World!">>),
    ?assertEqual({ok, <<"Hello, World!">>}, read_string(Port, Ptr)).