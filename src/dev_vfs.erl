%%% @doc A virtual filesystem device.
%%% Implements a file-system-as-map structure, which is traversible externally.
%%% Each file is a binary and each directory is a Converge message.
%%% Additionally, this module adds a series of WASI-preview-1 compatible
%%% functions for accessing the filesystem as imported functions by WASM
%%% modules.

-module(dev_vfs).
-export([init/3, compute/1, stdout/1]).
-export([path_open/3, fd_write/3, fd_read/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(INIT_VFS,
    #{
        <<"dev">> => #{
            <<"stdin">> => <<>>,
            <<"stdout">> => <<>>,
            <<"stderr">> => <<>>
        }
    }
).

-define(INIT_FDS,
    #{
        0 => #{
            <<"Filename">> => <<"/dev/stdin">>,
            <<"Offset">> => 0
        },
        1 => #{
            <<"Filename">> => <<"/dev/stdout">>,
            <<"Offset">> => 0
        },
        2 => #{
            <<"Filename">> => <<"/dev/stderr">>,
            <<"Offset">> => 0
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
                <<"WASM/stdlib/wasi_snapshot_preview1">> =>
                    #{ device => <<"VFS/WASI-1.0">>}
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

compute(Msg1) ->
    {ok, Msg1}.

% %% @doc Encode the input message for inclusion in the VFS.
% execute(M1, M2, Opts) ->
%     case hb_converge:get(<<"Pass">>, M1, Opts) of
%         1 ->
%             MsgToProc = hb_converge:get(<<"Message">>, M2, Opts),
%             JSON =
%                 ar_bundles:serialize(
%                     hb_message:message_to_tx(MsgToProc),
%                     json
%                 ),
%             ?event(setting_message_vfs_key),
%             {ok,
%                 hb_converge:set(
%                     M1,
%                     <<"vfs/message">>,
%                     JSON,
%                     Opts
%                 )
%             };
%         _ -> {ok, M1}
%     end.

%% @doc Return the stdout buffer from a state message.
stdout(M) ->
    hb_converge:get(<<"vfs/dev/stdout">>, M).

%% @doc Adds a file descriptor to the state message.
%path_open(M, Port, [FDPtr, LookupFlag, PathPtr|_]) ->
path_open(Msg1, Msg2, Opts) ->
    FDs = hb_converge:get(<<"File-Descriptors">>, Msg1, Opts),
    Port = hb_private:get(<<"Port">>, Msg1, Opts),
    [FDPtr, LookupFlag, PathPtr|_] = hb_converge:get(<<"Args">>, Msg2, Opts),
    ?event({path_open, FDPtr, LookupFlag, PathPtr}),
    Path = hb_beamr_io:read_string(Port, PathPtr),
    ?event({path_open, Path}),
    FD = #{
        index := Index
    } =
        case hb_converge:get(<<"vfs/", Path/binary>>, Msg1) of
            not_found ->
                #{
                    index => length(hb_converge:keys(FDs)) + 1,
                    filename => Path,
                    offset => 0
                };
            F -> F
        end,
    {
        ok,
        #{
            state =>
                hb_converge:set(
                    Msg1,
                    <<"vfs/", Path/binary>>,
                    FD
                ),
            wasm_response => [0, Index]
        }
    }.

%% @doc WASM stdlib implementation of `fd_write', using the WASI-p1 standard
%% interface.
fd_write(Msg1, Msg2, Opts) ->
    State = hb_converge:get(<<"State">>, Msg1, Opts),
    Port = hb_private:get(<<"WASM/Port">>, State, Opts),
    [FD, Ptr, Vecs, RetPtr|_] = hb_converge:get(<<"Args">>, Msg2, Opts),
    fd_write(State, Port, [FD, Ptr, Vecs, RetPtr], 0, Opts).

fd_write(S, Port, [_, _Ptr, 0, RetPtr], BytesWritten, _Opts) ->
    hb_beamr_io:write(
        Port,
        RetPtr,
        <<BytesWritten:64/little-unsigned-integer>>
    ),
    {ok, #{ state => S, wasm_response => [0] }};
fd_write(S, Port, [FDnum, Ptr, Vecs, RetPtr], BytesWritten, Opts) ->
    FDNumStr = integer_to_binary(FDnum),
    FD = hb_converge:get(<<"File-Descriptors/", FDNumStr/binary>>, S, Opts),
    Filename = hb_converge:get(<<"Filename">>, FD, Opts),
    StartOffset = hb_converge:get(<<"Offset">>, FD, Opts),
    {VecPtr, Len} = parse_iovec(Port, Ptr),
    {ok, Data} = hb_beamr_io:read(Port, VecPtr, Len),
    Before =
        binary:part(
            OrigData = hb_converge:get(<<"Data">>, FD, Opts),
            0,
            StartOffset
        ),
    After =
        binary:part(OrigData, StartOffset, byte_size(OrigData) - StartOffset),
    S1 =
        hb_converge:set(
            S,
            <<"File-Descriptors/", FDNumStr/binary, "/Offset">>,
            StartOffset + byte_size(Data),
            Opts
        ),
    S2 =
        hb_converge:set(
            S1,
            <<"vfs/", Filename/binary>>,
            <<Before/binary, Data/binary, After/binary>>,
            Opts
        ),
    fd_write(
        S2,
        Port,
        [FD, Ptr + 16, Vecs - 1, RetPtr],
        BytesWritten + byte_size(Data),
        Opts
    ).

%% @doc Read from a file using the WASI-p1 standard interface.
fd_read(Msg1, Msg2, Opts) ->
    State = hb_converge:get(<<"State">>, Msg1, Opts),
    Port = hb_private:get(<<"WASM/Port">>, State, Opts),
    [FD, VecsPtr, NumVecs, RetPtr|_] = hb_converge:get(<<"Args">>, Msg2, Opts),
    fd_read(State, Port, [FD, VecsPtr, NumVecs, RetPtr], 0, Opts).

fd_read(S, Port, [FD, _VecsPtr, 0, RetPtr], BytesRead, _Opts) ->
    ?event({{completed_read, FD, BytesRead}}),
    hb_beamr_io:write(Port, RetPtr,
        <<BytesRead:64/little-unsigned-integer>>),
    {ok, #{ state => S, wasm_response => [0] }};
fd_read(S, Port, [FDNum, VecsPtr, NumVecs, RetPtr], BytesRead, Opts) ->
    ?event({fd_read, FDNum, VecsPtr, NumVecs, RetPtr}),
    % Parse the request
    FDNumStr = integer_to_binary(FDNum),
    Filename =
        hb_converge:get(
            <<"File-Descriptors/", FDNumStr/binary, "/Filename">>, S, Opts),
    {VecPtr, Len} = parse_iovec(Port, VecsPtr),
    % Read the bytes from the file
    Data = hb_converge:get(<<"vfs/", Filename/binary>>, S, Opts),
    Offset =
        hb_converge:get(
            <<"File-Descriptors/", FDNumStr/binary, "/Offset">>, S, Opts),
    ReadSize = min(Len, byte_size(Data) - Offset),
    Bin = binary:part(Data, Offset, ReadSize),
    % Write the bytes to the WASM port
    ok = hb_beamr_io:write(Port, VecPtr, Bin),
    fd_read(
        hb_converge:set(
            S,
            <<"File-Descriptors/", FDNumStr/binary, "/Offset">>,
            Offset + ReadSize,
            Opts
        ),
        Port,
        [FDNum, VecsPtr + 16, NumVecs - 1, RetPtr],
        BytesRead + ReadSize,
        Opts
    ).

%% @doc Parse an iovec in WASI-preview-1 format.
parse_iovec(Port, Ptr) ->
    {ok, VecStruct} = hb_beamr_io:read(Port, Ptr, 16),
    <<
        BinPtr:64/little-unsigned-integer,
        Len:64/little-unsigned-integer
    >> = VecStruct,
    {BinPtr, Len}.

%%% Tests

init() ->
    application:ensure_all_started(hb).

generate_vfs_stack(File, Func, Params) ->
    init(),
    Msg0 = dev_wasm:store_wasm_image(File),
    Msg1 = Msg0#{
        device => <<"Stack/1.0">>,
        <<"Device-Stack">> => [<<"VFS/WASI-1.0">>, <<"WASM-64/1.0">>],
        <<"Stack-Keys">> => [<<"Init">>, <<"Compute">>],
        <<"WASM-Function">> => Func,
        <<"WASM-Params">> => Params
    },
    {ok, Msg2} = hb_converge:resolve(Msg1, <<"Init">>, #{}),
    Msg2.

run_vfs_stack(File, Func, Params, AdditionalMsg) ->
    Msg2 = generate_vfs_stack(File, Func, Params),
    ?event({after_init, Msg2}),
    Msg3 =
        maps:merge(
            Msg2,
            hb_converge:set(
                #{
                    <<"WASM-Function">> => Func,
                    <<"WASM-Params">> => Params
                },
                AdditionalMsg,
                #{ hashpath => ignore }
            )
        ),
    ?event({after_setup, Msg3}),
    {ok, StateRes} = hb_converge:resolve(Msg3, <<"Compute">>, #{}),
    ?event({after_resolve, StateRes}),
    {ok, StateRes}.

vfs_is_serializable_test() ->
    StackMsg = generate_vfs_stack("test/test-print.wasm", <<"hello">>, []),
    VFSMsg = hb_converge:get(<<"VFS">>, StackMsg),
    VFSMsg2 =
        hb_message:minimize(
            hb_message:tx_to_message(
                hb_message:message_to_tx(VFSMsg))),
    ?assert(hb_message:match(VFSMsg, VFSMsg2)).

stack_is_serializable_test() ->
    Msg = generate_vfs_stack("test/test-print.wasm", <<"hello">>, []),
    Msg2 = hb_message:tx_to_message(hb_message:message_to_tx(Msg)),
    ?assert(hb_message:match(Msg, Msg2)).

basic_write_test() ->
    {ok, StateRes} = run_vfs_stack("test/test-print.wasm", <<"hello">>, [], #{}),
    ?assertEqual(<<"Hello World\nHowdy!\n">>, stdout(StateRes)).
