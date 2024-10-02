-module(dev_checkpoint).
-export([uses/0, init/2, execute/2, read/2]).

-include("src/include/ao.hrl").

-define(ROOT, "data").

read(_ProcID, _AssignmentID) ->
    unavailable.

uses() -> all.

init(State, _Params) ->
    % TODO: Read the latest checkpoint if it exists.
    {ok, State# { fs => #{} }}.

execute(_Msg, State = #{ process := Process, fs := FS, phase := post_exec }) ->
    case isCheckpointSlot(State) of
        true -> write_checkpoint(State);
        false -> ok
    end,
    write_result(Process, FS),
    {ok, State# { fs => #{} }};
execute(_Msg, State) ->
    {ok, State}.

isCheckpointSlot(_) -> true.

write_checkpoint(_State) -> ok.

write_result(Proc, FS) when is_record(Proc, tx) ->
    write_result(?ROOT ++ "/" ++ binary_to_list(Proc#tx.id) ++ "/results/", FS);
write_result(Prefix, FS) ->
    maps:map(fun(Path, Data) when is_binary(Data) ->
        filelib:ensure_dir(Prefix ++ "/" ++ Path),
        file:write_file(Prefix ++ "/" ++ Path, Data);
    (Path, Dir) -> write_result(Prefix ++ "/" ++ Path, Dir)
    end, FS),
    ok.