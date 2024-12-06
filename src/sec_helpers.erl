-module(sec_helpers).
-export([write_to_file/2, read_file/1, run_command/1]).

-include("include/hb.hrl").
-hb_debug(print).

%% Helper function to write data to a file
write_to_file(FilePath, Data) ->
    case file:write_file(FilePath, Data) of
        ok -> ?event({"Written data to file", FilePath});
        {error, Reason} -> ?event({"Failed to write to file", FilePath, Reason})
    end.

%% Helper function to read a file
read_file(FilePath) ->
    ?event({"Reading file", FilePath}),
    case file:read_file(FilePath) of
        {ok, Data} -> {FilePath, Data};
        {error, Reason} -> {error, Reason}
    end.

%% Generalized function to run a shell command and optionally apply a success function
%% When SuccessFun is provided, it is called upon successful execution
run_command(Command) ->
    ?event({"Executing command", Command}),
    Output = os:cmd(Command ++ " 2>&1"),
    case Output of
        % Empty output interpreted as success if no output is expected
        "" -> {ok, []};
        % Return output for further inspection
        _ -> {ok, Output}
    end.