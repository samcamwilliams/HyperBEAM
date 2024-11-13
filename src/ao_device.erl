-module(ao_device).
-export([from_message/1]).
-export([call/3, call/4]).
-include("include/ao.hrl").

%%% The root implementation of the device call logic.
%%% Every device is a simple module that takes a single message as an argument
%%% and returns a single message as a result.
%%% 
%%% This module abstracts the handling of passing additional optional metadata
%%% to the device call, as well as catching any errors that may be thrown by the
%%% device through a generic framework.

from_message(M) ->
    case lists:keyfind(<<"Device">>, 1, M#tx.tags) of
        {_, DevID} -> ao_device_loader:from_id(DevID);
        false -> ao_device_loader:default()
    end.

call(Dev, FuncName, Args) -> call(Dev, FuncName, Args, #{}).
call(_DevMod, _FuncName, [], _Opts) ->
    % If the device doesn't implement the function, tell the caller.
    no_match;
call({DevMod, EmbeddedFunc}, FuncName, Args, Opts) ->
    call(DevMod, EmbeddedFunc, [FuncName|Args], Opts);
call(DevMod, FuncName, Args, Opts) ->
    % If the device implements the function with the given arity, call it.
    % Otherwise, recurse with one fewer arguments.
    ok = ensure_loaded(DevMod),
    case erlang:function_exported(DevMod, FuncName, length(Args)) of
        true -> maybe_unsafe_call(DevMod, FuncName, Args, Opts);
        false -> call(DevMod, FuncName, lists:droplast(Args), Opts)
    end.

ensure_loaded(DevMod) ->
    case code:ensure_loaded(DevMod) of
        {module, _Mod} -> ok;
        {error, Reason} -> exit({error_loading_device, DevMod, Reason})
    end.

%% @doc Call a device function without catching exceptions if the error
%% strategy is set to throw.
maybe_unsafe_call(DevMod, FuncName, Args, #{ error_strategy := throw }) ->
    ?c({executing_dev_call, DevMod, FuncName, length(Args)}),
    erlang:apply(DevMod, FuncName, Args);
maybe_unsafe_call(DevMod, FuncName, Args, Opts) ->
    try maybe_unsafe_call(DevMod, FuncName, Args, Opts#{ error_strategy => throw })
    catch Type:Error:BT ->
        ?c({error_calling_dev, DevMod, FuncName, length(Args), {Type, Error, BT}}),
        {error, {Type, Error, BT}}
    end.