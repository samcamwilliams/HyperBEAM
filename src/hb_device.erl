-module(hb_device).
-export([from_message/1]).
-export([call/3, call/4]).
-include("include/hb.hrl").

%%% This module is the root of the device call logic in hyperbeam.
%%% 
%%% Every device is a collection of keys/functions that can be called in order
%%% to yield their values. Each key may return another message, or a 
%%% binary.
%%% 
%%% When a device key is called, it is passed the current message (likely its 
%%% state), as well as an optional additional message. It must return a tuple of
%%% the form {Status, NewMessage}, where Status is either ok or error, and 
%%% NewMessage is either a new message or a binary.

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
	?event({ensuring_loaded, DevMod}),
    case code:ensure_loaded(DevMod) of
        {module, _Mod} -> ok;
        {error, Reason} ->
			exit({error_loading_device, DevMod, Reason})
    end.

%% @doc Call a device function without catching exceptions if the error
%% strategy is set to throw.
maybe_unsafe_call(DevMod, FuncName, Args, #{ error_strategy := throw }) ->
    ?event({executing_dev_call, DevMod, FuncName, length(Args)}),
    erlang:apply(DevMod, FuncName, Args);
maybe_unsafe_call(DevMod, FuncName, Args, Opts) ->
    try maybe_unsafe_call(DevMod, FuncName, Args, Opts#{ error_strategy => throw })
    catch Type:Error:BT ->
        ?event({error_calling_dev, DevMod, FuncName, length(Args), {Type, Error, BT}}),
        {error, {Type, Error, BT}}
    end.

%% @doc Load a device module from its name or a message ID.
%% Returns {ok, Executable} where Executable is an atom or a tuple of the
%% form {Mod, Func}. On error, a tuple of the form {error, Reason} is returned.
from_id(ID) -> from_id(ID, #{}).

from_id({ID, Func}, Opts) ->
    case from_id(ID, Opts) of
        {ok, Mod} -> {ok, {Mod, Func}};
        Error -> Error
    end;
from_id(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
from_id(ID, _Opts) when is_binary(ID) and byte_size(ID) == 43 ->
    case lists:member(ID, hb:get(loadable_devices)) of
        true ->
            Msg = hb_client:download(ID),
            RelBin = erlang:system_info(otp_release),
            case lists:keyfind(<<"Content-Type">>, 1, Msg#tx.tags) of
                <<"BEAM/", RelBin/bitstring>> ->
                    {_, ModNameBin} = lists:keyfind(<<"Module-Name">>, 1, Msg#tx.tags),
                    ModName = list_to_atom(binary_to_list(ModNameBin)),
                    case erlang:load_module(ModName, Msg#tx.data) of
                        {module, _} -> {ok, ModName};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        false -> {error, module_not_admissable}
    end;
from_id(ID, _Opts) ->
    case maps:get(ID, hb:get(preloaded_devices), unsupported) of
        unsupported -> {error, module_not_admissable};
        Mod -> {ok, Mod}
    end.

default() -> dev_identity.

%% @doc Locate the appropriate device module to execute for the given message.
%% If the message specifies a device for itself, use that. Else, use the default
%% device.
from_message(M) ->
    case lists:keyfind(<<"Device">>, 1, M#tx.tags) of
        {_, DeviceID} ->
            from_id(DeviceID);
        false ->
            {ok, default()}
    end.
