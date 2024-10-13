-module(cu_device_stack).
-export([from_process/1, normalize/3, call/2, call/3]).

%%% Functions for wrangling AO process devices individually or as stacks.
%%% See cu_process.erl for an overview of this architecture and its
%%% specifics.

-include("include/ao.hrl").
-ao_debug(print).

from_process(M) when is_record(M, tx) ->
    from_process(M#tx.tags);
from_process([]) -> [];
from_process([{<<"Device">>, DevID}| Tags]) ->
    case cu_device_loader:from_id(DevID) of
        {ok, ModName} ->
            {Params, Rest} = extract_params(Tags),
            [{ModName, Params, undefined}|from_process(Rest)];
        {error, Reason} ->
            throw({error_getting_device, DevID, Reason})
    end;
from_process([{_OtherTag, _OtherVal}|Tags]) ->
    from_process(Tags).

extract_params(Tags) -> extract_params([], Tags).
extract_params(Params, []) ->
    {lists:reverse(Params), []};
extract_params(Params, Rest = [{<<"Device">>, _}|_]) ->
    {lists:reverse(Params), Rest};
extract_params(Params, [{PName, PVal}|Rest]) ->
    extract_params([{PName, PVal}|Params], Rest).

normalize(Pre, Proc, Post) ->
    Devs = normalize(Pre) ++ from_process(Proc) ++ normalize(Post),
    lists:map(
        fun({{DevMod, DevS, Params}, N}) ->
            case cu_device_loader:from_id(DevMod) of
                {ok, Mod} ->
                    {N, Mod, DevS, Params};
                Else -> throw(Else)
            end
        end,
        lists:zip(Devs, lists:seq(1, length(Devs)))
    ).

normalize([]) -> [];
normalize([{DevMod, Params}|Rest]) ->
    [{DevMod, undefined, Params} | normalize(Rest) ];
normalize([ Dev = {_DevMod, _InitPriv, _Params} | Rest ]) ->
    [ Dev | normalize(Rest) ];
normalize([DevID|Rest]) ->
    [{DevID, undefined, []} | normalize(Rest) ].

%% @doc Run a call across a state containing a stack of devices
call(S, FuncName) -> call(S, FuncName, #{}).
call(S = #{ devices := Devs }, FuncName, Opts) ->
    % Reset the shared global state variables for the stack before calling
    do_call(
        Devs,
        S#{ result => undefined, errors => [], pass => 1 }, FuncName, Opts
    ).

do_call([], S, _FuncName, _Opts) -> {ok, S};
do_call(AllDevs = [Dev = {_N, DevMod, DevS, Params}|Devs], S, FuncName, Opts) ->
    ?c({calling, DevMod, FuncName}),
    case call_dev(S, Dev, FuncName, maps:get(arg_prefix, Opts, []) ++ [S, DevS, Params]) of
        {ok, NewS} when is_map(NewS) ->
            do_call(Devs, NewS, FuncName, Opts);
        {ok, NewS, NewPrivS} when is_map(NewS) -> do_call(Devs, update(NewS, Dev, NewPrivS), FuncName, Opts);
        {pass, NewS} when is_map(NewS) -> maybe_pass(NewS, FuncName, Opts);
        {pass, NewS, NewPrivS} when is_map(NewS) -> maybe_pass(update(NewS, Dev, NewPrivS), FuncName, Opts);
        {error, Info} -> maybe_error(AllDevs, S, FuncName, Opts, Info);
        Unexpected -> maybe_error(AllDevs, S, FuncName, Opts, {unexpected_result, Unexpected})
    end.

maybe_error([{N, DevMod, _DevS, _Params}|Devs], S = #{ errors := Errs }, FuncName, Opts, Info) ->
    case maps:get(error_strategy, Opts, stop) of
        stop -> {error, N, DevMod, Info};
        throw -> throw({error_running_dev, N, DevMod, Info});
        continue ->
            do_call(
                Devs,
                S#{ errors := Errs ++ [{N, DevMod, Info}]},
                FuncName,
                Opts
            );
        ignore -> do_call(Devs, S, FuncName, Opts)
    end.

maybe_pass(NewS = #{ pass := Pass }, FuncName, Opts) ->
    case maps:get(pass, Opts, allowed) of
        disallowed ->
            % If we cannot handle repassing automatically, return the rest of
            % the device stack to the caller, as well as the new state.
            {pass, NewS};
        allowed ->
            #{ devices := NewDevs } = NewS,
            do_call(NewDevs, NewS#{ pass => Pass + 1 }, FuncName, Opts)
    end.

call_dev(S, _DevMod, _FuncName, []) ->
    % If the device doesn't implement the function, we just return the state
    % as is.
    {ok, S};
call_dev(S, Dev = {_, DevMod, _, _}, FuncName, Args) ->
    case erlang:function_exported(DevMod, FuncName, length(Args)) of
        true ->
            try erlang:apply(DevMod, FuncName, Args)
            catch _Type:Error:BT -> {error, {Error, BT}}
            end;
        false -> call_dev(S, Dev, FuncName, lists:droplast(Args))
    end.

%% @doc Update the private state of the device (maintaining list stability).
update(S = #{ devices := Devs }, {N, Mod, _, Params}, NewDevState) ->
    S#{
        devices := lists:keyreplace(N, 1, Devs, {N, Mod, NewDevState, Params})
    }.