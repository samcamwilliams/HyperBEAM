-module(dev_stack).
-export([from_process/1, create/1, create/2, create/3]).
-export([init/2, execute/2, execute/3]).

%%% A device that contains a stack of other devices, which it runs in order
%%% when its `execute` function is called.

-include("include/ao.hrl").
-ao_debug(print).

init(Process, Opts) ->
    {ok, S = #{ devices := Devices }} =
        create(
            maps:get(pre, Opts, []),
            Process,
            maps:get(post, Opts, [])
        ),
    call(
        Devices,
        S#{ results => undefined, errors => [], pass => 1 },
        init,
        maps:get(opts, Opts, #{})
    ).

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

create(Pre) -> create(Pre, [], []).
create(Pre, Post) -> create(Pre, [], Post).
create(Pre, Proc, Post) ->
    Devs = normalize_list(Pre) ++ from_process(Proc) ++ normalize_list(Post),
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

normalize_list([]) -> [];
normalize_list([{DevMod, Params}|Rest]) ->
    [{DevMod, undefined, Params} | normalize_list(Rest) ];
normalize_list([ Dev = {_DevMod, _InitPriv, _Params} | Rest ]) ->
    [ Dev | normalize_list(Rest) ];
normalize_list([DevID|Rest]) ->
    [{DevID, undefined, []} | normalize_list(Rest) ].

%% @doc Wrap calls to the device stack as if it is a single device.
%% Call the execute function on each device in the stack, then call the
%% finalize function on the resulting state.
execute(M, S) ->
    {ok, NewS} = only_execute(M, S),
    call(
        maps:get(devices, NewS, []),
        NewS,
        finalize,
        maps:get(opts, S, #{})
    ).

%% @doc Only execute the devices in the stack, without calling the finalize
%% function.
only_execute(M, S = #{ devices := Devs, state := S }) ->
    % Reset the state variables for the devices in the stack before calling
    call(
        Devs,
        S#{ results => undefined, errors => [], pass => 1 },
        execute,
        (maps:get(opts, S, #{}))#{ arg_prefix := [M] }
    ).

call(Devs, S, FuncName, Opts) ->
    do_call(
        Devs,
        S#{ results => undefined, errors => [], pass => 1 },
        FuncName,
        Opts
    ).

do_call([], S, _FuncName, _Opts) -> {ok, S};
do_call(AllDevs = [Dev = {_N, DevMod, DevS, Params}|Devs], S = #{ pass := Pass }, FuncName, Opts) ->
    ?c({calling, DevMod, FuncName, Pass}),
    case cu_device:call(DevMod, FuncName, maps:get(arg_prefix, Opts, []) ++ [S, DevS, Params], Opts) of
        no_match ->
            do_call(Devs, S, FuncName, Opts);
        {ok, NewS} when is_map(NewS) ->
            do_call(Devs, NewS, FuncName, Opts);
        {ok, NewS, NewPrivS} when is_map(NewS) ->
            do_call(Devs, update(NewS, Dev, NewPrivS), FuncName, Opts);
        {skip, NewS} when is_map(NewS) ->
            NewS;
        {skip, NewS, NewPrivS} when is_map(NewS) ->
            update(NewS, Dev, NewPrivS);
        {pass, NewS} when is_map(NewS) ->
            maybe_pass(NewS, FuncName, Opts);
        {pass, NewS, NewPrivS} when is_map(NewS) ->
            maybe_pass(update(NewS, Dev, NewPrivS), FuncName, Opts);
        {error, Info} ->
            maybe_error(AllDevs, S, FuncName, Opts, Info);
        Unexpected ->
            maybe_error(AllDevs, S, FuncName, Opts, {unexpected_result, Unexpected})
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

%% @doc Update the private state of the device (maintaining list stability).
update(S = #{ devices := Devs }, {N, Mod, _, Params}, NewDevState) ->
    S#{
        devices := lists:keyreplace(N, 1, Devs, {N, Mod, NewDevState, Params})
    }.