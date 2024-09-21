-module(cu_device_stack).
-export([from_process/1, normalize/3, call/2]).

%%% Functions for wrangling AO process devices individually or as stacks.
%%% See cu_process.erl for an overview of this architecture and its
%%% specifics.

-include("include/ao.hrl").

from_process(M) when is_record(tx, M) ->
    from_process(M#tx.tags);
from_process([]) -> [];
from_process([{<<"Device">>, DevID}| Tags]) ->
    case cu_device_loader:from_id(DevID) of
        {ok, ModName} ->
            {Params, Rest} = extract_params(Tags),
            [{ModName, Params, undefined}|from_process(Rest)];
        {error, Reason} ->
            throw({error_getting_device, DevID, Reason})
    end.

extract_params(Tags) -> extract_params([], Tags).
extract_params(Params, []) ->
    {lists:reverse(Params), []};
extract_params(Params, Rest = [{<<"Device">>, _}|_]) ->
    {lists:reverse(Params), Rest};
extract_params(Params, [{PName, PVal}|Rest]) ->
    extract_params([{PName, PVal}|Params], Rest).

normalize(Pre, Proc, Post) ->
    Devs = normalize(Pre) ++ extract_devs(Proc) ++ normalize(Post),
    lists:merge(
        fun({DevMod, DevS, Params}, N) ->
            case cu_device_loader:from_id(DevMod) of
                {ok, Mod} ->
                    {N, Mod, DevS, Params};
                Else -> throw(Else)
            end
        end,
        lists:zip(Devs, lists:seq(1, lists:length(Devs))
    ).
normalize([]) -> [];
normalize([{DevMod, Params}|Rest]) ->
    [{DevMod, undefined, Params} | normalize(Rest) ];
normalize([ Dev = {_DevMod, _InitPriv, _Params} | Rest ]) ->
    [ Dev | normalize_devs(Rest) ];
normalize([DevID|Rest]) ->
    [{DevID, undefined, []} | normalize_devs(Rest) ].

%% @doc Run a call across a state containing a stack of devices
call(S, FuncName) -> call(S, FuncName, #{}).
call(S#state { devices = Devs }, FuncName, Opts) ->
    % Reset the shared global state variables for the stack before calling
    do_call(
        Devs,
        S#{ result => undefined, errors => [], pass => 1 }, FuncName, Opts
    ).

do_call([], S, _FuncName, _Opts) ->
    case maps:get(return, Opts, ok_if_no_errors) of
        ok_if_no_errors ->
            case S#{ errors } of
                [] -> {ok, S};
                Errs -> {error, Errs, S}
            end;
        ok -> {ok, S}
    end;
do_call([Dev = {N, DevMod, DevS, Params}|Devs], S#{ pass := Pass }, FuncName, Opts) ->
    case call_dev(Dev, FuncName, maps:get(pre, Opts, []) ++ [S, DevS, Params]) of
        {error, Info} ->
            case maps:get(error_strategy, Opts, stop) of
                stop -> {error, N, DevMod, Info};
                throw -> throw({error_running_dev, N, DevMod, Info});
                continue ->
                    do_call(
                        Devs,
                        S#{ errors = S#{errors} ++ [{N, DevMod, Info}]},
                        FuncName,
                        Opts
                    );
                ignore ->
                    do_call(DevS, S, FuncName, Opts)
            end;
        {repass, NewS} ->
            case maps:get(repass, Opts, disallowed) of
                disallowed ->
                    % If we cannot handle repassing automatically, return the rest of
                    % the device stack to the caller, as well as the new state.
                    {repass, Devs, NewS};
                allowed ->
                    do_call(NewS#{ devices }, NewS#{ pass => Pass + 1 }, FuncName, Opts)
            end;
        {ok, NewS} ->
            do_call(Devs, NewS, FuncName, Opts)
    end.
    

call_dev(DevMod, FuncName, []) ->
    {error, {not_runnable, DevMod, FuncName}};
call_dev(Dev = {_, DevMod, _, _}, FuncName, Args) ->
    case erlang:function_exported(DevMod, FuncName, length(Args)) of
        true -> handle_result(Dev, erlang:apply(DevMod, FuncName, Args));
        false -> call_dev(DevMod, FuncName, lists:droplast(Args))
    end.

%% @doc If the device returned a new private state, roll this into the global
%% state, returning only ok and this term.
handle_result(Dev, {ok, NewState, NewPrivState}) ->
    {ok, #{ devices = update(NewState, Dev, PrivState)}};
handle_result(_Dev, Res) -> Res.

%% @doc Update the private state of the device (maintaining list stability).
update(State#{ devices = Devs }, {N, Mod, _, Params}, NewDevState) ->
    State#{
        devices = lists:keyreplace(N, 1, Devs, {N, Mod, NewDevState, Params})
    }.