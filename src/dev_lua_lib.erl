%%% @doc A module for providing AO library functions to the Lua environment.
%%% This module contains the implementation of the functions, each by the name
%%% that should be used in the `ao' table in the Lua environment. Every export
%%% is imported into the Lua environment.
%%% 
%%% Each function adheres closely to the Luerl calling convention, adding the 
%%% appropriate node message as a third argument:
%%% 
%%%     fun(Args, State, NodeMsg) -> {ResultTerms, NewState}
%%% 
%%% As Lua allows for multiple return values, each function returns a list of
%%% terms to grant to the caller. Matching the tuple convention used by AO-Core,
%%% the first term is typically the status, and the second term is the result.
-module(dev_lua_lib).
%%% Library functions. Each exported function is _automatically_ added to the
%%% Lua environment, except for the `install/3' function, which is used to
%%% install the library in the first place.
-export([resolve/3, set/3, event/3, install/3]).
-include("include/hb.hrl").

%% @doc Install the library into the given Lua environment.
install(Base, State, Opts) ->
    % Calculate and set the new `preloaded_devices' option.
    AllDevs = hb_opts:get(preloaded_devices, Opts),
    DevSandboxDef =
        hb_ao:get(
            <<"device-sandbox">>,
            {as, <<"message@1.0">>, Base},
            false,
            Opts
        ),
    AdmissibleDevs =
        case DevSandboxDef of
            false -> AllDevs;
            DevNames ->
                lists:map(
                    fun(Name) ->
                        [Dev] =
                            lists:filter(
                                fun(X) ->
                                    hb_ao:get(<<"name">>, X, Opts) == Name
                                end,
                                AllDevs
                            ),
                        Dev
                    end,
                    hb_util:message_to_ordered_list(DevNames)
                )
        end,
    ?event({adding_ao_core_resolver, {devs, AdmissibleDevs}}),
    ExecOpts = Opts#{ preloaded_devices => AdmissibleDevs },
    % Initialize the AO-Core resolver.
    BaseAOTable =
        case luerl:get_table_keys_dec([ao], State) of
            {ok, nil, _} ->
                ?event(no_ao_table),
                #{};
            {ok, ExistingTable, _} ->
                ?event({existing_ao_table, ExistingTable}),
                dev_lua:decode(ExistingTable)
        end,
    ?event({base_ao_table, BaseAOTable}),
    {ok, State2} =
        luerl:set_table_keys_dec(
            [ao],
            dev_lua:encode(BaseAOTable),
            State
        ),
    {
        ok,
        lists:foldl(
            fun(FuncName, StateIn) ->
                {ok, StateOut} =
                    luerl:set_table_keys_dec(
                        [ao, FuncName],
                        fun(Args, ImportState) ->
                            dev_lua_lib:FuncName(Args, ImportState, ExecOpts)
                        end,
                        StateIn
                    ),
                StateOut
            end,
            State2,
            [
                FuncName
            ||
                {FuncName, _} <- dev_lua_lib:module_info(exports),
                FuncName /= module_info
            ]
        )
    }.

%% @doc A wrapper function for performing AO-Core resolutions. Offers both the 
%% single-message (using `hb_singleton:from/1' to parse) and multiple-message
%% (using `hb_ao:resolve_many/2') variants.
resolve([EncodedMsg], ExecState, ExecOpts) ->
    AOMsg = dev_lua:decode(luerl:decode(EncodedMsg, ExecState)),
    ?event({ao_core_resolver, {msg, AOMsg}}),
    ParsedMsgs = hb_singleton:from(AOMsg),
    ?event({parsed_msgs_to_resolve, ParsedMsgs}),
    resolve(ParsedMsgs, ExecState, ExecOpts);
resolve(RawMsgs, ExecState, ExecOpts) ->
    MaybeAsMsgs = lists:map(fun convert_as/1, RawMsgs),
    try hb_ao:resolve_many(MaybeAsMsgs, ExecOpts) of
        {Status, Res} ->
            ?event({resolved_msgs, {status, Status}, {res, Res}}),
            {[hb_util:bin(Status), dev_lua:encode(Res)], ExecState}
    catch
        Error ->
            ?event(lua_error, {ao_core_resolver_error, Error}),
            {[<<"error">>, Error], ExecState}
    end.

%% @doc Converts any `as' terms from Lua to their HyperBEAM equivalents.
convert_as([<<"as">>, Device, RawMsg]) ->
    {as, Device, RawMsg};
convert_as(Other) ->
    Other.

%% @doc Wrapper for `hb_ao`'s `set' functionality.
set([EncodedBase, EncodedKey, EncodedValue], ExecState, ExecOpts) ->
    Base = dev_lua:decode(luerl:decode(EncodedBase, ExecState)),
    Key = dev_lua:decode(luerl:decode(EncodedKey, ExecState)),
    Value = dev_lua:decode(luerl:decode(EncodedValue, ExecState)),
    ?event({ao_core_set, {base, Base}, {key, Key}, {value, Value}}),
    NewRes = dev_lua:encode(hb_ao:set(Base, Key, Value, ExecOpts)),
    {[NewRes], ExecState};
set([EncodedBase, NewValues], ExecState, ExecOpts) ->
    BaseMsg = dev_lua:decode(luerl:decode(EncodedBase, ExecState)),
    Values = dev_lua:decode(luerl:decode(NewValues, ExecState)),
    ?event({ao_core_set, {base, BaseMsg}, {new_values, Values}}),
    NewRes = dev_lua:encode(hb_ao:set(BaseMsg, Values, ExecOpts)),
    {[NewRes], ExecState}.

%% @doc Allows Lua scripts to signal events using the HyperBEAM hosts internal
%% event system.
event([EncodedEvent], ExecState, Opts) ->
    event([<<"lua_event">>, EncodedEvent], ExecState, Opts);
event([RawGroup, EncodedEvent], ExecState, Opts) ->
    Group =
        try dev_lua:decode(RawGroup)
        catch
            error:_ ->
                ?event(lua_error,
                    {group_decode_failed, {group, RawGroup}}
                ),
                lua_event
        end,
    Event =
        try dev_lua:decode(luerl:decode(EncodedEvent, ExecState))
        catch
            error:Reason ->
                ?event(lua_error,
                    {event_decode_failed,
                        {reason, Reason},
                        {event, EncodedEvent}
                    }
                ),
                #{<<"error">> => Reason}
        end,
    ?event(Group, {Group, Event}, Opts),
    {[<<"ok">>], ExecState}.