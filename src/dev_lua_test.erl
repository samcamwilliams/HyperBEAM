%%% A wrapper module for generating and executing EUnit tests for all Lua scripts.
%%% When executed with `rebar3 eunit --module dev_lua_tests`, it will scan the
%%% `scripts' directory for all Lua files, and generate an EUnit test suite for
%%% each one. An individual test is generated for each function in the global
%%% `_G' table that ends in `_test'.
-module(dev_lua_test).
-include_lib("eunit/include/eunit.hrl").

%% @doc Generate an EUnit test suite for each Lua script in the `scripts`
%% directory. Each suite contains a seriest of tests that map to each of the 
%% function in the global `_G' table that end in `_test'.
exec_test_() ->
    {ok, Files} = file:list_dir("scripts"),
    lists:map(
        fun suite/1,
        lists:filter(
            fun(File) ->
                terminates_with(File, <<"lua">>)
            end,
            Files
        )
    ).

%% @doc Generate an EUnit test suite for a given Lua script.
suite(File) ->
    {ok, Script} = file:read_file(filename:join("scripts", File)),
    {ok, State} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"lua@5.3a">>,
                <<"script">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"module">> => File,
                    <<"body">> => Script
                }
            },
            <<"init">>,
            #{}
        ),
    {foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        lists:map(
            fun(FuncName) ->
                {
                    File ++ "/" ++ binary_to_list(FuncName),
                    fun() -> exec_test(State, FuncName) end
                }
            end,
            lists:filter(
                fun(FuncName) ->
                    terminates_with(FuncName, <<"_test">>)
                end,
                hb_ao:get(<<"functions">>, State, #{})
            )
        )
    }.

%% @doc Generate an EUnit test for a given function.
exec_test(State, Function) ->
    {Status, Result} =
        hb_ao:resolve(
            State,
            #{ <<"path">> => Function, <<"parameters">> => [] },
            #{}
        ),
    case Status of
        ok -> ok;
        error ->
            hb_util:debug_print(Result, <<"Lua">>, Function, 1),
            ?assertEqual(
                ok,
                Status
            )
    end.

%%% Utility functions.

%% @doc Check if a string terminates with a given suffix.
terminates_with(String, Suffix) ->
    binary:longest_common_suffix(lists:map(fun hb_util:bin/1, [String, Suffix]))
        == byte_size(Suffix).
