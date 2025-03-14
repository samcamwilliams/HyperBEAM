%%% @doc Simple utilities for testing HyperBEAM.
-module(hb_test_utils).
-export([suite_with_opts/2, run/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Run each test in a suite with each set of options. Start and reset
%% the store(s) for each test. Expects suites to be a list of tuples with
%% the test name, description, and test function.
%% The list of `Opts' should contain maps with the `name' and `opts' keys.
%% Each element may also contain a `skip' key with a list of test names to skip.
%% They can also contain a `desc' key with a description of the options.
suite_with_opts(Suite, OptsList) ->
    lists:map(
        fun(OptSpec = #{ name := _Name, opts := Opts, desc := ODesc}) ->
            Store = hb_opts:get(store, hb_opts:get(store), Opts),
            Skip = maps:get(skip, OptSpec, []),
            {foreach,
                fun() ->
                    ?event(rocksdb, {starting, Store}),
                    hb_store:start(Store)
                end,
                fun(_) ->
                    %hb_store:reset(Store)
                    ok
                end,
                [
                    {ODesc ++ ": " ++ TestDesc, fun() -> Test(Opts) end}
                ||
                    {TestAtom, TestDesc, Test} <- Suite, 
                        not lists:member(TestAtom, Skip)
                ]
            }
        end,
        OptsList
    ).

%% Run a single test with a given set of options.
run(Name, OptsName, Suite, OptsList) ->
    {_, _, Test} = lists:keyfind(Name, 1, Suite),
    [Opts|_] =
        [ O || #{ name := OName, opts := O } <- OptsList,
            OName == OptsName
        ],
    Test(Opts).