-module(dev_test).
-export([info/1, test_func/1, compute/3, init/3, restore/3, snapshot/3, mul/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% A simple test device for Converge, so that we can test the functionality that
%%% depends on using Erlang's module system.
%%% 
%%% NOTE: This device is labelled `Test-Device/1.0' to avoid conflicts with
%%% other testing functionality -- care should equally be taken to avoid
%%% using the `test' key in other settings.

%% @doc Exports a default_handler function that can be used to test the
%% handler resolution mechanism.
info(_) ->
	#{
	}.

test_func(_) ->
	{ok, <<"GOOD_FUNCTION">>}.

%% @doc Example implementation of a `compute' handler. Makes a running list of
%% the slots that have been computed in the state message and places the new
%% slot number in the results key.
compute(Msg1, Msg2, Opts) ->
    AssignmentSlot = hb_converge:get(<<"Assignment/Slot">>, Msg2, Opts),
    Seen = hb_converge:get(<<"Already-Seen">>, Msg1, Opts),
    {ok,
        hb_converge:set(
            Msg1,
            #{
                <<"Results">> =>
                    #{ <<"Assignment-Slot">> => AssignmentSlot },
                <<"Already-Seen">> => [AssignmentSlot | Seen]
            },
            Opts
        )
    }.

%% @doc Example `init/3' handler. Sets the `Already-Seen' key to an empty list.
init(Msg, _Msg2, Opts) ->
    ?event({init_called_on_dev_test, Msg}),
    {ok, hb_converge:set(Msg, #{ <<"Already-Seen">> => [] }, Opts)}.

%% @doc Example `restore/3' handler. Sets the hidden key `Test/Started` to the
%% value of `Current-Slot` and checks whether the `Already-Seen` key is valid.
restore(Msg, _Msg2, Opts) ->
    ?event({restore_called_on_dev_test, Msg}),
    case hb_converge:get(<<"Already-Seen">>, Msg, Opts) of
        not_found ->
            ?event({restore_not_found, Msg}),
            {error, <<"No viable state to restore.">>};
        AlreadySeen ->
            ?event({restore_found, AlreadySeen}),
            {ok,
                hb_private:set(
                    Msg,
                    #{ <<"Test-Key/Started-State">> => AlreadySeen },
                    Opts
                )
            }
    end.

%% @doc Example implementation of an `imported` function for a WASM
%% executor.
mul(Msg1, Msg2) ->
    State = hb_converge:get(<<"State">>, Msg1, #{ hashpath => ignore }),
    [Arg1, Arg2] = hb_converge:get(args, Msg2, #{ hashpath => ignore }),
    {ok, #{ state => State, results => [Arg1 * Arg2] }}.

%% @doc Do nothing when asked to snapshot.
snapshot(_Msg1, _Msg2, _Opts) ->
    {ok, #{}}.

%%% Tests

%% @doc Tests the resolution of a default function.
device_with_function_key_module_test() ->
	Msg =
		#{
			device => <<"Test-Device/1.0">>
		},
	?assertEqual(
		{ok, <<"GOOD_FUNCTION">>},
		hb_converge:resolve(Msg, test_func, #{})
	).

compute_test() ->
    Msg0 = #{ device => <<"Test-Device/1.0">> },
    {ok, Msg1} = hb_converge:resolve(Msg0, init, #{}),
    Msg2 =
        hb_converge:set(
            #{ path => <<"Compute">> },
            #{
                <<"Assignment/Slot">> => 1,
                <<"Message/Number">> => 1337
            },
            #{}
        ),
    {ok, Msg3} = hb_converge:resolve(Msg1, Msg2, #{}),
    ?assertEqual(1, hb_converge:get(<<"Results/Assignment-Slot">>, Msg3, #{})),
    Msg4 =
        hb_converge:set(
            #{ path => <<"Compute">> },
            #{
                <<"Assignment/Slot">> => 2,
                <<"Message/Number">> => 9001
            },
            #{}
        ),
    {ok, Msg5} = hb_converge:resolve(Msg3, Msg4, #{}),
    ?assertEqual(2, hb_converge:get(<<"Results/Assignment-Slot">>, Msg5, #{})),
    ?assertEqual([2, 1], hb_converge:get(<<"Already-Seen">>, Msg5, #{})).

restore_test() ->
    Msg1 = #{ device => <<"Test-Device/1.0">>, <<"Already-Seen">> => [1] },
    {ok, Msg3} = hb_converge:resolve(Msg1, restore, #{}),
    ?assertEqual([1], hb_private:get(<<"Test-Key/Started-State">>, Msg3, #{})).