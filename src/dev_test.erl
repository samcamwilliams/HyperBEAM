-module(dev_test).
-export([info/1, test_func/1, compute/3, init/3, restore/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% A simple test device for Converge, so that we can test the functionality that
%%% depends on using Erlang's module system.

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
    {ok, hb_converge:set(Msg, #{ <<"Already-Seen">> => [] }, Opts)}.

%% @doc Example `restore/3' handler. Sets the hidden key `Test/Started` to the
%% value of `Current-Slot` and checks whether the `Already-Seen` key is valid.
restore(Msg, _Msg2, Opts) ->
    case hb_converge:get(<<"Already-Seen">>, Msg, Opts) of
        not_found ->
            {error, <<"No viable state to restore.">>};
        AlreadySeen ->
            {ok,
                hb_private:set(
                    Msg,
                    #{ <<"Test/Started-State">> => AlreadySeen },
                    Opts
                )
            }
    end.

%%% Tests

%% @doc Tests the resolution of a default function.
device_with_function_key_module_test() ->
	Msg =
		#{
			device => <<"Test/1.0">>
		},
	?assertEqual(
		{ok, <<"GOOD_FUNCTION">>},
		hb_converge:resolve(Msg, test_func, #{})
	).

compute_test() ->
    Msg0 = #{ device => <<"Test/1.0">> },
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
    Msg1 = #{ device => <<"Test/1.0">>, <<"Already-Seen">> => [1] },
    {ok, Msg3} = hb_converge:resolve(Msg1, restore, #{}),
    ?assertEqual([1], hb_private:get(<<"Test/Started-State">>, Msg3, #{})).