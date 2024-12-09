-module(dev_test).
-export([info/1, test_func/1]).
-include_lib("eunit/include/eunit.hrl").

%%% A simple test device for Converge, so that we can test the functionality that
%%% depends on using Erlang's module system.

%% @doc Exports a default_handler function that can be used to test the
%% handler resolution mechanism.
info(_) ->
	#{
		exports => [test_func]
	}.

test_func(_) ->
	{ok, <<"GOOD_FUNCTION">>}.

%% @doc Tests the resolution of a default function.
device_with_function_key_module_test() ->
	Msg =
		#{
			device => <<"Test/1.0">>
		},
	?assertEqual(
		{ok, <<"GOOD_FUNCTION">>},
		hb_converge:resolve(Msg, test_func)
	).
