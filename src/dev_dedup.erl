%%% @moduledoc A device that deduplicates messages send to a process.
%%% Only runs on the first pass of the `compute` key call if executed
%%% in a stack. Currently the device stores its list of already seen 
%%% items in memory, but at some point it will likely make sense to 
%%% drop them in the cache.
-module(dev_dedup).
-export([init/1, compute/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

init(Msg1) ->
    {ok, Msg1#{ dedup => [] }}.

%% @doc On first pass prepare the call, on second pass get the results.
compute(M1, M2, Opts) ->
    case hb_converge:get(<<"Pass">>, M1, 1, Opts) of
        1 ->
            Msg2ID = hb_converge:get(<<"id">>, M2, Opts),
            Dedup = hb_converge:get(<<"Dedup">>, M2, [], Opts),
            ?event({dedup_checking, {existing, Dedup}}),
            case lists:member(Msg2ID, Dedup) of
                true ->
                    ?event({already_seen, Msg2ID}),
                    {skip, M1};
                false ->
                    ?event({not_seen, Msg2ID}),
                    M3 = hb_converge:set(
                        M1,
                        #{ <<"Dedup">> => [Msg2ID|Dedup] }
                    ),
                    ?event({dedup_updated, M3}),
                    {ok, M3}
            end;
        _ -> {ok, M1}
    end.

%%% Tests

%% @doc Create a device that uses a stack with an instance of this module,
%% and a `Test-Device' that is a simple device that just maintains a list
%% of the `Current-Slot` values it has already seen.
test_stack() ->
    #{
        <<"Device">> => <<"Stack/1.0">>,
        <<"Device-Stack">> =>
            [
                <<"Dedup/1.0">>,
                #{
                    <<"Compute">> =>
                        fun(Msg1, #{ <<"Message">> := Msg2 }, Opts) ->
                            {ok,
                                Msg1#{
                                    <<"Processed">> =>
                                        [
                                            Msg2
                                        |
                                            hb_converge:get(
                                                <<"Processed">>,
                                                Msg1,
                                                [],
                                                Opts
                                            )
                                        ]
                                }
                            }
                        end
                }
            ]
    }.

assignment_dedup_test() ->
    % Create a stack with a dedup device and a device that will log the
    % messages that it has been called upon.
    Msg1 = test_stack(),
    % Create 2 messages that will be processed by the stack.
    Msg2 = #{ 
        path => <<"Compute">>,
        <<"Assignment">> => #{ <<"Slot">> => 1 },
        <<"Message">> => <<"Test-Message-1">> 
    },
    Msg3 = #{ 
        path => <<"Compute">>,
        <<"Assignment">> => #{ <<"Slot">> => 2 },
        <<"Message">> => <<"Test-Message-2">> 
    },
    % Process each of the message twice through the stack. Ignore hashpath 
    % due to the explicit device being stored in the list, making a normal
    % ID generation impossible.
    {ok, Out1} = hb_converge:resolve(Msg1, Msg2, #{ hashpath => ignore }),
    {ok, Out2} = hb_converge:resolve(Out1, Msg2, #{ hashpath => ignore }),
    {ok, Out3} = hb_converge:resolve(Out2, Msg3, #{ hashpath => ignore }),
    {ok, Out4} = hb_converge:resolve(Out3, Msg3, #{ hashpath => ignore }),
    % Check that each message was seen only once.
    ?assertMatch(
        {ok, [<<"Test-Message-2">>, <<"Test-Message-1">>]},
        hb_converge:resolve(Out4, <<"Processed">>, #{})
    ).

