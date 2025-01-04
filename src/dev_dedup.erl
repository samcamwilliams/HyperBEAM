%%% @moduledoc A device that deduplicates messages send to a process.
%%% Only runs on the first pass of the `compute` key call if executed
%%% in a stack. Currently the device stores its list of already seen 
%%% items in memory, but at some point it will likely make sense to 
%%% drop them in the cache.
-module(dev_dedup).
-export([init/1, compute/3]).
-include_lib("eunit/include/eunit.hrl").

init(Msg1) ->
    {ok, Msg1#{ dedup => [] }}.

%% @doc On first pass prepare the call, on second pass get the results.
compute(M1, M2, Opts) ->
    case hb_converge:get(<<"Pass">>, M1, 1, Opts) of
        1 ->
            Msg2ID = hb_converge:get(<<"id">>, M2, Opts),
            Dedup = hb_converge:get(<<"Dedup">>, M2, Opts),
            case lists:member(Msg2ID, Dedup) of
                true -> {skip, M1};
                false ->
                    {ok,
                        hb_converge:set(
                            M1,
                            #{ <<"Dedup">> => [Msg2ID|Dedup] }
                        )
                    }
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
                    info =>
                        #{
                            handler =>
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
                }
            ]
    }.

assignment_dedup_test() ->
    Msg1 = test_stack(),
    Msg2 = #{ <<"Assignment">> => #{ <<"Slot">> => 1 },
        <<"Message">> => <<"Test-Message-1">> },
    Msg3 = #{ <<"Assignment">> => #{ <<"Slot">> => 2 },
        <<"Message">> => <<"Test-Message-2">> },
    {ok, Out1} = hb_converge:resolve(Msg1, Msg2, #{}),
    {ok, Out2} = hb_converge:resolve(Out1, Msg2, #{}),
    {ok, Out3} = hb_converge:resolve(Out2, Msg3, #{}),
    {ok, Out4} = hb_converge:resolve(Out3, Msg3, #{}),
    ?assertMatch(
        {ok, [<<"Test-Message-2">>, <<"Test-Message-1">>]},
        hb_converge:resolve(Out4, <<"Processed">>, #{})
    ).
