%%% @doc A device that triggers repass events until a certain counter has been
%%% reached. This is useful for certain types of stacks that need various
%%% execution passes to be completed in sequence across devices.
-module(dev_multipass).
-export([init/3, compute/3]).
-include_lib("eunit/include/eunit.hrl").

init(M1, _M2, _Opts) ->
    {ok, M1}.

compute(Msg1, _Msg2, Opts) ->
    Passes = hb_converge:get(<<"Passes">>, Msg1, 1, Opts),
    Pass = hb_converge:get(<<"Pass">>, Msg1, 1, Opts),
    case Pass < Passes of
        true ->
            {pass, Msg1};
        false ->
            {ok, Msg1}
    end.

%%% Tests

basic_multipass_test() ->
    Msg1 =
        #{
            <<"device">> => <<"Multipass/1.0">>,
            <<"Passes">> => 2,
            <<"Pass">> => 1
        },
    Msg2 = Msg1#{ <<"Pass">> => 2 },
    ?assertMatch({pass, _}, hb_converge:resolve(Msg1, <<"Compute">>, #{})),
    ?assertMatch({ok, _}, hb_converge:resolve(Msg2, <<"Compute">>, #{})).
