-module(dev_multipass).
-export([init/2, execute/2, uses/0]).

%%% A device that triggers repass events until a certain counter has been reached.
%%% This is useful for certain types of stacks that need various execution passes
%%% to be completed in sequence across devices.

init(S, Params) ->
    {<<"Passes">>, Passes} = lists:keyfind(<<"Passes">>, 1, Params),
    {ok, S#{ pass => 1, passes => binary_to_integer(Passes) }}.

execute(_, S = #{ pass := Pass, passes := Passes }) when Pass < Passes ->
    {pass, S};
execute(_, S) ->
    {ok, S}.

uses() -> all.