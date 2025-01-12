%%% @doc The hyperbeam meta device, which is the default entry point
%%% for all messages processed by the machine. This device executes a
%%% Converge singleton request, after first applying the node's 
%%% pre-processor, if set.
-module(dev_meta).
-export([handle/3]).
-include("include/hb.hrl").

handle(Base, Request, Opts) ->
    hb_converge:resolve(Request, Opts).