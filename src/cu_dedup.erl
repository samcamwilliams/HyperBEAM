-module(cu_policy_dedupe).
-export([init/1, apply/3]).

-record(opts, {
    cron = false, % False or a number of milliseconds.
    last_slot = -1 % The last slot we have processed already.
    last_time = -1 % The last time we updated cron.
}).

init(Process) ->
    Process#tx.id,