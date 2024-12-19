
%%% @doc A long-lived process worker that keeps state in memory between
%%% calls. Implements the interface of `hb_converge' to receive and respond 
%%% to computation requests regarding a process as a singleton.
-module(dev_process_worker).
-export([server/3, stop/1, group/3]).

%% @doc Returns a group name for a request. The worker is responsible for all
%% computation work on the same process on a single node, so we use the
%% process ID as the group name.
group(Msg1, _Msg2, Opts) ->
    Proc = hb_converge:get(<<"Process">>, {as, dev_message, Msg1}, Opts),
    hb_util:human_id(hb_converge:get(id, Proc, Opts#{ hashpath := ignore})).

%% @doc Spawn a new worker process. This is called after the end of the first
%% execution of `hb_converge:resolve/3', so the state we are given is the
%% already current.
server(Msg1, _FirstMsg, Opts) ->
    receive
        {resolve, Listener, Msg1, Msg2, ListenerOpts} ->
            Res =
                hb_converge:resolve(
                    Msg1,
                    Msg2,
                    ListenerOpts#{ spawn_worker => false }
                ),
            Listener ! {resolved, self(), Msg1, Msg2, Res},
            server(Msg1, Msg2, Opts);
        stop -> ok
    end.

%% @doc Stop a worker process.
stop(Worker) ->
    exit(Worker, normal).

