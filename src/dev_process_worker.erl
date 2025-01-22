
%%% @doc A long-lived process worker that keeps state in memory between
%%% calls. Implements the interface of `hb_converge' to receive and respond 
%%% to computation requests regarding a process as a singleton.
-module(dev_process_worker).
-export([server/3, stop/1, group/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Returns a group name for a request. The worker is responsible for all
%% computation work on the same process on a single node, so we use the
%% process ID as the group name.
group(Msg1, undefined, Opts) ->
    hb_persistent:default_grouper(Msg1, undefined, Opts);
group(Msg1, Msg2, Opts) ->
    case hb_path:matches(<<"compute">>, hb_path:hd(Msg2, Opts)) of
        true ->
            process_to_group_name(Msg1, Opts);
        _ ->
            hb_persistent:default_grouper(Msg1, Msg2, Opts)
    end.

process_to_group_name(Msg1, Opts) ->
    hb_util:human_id(
        hb_converge:get(
            <<"process/id">>,
            {as,
                dev_message,
                dev_process:ensure_process_key(Msg1, Opts)
            },
            Opts#{ hashpath => ignore }
        )
    ).

%% @doc Spawn a new worker process. This is called after the end of the first
%% execution of `hb_converge:resolve/3', so the state we are given is the
%% already current.
server(GroupName, Msg1, Opts) ->
    hb_persistent:default_worker(GroupName, Msg1, Opts#{ static_worker => true }).

%% @doc Stop a worker process.
stop(Worker) ->
    exit(Worker, normal).

%%% Tests

test_init() ->
    application:ensure_all_started(hb),
    ok.

info_test() ->
    test_init(),
    M1 = dev_process:test_wasm_process(<<"test/aos-2-pure-xs.wasm">>),
    Res = hb_converge:info(M1, #{}),
    ?assertEqual(fun dev_process_worker:group/3, maps:get(grouper, Res)).

grouper_test() ->
    test_init(),
    M1 = dev_process:test_aos_process(),
    M2 = #{ <<"path">> => <<"compute">>, <<"v">> => 1 },
    M3 = #{ <<"path">> => <<"compute">>, <<"v">> => 2 },
    M4 = #{ <<"path">> => <<"not-compute">>, <<"v">> => 3 },
    G1 = hb_persistent:group(M1, M2, #{}),
    G2 = hb_persistent:group(M1, M3, #{}),
    G3 = hb_persistent:group(M1, M4, #{}),
    ?event({group_samples, {g1, G1}, {g2, G2}, {g3, G3}}),
    ?assertEqual(G1, G2),
    ?assertNotEqual(G1, G3).