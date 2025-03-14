%%% @doc `push@1.0` takes a message or slot number, evaluates it, and recursively
%%% pushes the resulting messages to other processes. The `push'ing mechanism
%%% continues until the there are no remaining messages to push.
-module(dev_push).
%%% Public API
-export([push/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Push either a message or an assigned slot number.
push(Base, Req, Opts) ->
    ModBase = dev_process:as_process(Base, Opts),
    case hb_converge:get(<<"slot">>, {as, <<"message@1.0">>, Req}, no_slot, Opts) of
        no_slot ->
            {ok, Assignment} = initial_push(ModBase, Req, Opts),
            case find_type(hb_converge:get(<<"body">>, Assignment, Opts), Opts) of
                <<"Message">> ->
                    ?event(push, {pushing_message, {base, ModBase}, {assignment, Assignment}}, Opts),
                    push_with_mode(ModBase, Assignment, Opts);
                <<"Process">> ->
                    ?event(push, {initializing_process, {base, ModBase}, {assignment, Assignment}}, Opts),
                    {ok, Assignment}
            end;
        _ -> push_with_mode(ModBase, Req, Opts)
    end.

push_with_mode(Base, Req, Opts) ->
    Mode = is_async(Base, Req, Opts),
    case Mode of
        <<"sync">> ->
            do_push(Base, Req, Opts);
        <<"async">> ->
            spawn(fun() -> do_push(Base, Req, Opts) end)
    end.

%% @doc Determine if the push is asynchronous.
is_async(Base, Req, Opts) ->
    hb_converge:get_first(
        [
            {Req, <<"push-mode">>},
            {Base, <<"push-mode">>},
            {Base, <<"process/push-mode">>}
        ],
        <<"sync">>,
        Opts
    ).

%% @doc Push a message or slot number.
do_push(Base, Assignment, Opts) ->
    Slot = hb_converge:get(<<"slot">>, Assignment, Opts),
    ID = dev_process:process_id(Base, #{}, Opts),
    ?event(push, {push_computing_outbox, {process_id, ID}, {slot, Slot}}),
    Result = hb_converge:resolve(
        Base,
        #{ <<"path">> => <<"compute/results/outbox">>, <<"slot">> => Slot },
        Opts#{ hashpath => ignore }
    ),
    ?event(push, {push_computed, {process, ID}, {slot, Slot}}),
    case Result of
        {ok, NoResults} when ?IS_EMPTY_MESSAGE(NoResults) ->
            ?event(push, {done, {slot, Slot}}),
            {ok, #{ <<"slot">> => Slot, <<"process">> => ID }};
        {ok, Outbox} ->
            Downstream =
                maps:map(
                    fun(Key, MsgToPush) ->
                        push_result_message(Base, Slot, Key, MsgToPush, Opts)
                    end,
                    Outbox
                ),
            {ok, Downstream#{
                <<"slot">> => Slot,
                <<"process">> => ID
            }}
    end.

push_result_message(Base, FromSlot, Key, MsgToPush, Opts) ->
    case hb_converge:get(<<"target">>, MsgToPush, undefined, Opts) of
        undefined ->
            ?event(push, {skip_no_target, {key, Key}, MsgToPush}, Opts),
            #{};
        TargetID ->
            ?event(push,
                {pushing_child,
                    {originates_from_slot, FromSlot},
                    {outbox_key, Key},
                    {target_id, TargetID}
                },
                Opts
            ),
            {ok, Assignment} = schedule_result(Base, MsgToPush, Opts),
            NextSlotOnProc = hb_converge:get(<<"slot">>, Assignment, Opts),
            PushedMsgID = hb_converge:get(<<"body/id">>, Assignment, Opts),
            ?event(push,
                {push_scheduled_message,
                    {process, TargetID},
                    {slot, NextSlotOnProc},
                    {pushed_msg, PushedMsgID}
                }),
            {ok, Downstream} = hb_converge:resolve(
                TargetID,
                #{ <<"path">> => <<"push">>, <<"slot">> => NextSlotOnProc },
                Opts#{ cache_control => <<"always">> }
            ),
            #{
                <<"id">> => PushedMsgID,
                <<"target">> => TargetID,
                <<"slot">> => NextSlotOnProc,
                <<"resulted-in">> => Downstream
            }
    end.

%% @doc Augment the message with from-* keys, if it doesn't already have them.
normalize_message(MsgToPush, Opts) ->
    hb_converge:set(
        MsgToPush,
        #{
            <<"target">> => target_process(MsgToPush, Opts)
        },
        Opts#{ hashpath => ignore }
    ).

%% @doc Find the target process ID for a message to push.
target_process(MsgToPush, Opts) ->
    case hb_converge:get(<<"target">>, MsgToPush, Opts) of
        not_found -> undefined;
        RawTarget -> extract(target, RawTarget)
    end.

%% @doc Return either the `target' or the `hint'.
extract(hint, Raw) ->
    {_, Hint} = split_target(Raw),
    Hint;
extract(target, Raw) ->
    {Target, _} = split_target(Raw),
    Target.

split_target(RawTarget) ->
    case binary:split(RawTarget, [<<"?">>, <<"&">>]) of
        [Target, QStr] -> {Target, QStr};
        _ -> {RawTarget, <<>>}
    end.

schedule_result(Base, MsgToPush, Opts) ->
    ?event(push,
        {push_scheduling_result,
            {target, {explicit, Base}},
            {msg, MsgToPush}},
        Opts
    ),
    SignedReq =
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> =>
                hb_message:attest(
                    MsgToPush,
                    Opts
                )
        },
    {ErlStatus, Res} =
        hb_converge:resolve(
            Base,
            SignedReq,
            Opts#{ cache_control => <<"always">> }
        ),
    ?event(push, {push_scheduling_result, {status, ErlStatus}, {response, Res}}, Opts),
    case {ErlStatus, hb_converge:get(<<"status">>, Res, 200, Opts)} of
        {ok, 200} -> {ok, Res};
        {ok, 307} ->
            Location = hb_converge:get(<<"location">>, Res, Opts),
            ?event(push, {redirect, {location, {explicit, Location}}}),
            NormMsg = normalize_message(MsgToPush, Opts),
            SignedNormMsg = hb_message:attest(NormMsg, Opts),
            remote_schedule_result(Location, SignedNormMsg, Opts);
        {error, _} ->
            {error, Res}
    end.

%% @doc Push a message or a process, prior to pushing the resulting slot number.
initial_push(Base, Req, Opts) ->
    ModReq = Req#{ <<"path">> => <<"schedule">>, <<"method">> => <<"POST">> },
    ?event(push, {initial_push, {base, Base}, {req, ModReq}}, Opts),
    case hb_converge:resolve(Base, ModReq, Opts) of
        {ok, Res} ->
            case hb_converge:get(<<"status">>, Res, 200, Opts) of
                200 -> {ok, Res};
                307 ->
                    Location = hb_converge:get(<<"location">>, Res, Opts),
                    remote_schedule_result(Location, Req, Opts)
            end;
        {error, Res} ->
            {error, Res}
    end.

remote_schedule_result(Location, SignedReq, Opts) ->
    ?event(push, {remote_schedule_result, {location, Location}, {req, SignedReq}}, Opts),
    {Node, RedirectPath} = parse_redirect(Location),
    Path =
        case find_type(SignedReq, Opts) of
            <<"Process">> -> <<"/schedule">>;
            <<"Message">> -> RedirectPath
        end,
    % Store a copy of the message for ourselves.
    hb_cache:write(SignedReq, Opts),
    ?event(push, {remote_schedule_result, {path, Path}}, Opts),
    case hb_http:post(Node, Path, maps:without([<<"path">>], SignedReq), Opts) of
        {ok, Res} ->
            ?event(push, {remote_schedule_result, {res, Res}}, Opts),
            case hb_converge:get(<<"status">>, Res, 200, Opts) of
                200 -> {ok, Res};
                307 ->
                    NewLocation = hb_converge:get(<<"location">>, Res, Opts),
                    remote_schedule_result(NewLocation, SignedReq, Opts)
            end;
        {error, Res} ->
            {error, Res}
    end.

find_type(Req, Opts) ->
    hb_converge:get_first(
        [
            {Req, <<"type">>},
            {Req, <<"body/type">>}
        ],
        Opts
    ).

parse_redirect(Location) ->
    Parsed = uri_string:parse(Location),
    Node =
        uri_string:recompose(
            (maps:remove(query, Parsed))#{
                path => <<"/schedule">>
            }
        ),
    {Node, maps:get(path, Parsed)}.

%%% Tests

full_push_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Opts = #{ priv_wallet => hb:wallet() },
        Msg1 = dev_process:test_aos_process(),
        ?event(push, {msg1, Msg1}),
        Script = ping_pong_script(2),
        ?event({script, Script}),
        {ok, Msg2} = dev_process:schedule_aos_call(Msg1, Script),
        ?event(push, {init_sched_result, Msg2}),
        {ok, StartingMsgSlot} =
            hb_converge:resolve(Msg2, #{ <<"path">> => <<"slot">> }, Opts),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => StartingMsgSlot
            },
        {ok, _} = hb_converge:resolve(Msg1, Msg3, Opts),
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_converge:resolve(Msg1, <<"now/results/data">>, Opts)
        )
    end}.

multi_process_push_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Opts = #{
            priv_wallet => hb:wallet(),
            cache_control => <<"always">>
        },
        Proc1 = dev_process:test_aos_process(),
        {ok, _} = dev_process:schedule_aos_call(Proc1, reply_script()),
        Proc2 = dev_process:test_aos_process(),
        ProcID1 =
            hb_converge:get(
                <<"process/id">>,
                dev_process:ensure_process_key(Proc1, Opts),
                Opts
            ),
        ProcID2 =
            hb_converge:get(
                <<"process/id">>,
                dev_process:ensure_process_key(Proc2, Opts),
                Opts
            ),
        ?event(push, {testing_with, {proc1_id, ProcID1}, {proc2_id, ProcID2}}),
        {ok, ToPush} = dev_process:schedule_aos_call(
            Proc2,
            <<
                "Handlers.add(\"Pong\",\n"
                "   function (test) return true end,\n"
                "   function(m)\n"
                "       print(\"GOT PONG\")\n"
                "   end\n"
                ")\n"
                "Send({ Target = \"", (ProcID1)/binary, "\", Action = \"Ping\" })\n"
            >>
        ),
        SlotToPush = hb_converge:get(<<"slot">>, ToPush, Opts),
        ?event(push, {slot_to_push_proc2, SlotToPush}),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => SlotToPush
            },
        {ok, PushResult} = hb_converge:resolve(Proc2, Msg3, Opts),
        ?event(push, {push_result_proc2, PushResult}),
        AfterPush = hb_converge:resolve(Proc2, <<"now/results/data">>, Opts),
        ?event(push, {after_push, AfterPush}),
        ?assertEqual({ok, <<"GOT PONG">>}, AfterPush)
    end}.

push_with_redirect_hint_test_disabled() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Stores = [{hb_store_fs, #{ prefix => "TEST-cache" }}],
        ExtOpts = #{ priv_wallet => ar_wallet:new(), store => Stores },
        LocalOpts = #{ priv_wallet => hb:wallet(), store => Stores },
        ExtScheduler = hb_http_server:start_node(ExtOpts),
        ?event(push, {external_scheduler, {location, ExtScheduler}}),
        % Create the Pong server and client
        Client = dev_process:test_aos_process(),
        PongServer = dev_process:test_aos_process(ExtOpts),
        % Push the new process that runs on the external scheduler
        {ok, ServerSchedResp} = hb_http:post(ExtScheduler, <<"/push">>, PongServer, ExtOpts),
        ?event(push, {pong_server_sched_resp, ServerSchedResp}),
        % Get the IDs of the server process
        PongServerID =
            hb_converge:get(
                <<"process/id">>,
                dev_process:ensure_process_key(PongServer, LocalOpts),
                LocalOpts
            ),
        {ok, ServerScriptSchedResp} =
            hb_http:post(
                ExtScheduler,
                <<PongServerID/binary, "/push">>,
                #{
                    <<"body">> =>
                        hb_message:attest(
                            #{
                                <<"target">> => PongServerID,
                                <<"action">> => <<"Eval">>,
                                <<"type">> => <<"Message">>,
                                <<"data">> => reply_script()
                            },
                            ExtOpts
                        )
                },
                ExtOpts
            ),
        ?event(push, {pong_server_script_sched_resp, ServerScriptSchedResp}),
        {ok, ToPush} =
            dev_process:schedule_aos_call(
                Client,
                <<
                    "Handlers.add(\"Pong\",\n"
                    "   function (test) return true end,\n"
                    "   function(m)\n"
                    "       print(\"GOT PONG\")\n"
                    "   end\n"
                    ")\n"
                    "Send({ Target = \"",
                        (PongServerID)/binary, "?hint=",
                        (ExtScheduler)/binary,
                    "\", Action = \"Ping\" })\n"
                >>,
                LocalOpts
            ),
        SlotToPush = hb_converge:get(<<"slot">>, ToPush, LocalOpts),
        ?event(push, {slot_to_push_client, SlotToPush}),
        Msg3 = #{ <<"path">> => <<"push">>, <<"slot">> => SlotToPush },
        {ok, PushResult} = hb_converge:resolve(Client, Msg3, LocalOpts),
        ?event(push, {push_result_client, PushResult}),
        AfterPush = hb_converge:resolve(Client, <<"now/results/data">>, LocalOpts),
        ?event(push, {after_push, AfterPush}),
        % Note: This test currently only gets a reply that the message was not
        % trusted by the process. To fix this, we would have to add another 
        % trusted authority to the `test_aos_process' call. For now, this is 
        % enough to validate that redirects are pushed through correctly.
        ?assertEqual({ok, <<"GOT PONG">>}, AfterPush)
    end}.

%%% Test helpers

ping_pong_script(Limit) ->
    <<
        "Handlers.add(\"Ping\",\n"
        "   function (test) return true end,\n"
        "   function(m)\n"
        "       C = tonumber(m.Count)\n"
        "       if C <= ", (integer_to_binary(Limit))/binary, " then\n"
        "           Send({ Target = ao.id, Action = \"Ping\", Count = C + 1 })\n"
        "           print(\"Ping\", C + 1)\n"
        "       else\n"
        "           print(\"Done.\")\n"
        "       end\n"
        "   end\n"
        ")\n"
        "Send({ Target = ao.id, Action = \"Ping\", Count = 1 })\n"
    >>.

reply_script() ->
    <<
        "Handlers.add(\"Reply\",\n"
        "   function (test) return true end,\n"
        "   function(m)\n"
        "       print(\"Replying to...\")\n"
        "       print(m.From)\n"
        "       Send({ Target = m.From, Action = \"Reply\", Message = \"Pong!\" })\n"
        "       print(\"Done.\")\n"
        "   end\n"
        ")\n"
    >>.