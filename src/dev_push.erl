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
    Mode = is_async(Base, Req, Opts),
    ModBase = dev_process:as_process(Base, Opts),
    ToPush =
        case hb_converge:get(<<"type">>, Base, Opts) of
            Name when Name == <<"message">>; Name == <<"process">> ->
                case hb_converge:resolve(Base, Req, Opts) of
                    {ok, Assignment} ->
                        Assignment;
                    {error, _} ->
                        {error, <<"Could not schedule initial message.">>}
                end;
            _ -> Req
        end,
    case Mode of
        <<"sync">> ->
            do_push(ModBase, ToPush, Opts);
        <<"async">> ->
            spawn(fun() -> do_push(ModBase, ToPush, Opts) end)
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
    ID = hb_converge:get(<<"process/id">>, Base, Opts),
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
            {ok, #{}};
        {ok, Outbox} ->
            Downstream =
                maps:map(
                    fun(Key, MsgToPush) ->
                        push_result_message(Base, Slot, Key, MsgToPush, Opts)
                    end,
                    Outbox
                ),
            {ok, Downstream}
    end.

push_result_message(Base, FromSlot, Key, MsgToPush, Opts) ->
    case target_process(MsgToPush, Opts) of
        no_target ->
            ?event(push, {skip_no_target, {key, Key}, MsgToPush}),
            #{};
        TargetID ->
            ?event(push,
                {pushing_child,
                    {originates_from_slot, FromSlot},
                    {outbox_key, Key},
                    {target_id, TargetID}
                }
            ),
            {ok, PushedMsgID, NextSlotOnProc} =
                schedule_result(Base, TargetID, MsgToPush, Opts),
            ?event(push,
                {push_scheduled,
                    {process, TargetID},
                    {assigned_slot, NextSlotOnProc},
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

%% @doc Find the target process ID for a message to push.
target_process(MsgToPush, Opts) ->
    case hb_converge:get(<<"target">>, MsgToPush, Opts) of
        not_found -> no_target;
        RawTarget ->
            case binary:split(RawTarget, [<<"?">>, <<"&">>], [global]) of
                [Target|_] -> Target;
                _ -> RawTarget
            end
    end.

schedule_result(FromProc, TargetProc, MsgToPush, Opts) ->
    ?event(push, {push_scheduling_result, {target, TargetProc}, {msg, MsgToPush}}),
    Res = hb_converge:resolve_many(
        [
            TargetProc,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> =>
                    PushedMsg = hb_message:attest(
                        MsgToPush,
                        Opts
                    )
            }
        ],
        Opts#{ cache_control => <<"always">> }
    ),
    case Res of
        {ok, #{ <<"slot">> := Slot}} ->
            PushedMsgID = hb_message:id(PushedMsg, all),
            {ok, PushedMsgID, Slot};
        {ok, #{ <<"status">> := 307 }} ->
            {error, <<"Received redirect response from scheduler.">>};
        {error, Error} ->
            {error, Error}
    end.

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

push_with_redirect_hint_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        SchedOpts = #{ priv_wallet => ar_wallet:new() },
        ExtScheduler = hb_http_server:start_node(SchedOpts),
        ?event(push, {external_scheduler, {location, ExtScheduler}, {opts, SchedOpts}}),
        Opts = #{ priv_wallet => hb:wallet() },
        % Setup the Pong server
        Client = dev_process:test_aos_process(),
        PongServer = dev_process:test_aos_process(Opts),
        PongServerID =
            hb_converge:get(
                <<"process/id">>,
                dev_process:ensure_process_key(PongServer, Opts),
                Opts
            ),
        % Setup the processes
        PongServerScriptMsg =
            hb_message:attest(
                #{
                    <<"path">> => <<"schedule">>,
                    <<"body">> =>
                        hb_message:attest(
                            #{
                                <<"target">> => PongServerID,
                                <<"action">> => <<"Eval">>,
                                <<"type">> => <<"Message">>,
                                <<"data">> => reply_script()
                            },
                            SchedOpts
                        )
                },
                SchedOpts
            ),
        ?event(push, {pong_server_script_msg, PongServerScriptMsg}),
        {ok, ServerScriptSchedResp} =
            hb_converge:resolve(
                PongServer,
                PongServerScriptMsg,
                SchedOpts
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
                >>
            ),
        SlotToPush = hb_converge:get(<<"slot">>, ToPush, Opts),
        ?event(push, {slot_to_push_client, SlotToPush}),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => SlotToPush
            },
        {ok, PushResult} = hb_converge:resolve(Client, Msg3, Opts),
        ?event(push, {push_result_client, PushResult}),
        AfterPush = hb_converge:resolve(Client, <<"now/results/data">>, Opts),
        ?event(push, {after_push, AfterPush}),
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