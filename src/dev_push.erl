%%% @doc `push@1.0` takes a message or slot number, evaluates it, and recursively
%%% pushes the resulting messages to other processes. The `push'ing mechanism
%%% continues until the there are no remaining messages to push.
-module(dev_push).
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
    ?event(push, {push_got_outbox, {slot, Slot}, {outbox, Result}}),
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
    case hb_converge:get(<<"target">>, MsgToPush, Opts) of
        not_found ->
            ?event(push, {skip_no_target, {key, Key}, MsgToPush}),
            #{};
        Target ->
            ?event(push, {pushing_child, {originates_from_slot, FromSlot}, {outbox_key, Key}}),
            Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
            Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
            ?event(push,
                {pushing_child,
                    {originates_from_slot, FromSlot},
                    {outbox_key, Key},
                    {push_address, Address}
                }
            ),
            {ok, NextSlotOnProc} = hb_converge:resolve(
                Base,
                #{
                    <<"method">> => <<"POST">>,
                    <<"path">> => <<"schedule/slot">>,
                    <<"body">> =>
                        PushedMsg = hb_message:attest(
                            MsgToPush,
                            Opts
                        )
                },
                Opts
            ),
            PushedMsgID = hb_message:id(PushedMsg, all),
            ?event(push,
                {push_scheduled,
                    {process, Target},
                    {assigned_slot, NextSlotOnProc},
                    {pushed_msg, PushedMsg}
                }),
            {ok, Downstream} = hb_converge:resolve(
                Base,
                #{ <<"path">> => <<"push">>, <<"slot">> => NextSlotOnProc },
                Opts
            ),
            #{
                <<"id">> => PushedMsgID,
                <<"target">> => Target,
                <<"slot">> => NextSlotOnProc,
                <<"resulted-in">> => Downstream
            }
    end.

schedule_result(Msg, Opts) ->
    {ok, PushRes} = hb_converge:resolve(
        Msg,
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> =>
                PushedMsg = hb_message:attest(
                    todo,
                    Opts
                )
        },
        Opts
    ),
    NextSlotOnProc =
        case PushRes of
            {ok, #{ <<"slot">> := NextSlot}} ->
                PushedMsgID = hb_message:id(PushedMsg, all),
                ?event(push,
                    {push_scheduled,
                    {assigned_slot, NextSlot},
                    {pushed_msg, PushedMsgID}
                }),
                NextSlot;
            {ok, #{ <<"status">> := 307 }} ->
                not_found
        end,
    {ok, NextSlotOnProc}.

%%% Tests

full_push_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Opts = #{ priv_wallet => hb:wallet() },
        Msg1 = dev_process:test_aos_process(),
        ?event(push, {msg1, Msg1}),
        Script = dev_process:ping_ping_script(2),
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