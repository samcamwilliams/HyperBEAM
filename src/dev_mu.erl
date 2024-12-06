-module(dev_mu).
-export([push/2]).
-include("include/hb.hrl").

%%% The main pushing logic for messages around the system.s

%% We should run the following device stack on the message:
%% dev_scheduler -> dev_cu -> dev_poda
%% After execution we take the result and fork again based on it.
-define(PUSH_DEV_STACK, [dev_scheduler, dev_cu, dev_poda]).

%% @doc The main entry point for pushing a message. Assumes the message is
%% a carrier message, and will extract the carried message to push from it.
push(CarrierMsg, State) ->
    % First pass: We need to verify the message and start the logger.
    Msg =
        case CarrierMsg#tx.data of
            #{ <<"1">> := CarriedMsg } ->
                CarriedMsg;
            _ -> CarrierMsg
        end,
    ?event({starting_push_for,
        {unsigned, hb_util:id(Msg, unsigned)},
        {signed, hb_util:id(Msg, signed)},
        {target, hb_util:id(Msg#tx.target)}
    }),
    ?no_prod(fix_mu_push_validation),
    case ar_bundles:verify_item(Msg) of
        _ ->
            Logger =
                case maps:get(logger, State, undefined) of
                    undefined -> hb_logger:start();
                    X -> X
                end,
            hb_logger:register(Logger),
            fork(
                #result {
                    messages = [Msg]
                },
                State#{
                    depth => 0,
                    store => maps:get(store, State, hb_opts:get(store)),
                    logger => Logger,
                    wallet => maps:get(wallet, State, hb:wallet())
                }
            ),
            % TODO: Implement trace waiting.
            ResTX = ar_bundles:sign_item(
                #tx{ tags = [{<<"Status">>, <<"200">>}]},
                hb:wallet()),
            {ok, #{ results => ResTX }}
        %false ->
        %    {error, cannot_push_invalid_message}
    end.

%% Take a computation result and fork each message/spawn/... into its own worker.
fork(Res, Opts) ->
    push_messages(upload, Res#result.spawns, Opts),
    push_messages(upload, Res#result.messages, Opts),
    push_messages(attest, Res#result.assignments, Opts).

push_messages(upload, Messages, Opts) ->
    lists:foreach(
        fun(Message) ->
            spawn(
                fun() ->
                    ?event(
                        {mu_forking_for,
                            {unsigned, hb_util:id(Message, unsigned)},
                            {signed, hb_util:id(Message, signed)},
                            {target, hb_util:id(Message#tx.target)},
                            {logger, maps:get(logger, Opts, undefined)}
                        }
                    ),
                    Stack = dev_stack:create(?PUSH_DEV_STACK),
                    {ok, Results} = hb_pam:resolve(
                        {dev_stack, execute},
                        push,
                        [
                            #{
                                devices => Stack,
                                message => Message,
                                logger => maps:get(logger, Opts, undefined),
                                store => maps:get(store, Opts, hb_opts:get(store)),
                                wallet => maps:get(wallet, Opts, hb:wallet())
                            }
                        ]
                    ),
                    ?event({pushing_result_for_computed_message,
                        {unsigned, hb_util:id(Message, unsigned)},
                        {signed, hb_util:id(Message, signed)},
                        {target, hb_util:id(Message#tx.target)}
                    }),
                    handle_push_result(Results, Opts)
                end
            )
        end,
        maybe_to_list(Messages)
    );
push_messages(attest, Assignments, #{ logger := Logger }) ->
    lists:foreach(
        fun(Assignment) ->
            hb_logger:log(Logger, {ok, "Assigning ", ar_bundles:id(Assignment, signed)}),
            hb_client:assign(Assignment),
            ?no_prod("After assigning, don't we want to push the message?")
        end,
        maybe_to_list(Assignments)
    ).

handle_push_result(Results, Opts = #{ depth := Depth }) ->
    % Second pass: We have the results, so we can fork the messages/spawns/...
    Res = #result{
        messages = maps:get(<<"/Outbox">>, Results, #{}),
        assignments = maps:get(<<"/Assignment">>, Results, #{}),
        spawns = maps:get(<<"/Spawn">>, Results, #{})
    },
    ?event({push_recursing,
        {depth, Depth},
        {messages, maps:size(Res#result.messages)},
        {assignments, maps:size(Res#result.assignments)},
        {spawns, maps:size(Res#result.spawns)}
    }),
    fork(Res, Opts#{ depth => Depth + 1 }).

maybe_to_list(Map) when is_map(Map) -> [V || {_K, V} <- maps:to_list(Map)];
maybe_to_list(undefined) -> [];
maybe_to_list(Else) when not is_list(Else) -> [Else];
maybe_to_list(Else) -> Else.