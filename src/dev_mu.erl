-module(dev_mu).
-export([start/1, start/2, push/2]).
-include("include/ao.hrl").

start(Item) ->
    start(
        Item,
        #{
            store => ao:get(store),
            logger => ao_logger:start(),
            wallet => ao:wallet()
        }
    ).
start(Item, Opts = #{ logger := Logger }) ->
    spawn(
        fun() ->
            ao_logger:log(Logger, {ok, start_push, Item}),
            case ar_bundles:verify_item(Item) of
                true ->
                    % TODO: This is not efficient...
                    push(ar_bundles:deserialize(ar_bundles:serialize(Item)), Opts);
                false ->
                    ao_logger:log(Logger, {error, invalid_item}),
                    {error, invalid_item}
            end
        end
    ),
    {ok, defined}.

push(Msg, State = #{ results := ResTXs, logger := Logger }) ->
    % Second pass: We have the results, so we can fork the messages/spawns/...
    Res = #result{
        messages = maps:get(<<"/Outbox">>, ResTXs, #{}),
        assignments = maps:get(<<"/Assignment">>, ResTXs, #{}),
        spawns = maps:get(<<"/Spawn">>, ResTXs, #{})
    },
    ao_logger:log(Logger, {ok, computed, ar_util:id(Msg, unsigned)}),
    fork(Res, maps:remove(results, State)),
    {ok, State};
push(CarrierMsg, State) ->
    % First pass: We need to verify the message and start the logger.
    Msg =
        case CarrierMsg#tx.data of
            #{ <<"1">> := CarriedMsg } ->
                CarriedMsg;
            _ -> CarrierMsg
        end,
    ?no_prod(fix_mu_push_validation),
    %case ar_bundles:verify_item(Msg) of
    case true of
        true ->
            Logger =
                case maps:get(logger, State, undefined) of
                    undefined -> ao_logger:start();
                    X -> X
                end,
            ao_logger:register(Logger),
            {ok,
                State#{
                    store => maps:get(store, State, ao:get(store)),
                    logger => Logger,
                    wallet => maps:get(wallet, State, ao:wallet())
                }
            };
        false ->
            {error, cannot_push_invalid_message}
    end.

%% Take a computation result and fork each message/spawn/... into its own worker.
fork(Res, Opts = #{ logger := Logger }) ->
    fork_items(Res#result.spawns, Opts),
    fork_items(Res#result.messages, Opts),
    lists:foreach(
        fun(Assignment) ->
            ao_logger:log(Logger, {ok, "Assigning ", ar_bundles:id(Assignment, unsigned)}),
            ao_client:assign(Assignment)
        end,
        maybe_to_list(Res#result.assignments)
    ).

fork_items(Items, Opts) ->
    % TODO: We should definitely not be using the HTTP interface for this!
    lists:foreach(
        fun(Item) ->
            spawn(
                fun() ->
                    cu_device:call(
                        dev_meta,
                        execute,
                        [
                            ar_bundles:sign_item(#tx {
                                tags = [
                                    {<<"Method">>, <<"POST">>},
                                    {<<"Path">>, <<"/mu/">>},
                                    {<<"Trace">>, <<"none">>}
                                ],
                                data = #{ <<"1">> => Item }
                            }, ao:wallet())
                        ],
                        Opts
                    )
                end
            )
        end,
        maybe_to_list(Items)
    ).

maybe_to_list(Map) when is_map(Map) -> [V || {_K, V} <- maps:to_list(Map)];
maybe_to_list(undefined) -> [];
maybe_to_list(Else) when not is_list(Else) -> [Else];
maybe_to_list(Else) -> Else.
