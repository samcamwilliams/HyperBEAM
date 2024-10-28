-module(dev_mu).
-export([start/1, start/2, push/2]).
-include("include/ao.hrl").
-ao_debug(print).

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
                    ao_logger:register(self()),
                    push(Item, Opts);
                false ->
                    ao_logger:log(Logger, {error, invalid_item}),
                    {error, invalid_item}
            end
        end
    ).

push(Msg, Opts = #{ results := ResTXs, logger := Logger }) ->
    % Second pass: We have the results, so we can fork the messages/spawns/...
    Res = #result{
        messages = (maps:get(<<"/Outbox">>, ResTXs, #tx{data = []}))#tx.data,
        assignments = (maps:get(<<"/Assignment">>, ResTXs, #tx{data = []}))#tx.data,
        spawns = (maps:get(<<"/Spawn">>, ResTXs, #tx{data = []}))#tx.data
    },
    ao_logger:log(Logger, {ok, computed, Msg#tx.id}),
    fork(Res, Opts),
    {ok, Opts};
push(Msg, S) ->
    % First pass: We need to verify the message and start the logger.
    case ar_bundles:verify_item(Msg) of
        true ->
            ao_logger:register(self()),
            Logger = ao_logger:start(),
            ao_logger:register(Logger),
            {ok,
                S#{
                    store => ao:get(store),
                    logger => Logger,
                    wallet => ao:wallet()
                }
            };
        false ->
            ar_bundles:print(Msg),
            {error, cannot_push_invalid_message}
    end.

%% Take a computation result and fork each message/spawn/... into its own worker.
fork(Res, Opts = #{ logger := Logger }) ->
    spawn(
        fun() ->
            lists:foreach(
                fun(Spawn) ->
                    start(Spawn, Opts)
                end,
                maybe_to_list(Res#result.spawns)
            ),
            lists:foreach(
                fun(Message) ->
                    start(Message, Opts)
                end,
                maybe_to_list(Res#result.messages)
            ),
            lists:foreach(
                fun(Assignment) ->
                    ao_logger:log(
                        Logger,
                        {ok, "Assigning ", ar_bundles:id(Assignment, unsigned)}
                    ),
                    ao_client:assign(Assignment)
                end,
                maybe_to_list(Res#result.assignments)
            )
    end).

maybe_to_list(Map) when is_map(Map) -> [V || {_K, V} <- maps:to_list(Map)];
maybe_to_list(undefined) -> [];
maybe_to_list(Else) when not is_list(Else) -> [Else];
maybe_to_list(Else) -> Else.
