-module(dev_mu).
-export([start/1, start/2]).
-export([push/2]).
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
                    exec(Item, Opts);
                false ->
                    ao_logger:log(Logger, {error, invalid_item}),
                    {error, invalid_item}
            end
        end
    ).

exec(Item, Opts) ->
    Stack = init_devices(Item),
    {ok, _Res} = run_stack(Stack, Item, Opts),
    {ok, Opts}.

%% Return a normalized device stack for pushing the given item.
init_devices(_Item) ->
    % TODO: We may want to make this respond flexibility to both the item
    % and the configuration of the node. Node operators should set a list
    % of admissible devices for execution during pushing.
    dev_stack:normalize(ao:get(default_mu_stack)).

run_stack(Stack, Item, Opts = #{ logger := Logger }) ->
    InitState = Opts#{
        devices => Stack,
        logger => Logger,
        item => Item
    },
    dev_stack:execute(InitState, push, #{ arg_prefix => [Item] }).

push(Item, Opts = #{ results := ResTXs, logger := Logger }) ->
    throw(stop),
    Res = #result{
        messages = (maps:get(<<"/Outbox">>, ResTXs, #tx{data = []}))#tx.data,
        assignments = (maps:get(<<"/Assignment">>, ResTXs, #tx{data = []}))#tx.data,
        spawns = (maps:get(<<"/Spawn">>, ResTXs, #tx{data = []}))#tx.data
    },
    ao_logger:log(Logger, {ok, computed, Item#tx.id}),
    fork(Res, Opts),
    {ok, Opts}.

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
