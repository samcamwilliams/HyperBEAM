-module(mu_push).
-export([start/1, start/2]).

-include("include/ar.hrl").

start(Item) -> start(Item, ao_logger:start()).

start(Item, Monitor) ->
    ao_logger:log(Monitor, {ok, start, Item}),
    case ar_bundles:verify_item(Item) of
        true ->
            spawn(
                fun() ->
                    ao_logger:register(self()),
                    push(Item, Monitor)
                end),
            Monitor;
        false -> {error, invalid_item}
    end.

push(Item, Monitor) ->
    case ao_client:schedule(Item) of
        {ok, Assignment} ->
            ao_logger:log(Monitor, {ok, scheduled, Assignment}),
            case ao_client:compute(Assignment) of
                {ok, Items} ->
                    ao_logger:log(Monitor, {ok, computed, Items}),
                    lists:map(
                        fun(NewItem) -> start(maybe_sign(NewItem), Monitor) end,
                        Items
                    );
                Error -> ao_logger:log(Monitor, Error)
            end;
        Error -> ao_logger:log(Monitor, Error)
    end.

maybe_sign(Item) ->
    %% TN.2: Will be unnecessary when CUs sign data items.
    ar_bundles:sign_item(
        Item#tx { last_tx = <<>> },
        ar_wallet:load_keyfile(ao:get(key_location))
    ).