-module(mu_push).
-export([start/1, start/2]).

-include("include/ar.hrl").

start(Item) ->
    start(Item, mu_push_monitor:start()).

start(Item, Monitor) ->
    mu_push_monitor:log(Monitor, {ok, start, Item}),
    case ar_bundles:verify_item(Item) of
        true ->
            spawn(
                fun() ->
                    mu_push_monitor:register(self()),
                    push(Item, Monitor)
                end);
        false -> {error, invalid_item}
    end.

push(Item, Monitor) ->
    scheduled = schedule(Item, Monitor),
    {ok, Items} = compute(Item, Monitor),
    mu_push_monitor:log(Monitor, {ok, computed, Items}),
    lists:map(
        fun(NewItem) ->
            SignedItem = case NewItem#tx.id of
                <<>> ->
                    mu_push_monitor:log(Monitor, {ok, signing, NewItem}),
                    ar_bundles:sign(NewItem);
                _ ->
                    NewItem
            end,
            start(SignedItem, Monitor)
        end,
        Items
    ).

schedule(Item, Monitor) ->
    case
        httpc:request(
            post,
            {ao:get(su), [], "application/json", ar_bundles:serialize(Item)},
            [],
            []
        ) of
            {ok, {{_, 200, _}, _, Body}} ->
                case ar_bundles:deserialize(Body) of
                    {error, _} ->
                        mu_push_monitor:log(Monitor, {error, assignment_invalid, Item}),
                        error;
                    Assignment ->
                        mu_push_monitor:log(Monitor, {ok, scheduled, Assignment}),
                        scheduled
                end;
            Response ->
                mu_push_monitor:log(Monitor, {error, su_http_error, Response}),
                error
    end.

compute(Item, Monitor) ->
    case
        httpc:request(ao:get(cu) ++ "/result/" ++ binary_to_list(Item#tx.id) ++ "?process-id=" ++ binary_to_list(Item#tx.target)) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ResElements} = jiffy:decode(Body),
                {<<"Messages">>, Msgs} = lists:keyfind(<<"Messages">>, 1, ResElements),
                {ok, lists:map(fun ar_bundles:json_struct_to_item/1, Msgs)};
            Response ->
                mu_push_monitor:log(Monitor, {error, cu_http_error, Response}),
                error
    end.