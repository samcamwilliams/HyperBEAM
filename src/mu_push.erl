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
    lists:map(fun(NewItem) -> start(NewItem, Monitor) end, Items ).

schedule(Item, Monitor) ->
    case
        httpc:request(
            post,
            {ao:get(su), [], "application/x-www-form-urlencoded", ar_bundles:serialize(Item)},
            [],
            []
        ) of
            {ok, {{_, 201, _}, _, Body}} ->
                case ar_bundles:deserialize(Body, json) of
                    {error, _} ->
                        mu_push_monitor:log(Monitor, {error, assignment_format_invalid, Item}),
                        error;
                    Assignment ->
                        case ar_bundles:verify_item(Assignment) of
                            true ->
                                mu_push_monitor:log(Monitor, {ok, scheduled, Assignment}),
                                scheduled;
                            false ->
                                mu_push_monitor:log(Monitor, {error, assignment_sig_invalid, Assignment})
                        end
                end;
            Response ->
                mu_push_monitor:log(Monitor, {error, su_http_error, Response}),
                error
    end.

compute(Item, Monitor) ->
    % TN.1: MU Should be reading real results, not mocked-out.
    case
        httpc:request(ao:get(cu)
                ++ "/result/"
                %TODO: ++ binary_to_list(ar_util:encode(Item#tx.id))
                ++ "p_HXhuer1pWOzvYEn8NMRrJQEODxneu6vd1wwsPqnXo"
                ++ "?process-id="
                %TODO: ++ binary_to_list(ar_util:encode(Item#tx.target))) of
                ++ "YxpLc0rVpVUuT5KuaVnhA8X0ISCCeBShprozN6r8fKc") of
            {ok, {{_, 200, _}, _, Body}} ->
                {ResElements} = jiffy:decode(Body),
                {<<"Messages">>, Msgs} = lists:keyfind(<<"Messages">>, 1, ResElements),
                {ok,
                    lists:map(fun maybe_sign/1,
                        lists:map(fun ar_bundles:json_struct_to_item/1, Msgs)
                    )
                };
            Response ->
                mu_push_monitor:log(Monitor, {error, cu_http_error, Response}),
                error
    end.

maybe_sign(Item) ->
    %% TN.2: Will be unnecessary when CUs sign data items.
    ar_bundles:sign_item(
        Item#tx { last_tx = <<>> },
        ar_wallet:load_keyfile(ao:get(key_location))
    ).