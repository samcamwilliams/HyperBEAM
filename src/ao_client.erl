-module(ao_client).
-export([schedule/1, compute/1]).

-include("include/ar.hrl").

schedule(Item) ->
    case
        httpc:request(
            post,
            {ao:get(su), [], "application/x-www-form-urlencoded", ar_bundles:serialize(Item)},
            [],
            []
        )
    of
        {ok, {{_, 201, _}, _, Body}} ->
            case ar_bundles:deserialize(Body, json) of
                {error, _} ->
                    {error, assignment_format_invalid, Item};
                Assignment ->
                    case ar_bundles:verify_item(Assignment) of
                        true ->
                            {ok, Assignment};
                        false ->
                            {error, assignment_sig_invalid, Assignment}
                    end
            end;
        Response ->
            {error, su_http_error, Response}
    end.

compute(Item) ->
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
                {ok, lists:map(fun ar_bundles:json_struct_to_item/1, Msgs)};
            Response ->
                {error, cu_http_error, Response}
    end.