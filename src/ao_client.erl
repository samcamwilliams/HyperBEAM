-module(ao_client).
-export([schedule/1, assign/1]).
-export([compute/1, cron/1, cron/2, cron/3, cron_cursor/1]).
-export([push/1]).
-export([arweave_timestamp/0]).

-include("include/ao.hrl").

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

assign(_ID) ->
    ao:c({not_implemented, assignments}).

compute(_Item) ->
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
                {ok, parse_result(Body)};
            Response ->
                {error, cu_http_error, Response}
    end.

push(Item) ->
    case
        httpc:request(
            post,
            {ao:get(mu) ++ "/", [], "application/x-www-form-urlencoded", ar_bundles:serialize(Item)},
            [],
            []
        )
    of
        {ok, {{_, 201, _}, _, _Body}} -> ok;
        Response -> {error, mu_http_error, Response}
    end.

cron(ProcID) ->
    cron(ProcID, cron_cursor(ProcID)).
cron(ProcID, Cursor) ->
    cron(ProcID, Cursor, ao:get(default_page_limit)).
cron(ProcID, Cursor, Limit) when is_binary(ProcID) ->
    cron(binary_to_list(ar_util:encode(ProcID)), Cursor, Limit);
cron(ProcID, undefined, RawLimit) ->
    cron(ProcID, cron_cursor(ProcID), RawLimit);
cron(ProcID, Cursor, Limit) ->
    case
        httpc:request(
            ao:get(cu)
                ++ "/cron/"
                ++ ProcID
                ++ "?cursor="
                ++ binary_to_list(Cursor)
                ++ "&limit="
                ++ integer_to_list(Limit))
    of
        {ok, {{_, 200, _}, _, Body}} ->
            try parse_result_set(Body) of
                {HasNextPage, Results} ->
                    {ok, HasNextPage, Results, (lists:last(Results))#result.cursor}
                catch
                    _:_ ->
                        {error, cu_invalid_cron_response, Body}
                end;
        Response -> {error, cu_http_error, Response}
    end.

cron_cursor(ProcID) ->
    case httpc:request(ao:get(cu) ++ "/cron/" ++ ProcID ++ "?sort=DESC&limit=1") of
        {ok, {{_, 200, _}, _, Body}} ->
            {_, Res} = parse_result_set(Body),
            case Res of
                [] -> undefined;
                [Result] -> Result#result.cursor
            end;
        Response ->
            {error, cu_http_error, Response}
    end.

parse_result_set(Body) ->
    {JSONStruct} = jiffy:decode(Body),
    {_, {PageInfoStruct}} = lists:keyfind(<<"pageInfo">>, 1, JSONStruct),
    {_, HasNextPage} = lists:keyfind(<<"hasNextPage">>, 1, PageInfoStruct),
    {_, EdgesStruct} = lists:keyfind(<<"edges">>, 1, JSONStruct),
    {HasNextPage, lists:map(fun json_struct_to_result/1, EdgesStruct)}.

parse_result(Body) ->
    json_struct_to_result(jiffy:decode(Body)).

%% Parse a CU result into a #result record. If the result is in the form of a
%% stream, then the cursor is returned in the #result record as well.
json_struct_to_result(NodeStruct) ->
    json_struct_to_result(NodeStruct, #result{}).
json_struct_to_result({NodeStruct}, Res) ->
    json_struct_to_result(NodeStruct, Res);
json_struct_to_result(Struct, Res) ->
    case lists:keyfind(<<"node">>, 1, Struct) of
        false ->
            Res#result{
                messages = lists:map(
                    fun ar_bundles:json_struct_to_item/1,
                    ar_util:find_value(<<"Messages">>, Struct, [])),
                assignments = ar_util:find_value(<<"Assignments">>, Struct, []),
                spawns = lists:map(
                    fun ar_bundles:json_struct_to_item/1,
                    ar_util:find_value(<<"Spawns">>, Struct, [])),
                output = ar_util:find_value(<<"Output">>, Struct, [])
            };
        {_, {NodeStruct}} ->
            json_struct_to_result(
                NodeStruct,
                Res#result{
                    cursor = ar_util:find_value(<<"cursor">>, Struct, undefined)
                }
            )
    end.

arweave_timestamp() ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(ao:get(arweave_gateway) ++ "/block/current"),
    {Fields} = jiffy:decode(Body),
    {_, Timestamp} = lists:keyfind(<<"timestamp">>, 1, Fields),
    {_, Hash} = lists:keyfind(<<"indep_hash">>, 1, Fields),
    {_, Height} = lists:keyfind(<<"height">>, 1, Fields),
    {Timestamp, Height, Hash}.