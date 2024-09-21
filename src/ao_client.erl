-module(ao_client).
-export([download/1]).
-export([upload/1, arweave_timestamp/0]).
-export([schedule/1, assign/1, register_su/1, register_su/2]).
-export([compute/1, cron/1, cron/2, cron/3, cron_cursor/1]).
-export([push/1]).

-include("include/ao.hrl").

%%% Arweave API functions

download(ID) ->
    % TODO: Need to recreate full data items, not just data...
    case httpc:request(ao:get(gateway) ++ "/" ++ ID) of
        {ok, {{_, 200, _}, _, Body}} -> #tx{ data = Body };
        Rest -> throw({id_get_failed, Res})
    end.

upload(Item) ->
    case httpc:request(
        post,
        {ao:get(bundler) ++ "/tx", [], "application/octet-stream", ar_bundles:serialize(Item)},
        [],
        []
    ) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jiffy:decode(Body, [return_maps])};
        Response ->
            {error, bundler_http_error, Response}
    end.

arweave_timestamp() ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(ao:get(gateway) ++ "/block/current"),
    {Fields} = jiffy:decode(Body),
    {_, Timestamp} = lists:keyfind(<<"timestamp">>, 1, Fields),
    {_, Hash} = lists:keyfind(<<"indep_hash">>, 1, Fields),
    {_, Height} = lists:keyfind(<<"height">>, 1, Fields),
    {Timestamp, Height, Hash}.

%%% Scheduler API functions

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

register_su(Location) ->
    register_su(Location, ao:get(key_location)).
register_su(Location, WalletLoc) when is_list(WalletLoc) ->
    register_su(Location, ar_wallet:load_keyfile(WalletLoc));
register_su(Location, Wallet) ->
    TX = #tx {
        tags = [
            {"Data-Protocol", "ao"},
            {"Variant", "ao.TN.1"},
            {"Type", "Scheduler-Location"},
            {"Url", Location},
            {"Time-To-Live", integer_to_list(ao:get(scheduler_location_ttl))}
        ]
    },
    ao_client:upload(ar_bundles:sign_item(TX, Wallet)).

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

%%% MU API functions

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

%%% CU API functions

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