-module(hb_client).
%% Arweave node API
-export([arweave_timestamp/0]).
%% Arweave bundling and data access API
-export([upload/1, download/1]).
%% Scheduling Unit API
-export([get_assignments/1, get_assignments/2, get_assignments/3, get_assignments/4]).
-export([schedule/1, schedule/2, assign/1, register_su/1, register_su/2]).
-export([cron/1, cron/2, cron/3, cron/4, cron_cursor/1, cron_cursor/2]).
%% Compute Unit API
-export([compute/2, compute/3, compute/4]).
%% Messaging Unit API
-export([push/1, push/2, push/3]).

-include("include/hb.hrl").

%%% Arweave node API

%% @doc Grab the latest block information from the Arweave gateway node.
arweave_timestamp() ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(hb_opts:get(gateway) ++ "/block/current"),
    {Fields} = jiffy:decode(Body),
    {_, Timestamp} = lists:keyfind(<<"timestamp">>, 1, Fields),
    {_, Hash} = lists:keyfind(<<"indep_hash">>, 1, Fields),
    {_, Height} = lists:keyfind(<<"height">>, 1, Fields),
    {Timestamp, Height, Hash}.

%%% Bundling and data access API

%% @doc Download the data associated with a given ID. See TODO below.
download(ID) ->
    % TODO: Need to recreate full data items, not just data...
    case httpc:request(hb_opts:get(gateway) ++ "/" ++ ID) of
        {ok, {{_, 200, _}, _, Body}} -> #tx{data = Body};
        _Rest -> throw({id_get_failed, ID})
    end.

%% @doc Upload a data item to the bundler node.
upload(Item) ->
    case
        httpc:request(
            post,
            {hb_opts:get(bundler) ++ "/tx", [], "application/octet-stream", ar_bundles:serialize(Item)},
            [],
            []
        )
    of
        {ok, {{_, 200, _}, _, Body}} ->
            ?event(upload_success),
            {ok, jiffy:decode(Body, [return_maps])};
        Response ->
            ?event(upload_error),
            {error, bundler_http_error, Response}
    end.

%%% Scheduling Unit API

%% @doc Schedule a message on a process.
schedule(Msg) ->
    {ok, Node} = hb_router:find(schedule, ar_bundles:id(Msg, signed)),
    schedule(Node, Msg).
schedule(Node, Msg) ->
    hb_http:post(
        Node,
        "/",
        Msg
    ).

%% @doc Not implemented. Should gain an assignment from the SU for an ID
%% without having the full message.
assign(_ID) ->
    ?event({not_implemented, assignments}).

%% Send a Scheduler-Location message to the network so other nodes
%% know where to find this address's SU.
register_su(Location) ->
    register_su(Location, hb_opts:get(key_location)).
register_su(Location, WalletLoc) when is_list(WalletLoc) ->
    register_su(Location, ar_wallet:load_keyfile(WalletLoc));
register_su(Location, Wallet) ->
    TX = #tx{
        tags = [
            {"Data-Protocol", "ao"},
            {"Variant", "ao.TN.1"},
            {"Type", "Scheduler-Location"},
            {"Url", Location},
            {"Time-To-Live", integer_to_list(hb_opts:get(scheduler_location_ttl))}
        ]
    },
    hb_client:upload(ar_bundles:sign_item(TX, Wallet)).

%% @doc Ask a SU for the assignments on a process.
get_assignments(ProcID) ->
    get_assignments(ProcID, 0, undefined).
get_assignments(ProcID, From) ->
    get_assignments(ProcID, From, undefined).
get_assignments(ProcID, From, To) ->
    {ok, Node} = hb_router:find(schedule, ProcID),
    get_assignments(Node, ProcID, From, To).
get_assignments(Node, ProcID, From, To) ->
    {ok, #tx{data = Data}} =
        hb_http:get(
            Node,
            "/?Action=Schedule&Process=" ++
                binary_to_list(hb_util:id(ProcID)) ++
                case From of
                    undefined -> "";
                    _ -> "&From=" ++
                        case is_integer(From) of
                            true -> integer_to_list(From);
                            false -> binary_to_list(From)
                        end
                end ++
                case To of
                    undefined -> "";
                    _ -> "&To=" ++
                        case is_integer(To) of
                            true -> integer_to_list(To);
                            false -> binary_to_list(To)
                        end
                end
        ),
    ?event({requested_assignments_returned, From, To}),
    extract_assignments(From, To, Data).

extract_assignments(_, _, Assignments) when map_size(Assignments) == 0 ->
    [];
extract_assignments(From, To, Assignments) ->
    ?event({extracting_assignments, From, To}),
    KeyID = list_to_binary(integer_to_list(From)),
    case maps:is_key(KeyID, Assignments) of
        true ->
            [
                maps:get(KeyID, Assignments)
                | extract_assignments(From + 1, To, maps:remove(KeyID, Assignments))
            ];
        false ->
            ?event({no_assignment_for_key, KeyID}),
            []
    end.

%% @doc Compute the result of a message on a process.
compute(ProcIDBarer, Msg) when is_record(ProcIDBarer, tx) ->
    case lists:keyfind(<<"Process">>, 1, ProcIDBarer#tx.tags) of
        {<<"Process">>, ProcID} ->
            % Potential point of confusion: If the ProcIDBarer message
            % has a `Process` tag, then it is an _assignment_ -- not a
            % process message. We extract the `Process` tag to use as the
            % process ID.
            {ok, Node} = hb_router:find(compute, ProcID),
            compute(Node, ProcIDBarer, Msg);
        false ->
            case lists:keyfind(<<"Type">>, 1, ProcIDBarer#tx.tags) of
                {<<"Type">>, <<"Process">>} ->
                    % If the ProcIDBarer message has a `Type` tag, then it is
                    % a _process message_ -- not an assignment. Extract its ID and
                    % run on that.
                    {ok, Node} =
                        hb_router:find(compute, ProcID = hb_util:id(Msg, signed)),
                    compute(Node, ProcID, Msg);
                false ->
                    throw(
                        {unrecognized_message_to_compute_on,
                            hb_util:id(Msg, unsigned)}
                    )
            end
    end.
compute(Node, ProcID, Slot) ->
    compute(Node, ProcID, Slot, #{}).
compute(Node, ProcID, Slot, Opts) when is_binary(ProcID) ->
    compute(Node, binary_to_list(hb_util:id(ProcID)), Slot, Opts);
compute(Node, ProcID, Slot, Opts) when is_integer(Slot) ->
    compute(Node, ProcID, integer_to_list(Slot), Opts);
compute(Node, ProcID, AssignmentID, Opts) when is_binary(AssignmentID) ->
    compute(Node, ProcID, binary_to_list(hb_util:id(AssignmentID)), Opts);
compute(Node, ProcID, Slot, Opts) when is_list(Slot) ->
    hb_http:get(
        Node,
        "/?Process=" ++ ProcID ++ "&Slot=" ++ Slot ++ path_opts(Opts, "&")
    );
compute(Node, Assignment, Msg, Opts) when is_record(Assignment, tx) andalso is_record(Msg, tx) ->
    hb_http:post(
        Node,
        "/" ++ path_opts(Opts, "?"),
        ar_bundles:normalize(#{
            <<"Message">> => Msg,
            <<"Assignment">> => Assignment
        })
    ).

%%% MU API functions

%% @doc Trigger the process of pushing a message around the network.
push(Item) -> push(Item, #{}).
push(Item, Opts) ->
    {ok, Node} = hb_router:find(message, ar_bundles:id(Item, unsigned)),
    push(Node, Item, Opts).
push(Node, Item, Opts) when Item#tx.target =/= <<>> ->
    hb_http:post(
        Node,
        ["/", hb_util:encode(Item#tx.target), "/Push" ++ path_opts(Opts, "?")],
        Item
    );
push(Node, Item, Opts) ->
    ?event({calling_remote_push, hb_util:id(Item)}),
    hb_http:post(
        Node,
        ["/Push", path_opts(Opts, "?")],
        Item
    ).

%%% CU API functions

%% @deprecated This should not be used anywhere.
%% TODO: Remove?
cron(ProcID) ->
    cron(ProcID, cron_cursor(ProcID)).
cron(ProcID, Cursor) ->
    cron(ProcID, Cursor, hb_opts:get(default_page_limit)).
cron(ProcID, Cursor, Limit) when is_binary(ProcID) ->
    cron(binary_to_list(hb_util:id(ProcID)), Cursor, Limit);
cron(ProcID, undefined, RawLimit) ->
    cron(ProcID, cron_cursor(ProcID), RawLimit);
cron(ProcID, Cursor, Limit) ->
    {ok, Node} = hb_router:find(compute, ProcID),
    cron(Node, ProcID, Cursor, Limit).
cron(Node, ProcID, Cursor, Limit) ->
    case
        httpc:request(
            Node ++
                "/cron/" ++
                ProcID ++
                "?cursor=" ++
                binary_to_list(Cursor) ++
                "&limit=" ++
                integer_to_list(Limit)
        )
    of
        {ok, {{_, 200, _}, _, Body}} ->
            try parse_result_set(Body) of
                {HasNextPage, Results} ->
                    {ok, HasNextPage, Results, (lists:last(Results))#result.cursor}
            catch
                _:_ ->
                    {error, cu_invalid_cron_response, Body}
            end;
        Response ->
            {error, cu_http_error, Response}
    end.

%% @doc See above: Should also be unnecessary.
cron_cursor(ProcID) ->
    {ok, Node} = hb_router:find(compute, ProcID),
    cron_cursor(Node, ProcID).
cron_cursor(Node, ProcID) ->
    case httpc:request(Node ++ "/cron/" ++ ProcID ++ "?sort=DESC&limit=1") of
        {ok, {{_, 200, _}, _, Body}} ->
            {_, Res} = parse_result_set(Body),
            case Res of
                [] -> undefined;
                [Result] -> Result#result.cursor
            end;
        Response ->
            {error, cu_http_error, Response}
    end.

%%% Utility functions

%% @doc Convert a map of parameters into a query string, starting with the
%% given separator.
path_opts(EmptyMap, _Sep) when map_size(EmptyMap) == 0 -> "";
path_opts(Opts, Sep) ->
    PathParts = tl(lists:flatten(lists:map(
        fun({Key, Val}) ->
            "&" ++ format_path_opt(Key) ++ "=" ++ format_path_opt(Val)
        end,
        maps:to_list(Opts)
    ))),
    Sep ++ PathParts.

format_path_opt(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_path_opt(Val) when is_binary(Val) ->
    binary_to_list(Val);
format_path_opt(Val) when is_integer(Val) ->
    integer_to_list(Val).

parse_result_set(Body) ->
    {JSONStruct} = jiffy:decode(Body),
    {_, {PageInfoStruct}} = lists:keyfind(<<"pageInfo">>, 1, JSONStruct),
    {_, HasNextPage} = lists:keyfind(<<"hasNextPage">>, 1, PageInfoStruct),
    {_, EdgesStruct} = lists:keyfind(<<"edges">>, 1, JSONStruct),
    {HasNextPage, lists:map(fun json_struct_to_result/1, EdgesStruct)}.

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
                    hb_util:find_value(<<"Messages">>, Struct, [])
                ),
                assignments = hb_util:find_value(<<"Assignments">>, Struct, []),
                spawns = lists:map(
                    fun ar_bundles:json_struct_to_item/1,
                    hb_util:find_value(<<"Spawns">>, Struct, [])
                ),
                output = hb_util:find_value(<<"Output">>, Struct, [])
            };
        {_, {NodeStruct}} ->
            json_struct_to_result(
                NodeStruct,
                Res#result{
                    cursor = hb_util:find_value(<<"cursor">>, Struct, undefined)
                }
            )
    end.