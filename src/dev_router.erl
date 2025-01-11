%%% @doc A device that routes outbound messages from the node to their
%%% appropriate network recipients via HTTP. All messages are initially
%%% routed to a single process per node, which then load-balances them
%%% between downstream workers that perform the actual requests.
-module(dev_router).
-export([find_route/2, find_route/3]).

%% @doc If we have a route that has multiple resolving nodes in a bundle, check
%% the load distribution strategy and choose a node. Supported strategies:
%% ```
%%       Random: Distribute load evenly across all nodes, non-deterministically.
%%       By-Base: According to the base message's hashpath.
%%       Nearest: According to the distance of the node's wallet address to the
%%                base message's hashpath.
%% '''
%% `By-Base` will ensure that all traffic for the same hashpath is routed to the
%% same node, minimizing work duplication, while `Random` ensures a more even
%% distribution of the requests.
find_route(Msg, Opts) -> find_route(undefined, Msg, Opts).
find_route(_, Msg, Opts) ->
    Routes = hb_opts:get(routes, Opts),
    R = first_match(Msg, Routes, Opts),
    case hb_converge:get(<<"Node">>, R, Opts) of
        Node when is_binary(Node) -> {ok, Node};
        not_found ->
            Nodes = hb_converge:get(<<"Nodes">>, R, Opts),
            case hb_converge:get(<<"Strategy">>, R, Opts) of
                not_found -> {ok, Nodes};
                Strategy ->
                    ChooseN = hb_converge:get(<<"Choose">>, R, 1, Opts),
                    Hashpath = hb_path:from_message(hashpath, R),
                    Chosen = choose(ChooseN, Strategy, Hashpath, Nodes, Opts),
                    case Chosen of
                        [X] when is_map(X) ->
                            {ok, hb_converge:get(<<"Host">>, X, Opts)};
                        [X] -> {ok, X};
                        _ ->
                            {ok, hb_converge:set(<<"Nodes">>, Chosen, Opts)}
                    end
            end
    end.

%% @doc Find the first matching template in a list of known routes.
first_match(ToMatch, Routes, Opts) ->
    first_match(
        ToMatch,
        Routes,
        hb_converge:keys(hb_converge:ensure_message(Routes)),
        Opts
    ).
first_match(_, _, [], _) -> no_matches;
first_match(ToMatch, Routes, [XKey|Keys], Opts) ->
    XM = hb_converge:get(XKey, Routes, Opts),
    case hb_message:match(ToMatch, hb_converge:get(<<"Template">>, XM, Opts)) of
        true -> XM;
        false -> first_match(ToMatch, Routes, Keys, Opts)
    end.

%% @doc Implements the load distribution strategies if given a cluster.
choose(0, _, _, _, _) -> [];
choose(N, <<"Random">>, _, Nodes, _Opts) ->
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    [Node | choose(N - 1, <<"Random">>, nop, lists:delete(Node, Nodes), _Opts)];
choose(N, <<"By-Base">>, Hashpath, Nodes, Opts) when is_binary(Hashpath) ->
    choose(N, <<"By-Base">>, binary_to_integer(Hashpath), Nodes, Opts);
choose(N, <<"By-Base">>, HashInt, Nodes, Opts) ->
    Node = lists:nth(HashInt rem length(Nodes), Nodes),
    [
        Node
    |
        choose(
            N - 1,
            <<"By-Base">>,
            HashInt,
            lists:delete(Node, Nodes),
            Opts
        )
    ];
choose(N, <<"Nearest">>, HashPath, Nodes, Opts) ->
    HashInt = binary_to_integer(HashPath),
    NodesWithDistances =
        lists:map(
            fun(Node) ->
                NodeMsg = hb_converge:get(<<"Host">>, Node, Opts),
                WalletNum =
                    binary_to_integer(
                        hb_converge:get(<<"Wallet">>, NodeMsg, Opts)),
                {Node, (WalletNum - HashInt) rem math:pow(2, 256)}
            end,
            Nodes
        ),
    lists:reverse(
        lists:foldl(
            fun(_, {Current, Remaining}) ->
                {Lowest, _} = lowest_distance(Remaining),
                {[Lowest|Current], lists:delete(Lowest, Remaining)}
            end,
            {[], NodesWithDistances},
            lists:seq(1, N)
        )
    ).

lowest_distance(Nodes) -> lowest_distance(Nodes, infinity).
lowest_distance([], X) -> X;
lowest_distance([{Node, Distance}|Nodes], {CurrentNode, CurrentDistance}) ->
    case Distance of
        infinity -> lowest_distance(Nodes, Node);
        _ when Distance < CurrentDistance -> lowest_distance(Nodes, Node);
        _ -> lowest_distance(Nodes, CurrentNode)
    end.
