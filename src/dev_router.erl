%%% @doc A device that routes outbound messages from the node to their
%%% appropriate network recipients via HTTP. All messages are initially
%%% routed to a single process per node, which then load-balances them
%%% between downstream workers that perform the actual requests.
%%% 
%%% The routes for the router are defined in the `routes` key of the `Opts`,
%%% as a precidence-ordered list of maps. The first map that matches the
%%% message will be used to determine the route.
%%% 
%%% Multiple nodes can be specified as viable for a single route, with the
%%% `Choose` key determining how many nodes to choose from the list (defaulting
%%% to 1). The `Strategy` key determines the load distribution strategy,
%%% which can be one of `Random`, `By-Base`, or `Nearest`. The route may also 
%%% define additional parallel execution parameters, which are used by the
%%% `hb_http` module to manage control of requests.
%%% 
%%% The structure of the routes should be as follows:
%%% ```
%%%     Node?: The node to route the message to.
%%%     Nodes?: A list of nodes to route the message to.
%%%     Strategy?: The load distribution strategy to use.
%%%     Choose?: The number of nodes to choose from the list.
%%%     Template?: A message template to match the message against, either as a
%%%                map or a path regex.
%%% '''
-module(dev_router).
%%% Device API:
-export([routes/3]).
%%% Public utilities:
-export([find_route/2, find_route/3, match_routes/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Device function that returns all known routes.
routes(M1, M2, Opts) ->
    ?event(debug, {routes_msg, M1, M2}),
    Routes = hb_opts:get(routes, [], Opts),
    case hb_converge:get(method, M2, Opts) of
        <<"POST">> ->
            Owner = hb_opts:get(owner, undefined, Opts),
            RouteOwners = hb_opts:get(route_owners, [Owner], Opts),
            Signers = hb_message:signers(M2),
            IsTrusted =
                lists:any(
                    fun(Signer) -> lists:member(Signer, Signers) end,
                    RouteOwners
                ),
            case IsTrusted of
                true ->
                    Priority = hb_converge:get(<<"Priority">>, M2, Opts),
                    NewRoutes =
                        lists:sort(fun(X, Y) -> X > Y end, [Priority|Routes]),
                    hb_http_server:set_opts(Opts#{ routes => NewRoutes }),
                    {ok, <<"Route added.">>};
                false -> {error, not_authorized}
            end;
        _ -> {ok, Routes}
    end.

%% @doc If we have a route that has multiple resolving nodes, check
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
%% 
%% Can operate as a `Router/1.0` device, which will ignore the base message,
%% routing based on the Opts and request message provided, or as a standalone
%% function, taking only the request message and the `Opts` map.
find_route(Msg, Opts) -> find_route(undefined, Msg, Opts).
find_route(_, Msg, Opts) ->
    Routes = hb_opts:get(routes, [], Opts),
    R = match_routes(Msg, Routes, Opts),
    case (R =/= no_matches) andalso hb_converge:get(<<"Node">>, R, Opts) of
        false -> no_matches;
        Node when is_binary(Node) -> {ok, Node};
        not_found ->
            Nodes = hb_converge:get(<<"Peers">>, R, Opts),
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
                            {ok, hb_converge:set(<<"Peers">>, Chosen, Opts)}
                    end
            end
    end.

%% @doc Find the first matching template in a list of known routes.
match_routes(ToMatch, Routes, Opts) ->
    match_routes(
        ToMatch,
        Routes,
        hb_converge:keys(Routes),
        Opts
    ).
match_routes(_, _, [], _) -> no_matches;
match_routes(ToMatch, Routes, [XKey|Keys], Opts) ->
    XM = hb_converge:get(XKey, Routes, Opts),
    Template =
        hb_converge:get(
            <<"Template">>,
            XM,
            #{},
            Opts#{ hashpath => ignore }
        ),
    case template_matches(ToMatch, Template) of
        true -> XM;
        false -> match_routes(ToMatch, Routes, Keys, Opts)
    end.

%% @doc Check if a message matches a message template or path regex.
template_matches(ToMatch, Template) when is_map(Template) ->
    hb_message:match(Template, ToMatch, primary);
template_matches(ToMatch, Regex) when is_binary(Regex) ->
    MsgPath = (hb_path:from_message(request, ToMatch)),
    hb_path:regex_matches(MsgPath, Regex).

%% @doc Implements the load distribution strategies if given a cluster.
choose(0, _, _, _, _) -> [];
choose(N, <<"Random">>, _, Nodes, _Opts) ->
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    [Node | choose(N - 1, <<"Random">>, nop, lists:delete(Node, Nodes), _Opts)];
choose(N, <<"By-Base">>, Hashpath, Nodes, Opts) when is_binary(Hashpath) ->
    choose(N, <<"By-Base">>, binary_to_bignum(Hashpath), Nodes, Opts);
choose(N, <<"By-Base">>, HashInt, Nodes, Opts) ->
    Node = lists:nth((HashInt rem length(Nodes)) + 1, Nodes),
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
    BareHashPath = hb_util:native_id(HashPath),
    NodesWithDistances =
        lists:map(
            fun(Node) ->
                Wallet = hb_converge:get(<<"Wallet">>, Node, Opts),
                DistanceScore =
                    hb_crypto:sha256(
                        <<
                            Wallet/binary,
                            BareHashPath/binary
                        >>
                    ),
                {Node, binary_to_bignum(DistanceScore)}
            end,
            Nodes
        ),
    lists:reverse(
        element(1,
            lists:foldl(
                fun(_, {Current, Remaining}) ->
                    Res = {Lowest, _} = lowest_distance(Remaining),
                    {[Lowest|Current], lists:delete(Res, Remaining)}
                end,
                {[], NodesWithDistances},
                lists:seq(1, N)
            )
        )
    ).

%% @doc Find the node with the lowest distance to the given hashpath.
lowest_distance(Nodes) -> lowest_distance(Nodes, {undefined, infinity}).
lowest_distance([], X) -> X;
lowest_distance([{Node, Distance}|Nodes], {CurrentNode, CurrentDistance}) ->
    case Distance of
        infinity -> lowest_distance(Nodes, {Node, Distance});
        _ when Distance < CurrentDistance ->
            lowest_distance(Nodes, {Node, Distance});
        _ -> lowest_distance(Nodes, {CurrentNode, CurrentDistance})
    end.

%% @doc Cast a human-readable or native-encoded ID to a big integer.
binary_to_bignum(Bin) when ?IS_ID(Bin) ->
    << Num:256/unsigned-integer >> = hb_util:native_id(Bin),
    Num.

%%% Tests

strategy_suite_test_() ->
    lists:map(
        fun(Strategy) ->
            {foreach,
                fun() -> ok end,
                fun(_) -> ok end,
                [
                    {
                        binary_to_list(Strategy) ++ ": " ++ Desc,
                        fun() -> Test(Strategy) end
                    }
                ||
                    {Desc, Test} <- [
                        {"unique", fun unique_test/1},
                        {"choose 1", fun choose_1_test/1},
                        {"choose n", fun choose_n_test/1}
                    ]
                ]
            }
        end,
        [<<"Random">>, <<"By-Base">>, <<"Nearest">>]
    ).

%% @doc Ensure that `By-Base` always chooses the same node for the same
%% hashpath.
by_base_determinism_test() ->
    FirstN = 5,
    Nodes = generate_nodes(5),
    HashPaths = generate_hashpaths(100),
    Simulation = simulate(HashPaths, FirstN, Nodes, <<"By-Base">>),
    Simulation2 = simulate(HashPaths, FirstN, Nodes, <<"By-Base">>),
    ?assertEqual(Simulation, Simulation2).

unique_test(Strategy) ->
    TestSize = 1,
    FirstN = 5,
    Nodes = generate_nodes(5),
    Simulation = simulate(TestSize, FirstN, Nodes, Strategy),
    unique_nodes(Simulation).

choose_1_test(Strategy) ->
    TestSize = 3750,
    Nodes = generate_nodes(20),
    Simulation = simulate(TestSize, 1, Nodes, Strategy),
    within_norms(Simulation, Nodes, TestSize).

choose_n_test(Strategy) ->
    TestSize = 1500,
    FirstN = 5,
    Nodes = generate_nodes(20),
    Simulation = simulate(TestSize, FirstN, Nodes, Strategy),
    within_norms(Simulation, Nodes, TestSize * 5),
    unique_nodes(Simulation).

unique_nodes(Simulation) ->
    lists:foreach(
        fun(SelectedNodes) ->
            lists:foreach(
                fun(Node) ->
                    ?assertEqual(1, hb_util:count(Node, SelectedNodes))
                end,
                SelectedNodes
            )
        end,
        Simulation
    ).

route_template_message_matches_test() ->
    Routes = [
        #{
            <<"Template">> => #{ <<"Other-Key">> => <<"Other-Value">> },
            <<"Node">> => <<"incorrect">>
        },
        #{
            <<"Template">> => #{ <<"Special-Key">> => <<"Special-Value">> },
            <<"Node">> => <<"correct">>
        }
    ],
    ?assertEqual(
        {ok, <<"correct">>},
        find_route(
            #{ path => <<"/">>, <<"Special-Key">> => <<"Special-Value">> },
            #{ routes => Routes }
        )
    ),
    ?assertEqual(
        no_matches,
        find_route(
            #{ path => <<"/">>, <<"Special-Key">> => <<"Special-Value2">> },
            #{ routes => Routes }
        )
    ),
    ?assertEqual(
        {ok, <<"fallback">>},
        find_route(
            #{ path => <<"/">> },
            #{ routes => Routes ++ [#{ <<"Node">> => <<"fallback">> }] }
        )
    ).

route_regex_matches_test() ->
    Routes = [
        #{
            <<"Template">> => <<"/.*/Compute">>,
            <<"Node">> => <<"incorrect">>
        },
        #{
            <<"Template">> => <<"/.*/Schedule">>,
            <<"Node">> => <<"correct">>
        }
    ],
    ?assertEqual(
        {ok, <<"correct">>},
        find_route(#{ path => <<"/abc/Schedule">> }, #{ routes => Routes })
    ),
    ?assertEqual(
        {ok, <<"correct">>},
        find_route(#{ path => <<"/a/b/c/Schedule">> }, #{ routes => Routes })
    ),
    ?assertEqual(
        no_matches,
        find_route(#{ path => <<"/a/b/c/BadKey">> }, #{ routes => Routes })
    ).

get_routes_test() ->
    Node = hb_http_server:start_test_node(
        #{
            force_signed => false,
            routes => Routes = [
                #{
                    <<"Template">> => <<"*">>,
                    <<"Node">> => <<"our_node">>,
                    <<"Priority">> => 10
                }
            ]
        }
    ),
    Res = hb_client:routes(Node),
    ?event(debug, {get_routes_test, Res}),
    {ok, RecvdRoutes} = Res,
    ?assert(hb_message:match(Routes, RecvdRoutes)).

add_route_test() ->
    Node = hb_http_server:start_test_node(
        #{
            force_signed => false,
            routes => Routes = [
                #{
                    <<"Template">> => <<"/Some/Path">>,
                    <<"Node">> => <<"old">>,
                    <<"Priority">> => 10
                }
            ]
        }
    ),
    Res = hb_client:add_route(Node,
        NewRoute = #{
            <<"Template">> => <<"/Some/New/Path">>,
            <<"Node">> => <<"new">>,
            <<"Priority">> => 15
        }
    ),
    ?event(debug, {add_route_test, Res}),
    ?assertEqual({ok, <<"Route added.">>}, Res),
    Res2 = hb_client:routes(Node),
    ?event(debug, {new_routes, Res2}),
    {ok, RecvdRoutes} = Res2,
    ?assert(hb_message:match(Routes ++ [NewRoute], RecvdRoutes)).

%%% Statistical test utilities

generate_nodes(N) ->
    [
        #{
            <<"Host">> =>
                <<"http://localhost:", (integer_to_binary(Port))/binary>>,
            <<"Wallet">> => hb_util:encode(crypto:strong_rand_bytes(32))
        }
    ||
        Port <- lists:seq(1, N)
    ].

generate_hashpaths(Runs) ->
    [
        hb_util:encode(crypto:strong_rand_bytes(32))
    ||
        _ <- lists:seq(1, Runs)
    ].

simulate(Runs, ChooseN, Nodes, Strategy) when is_integer(Runs) ->
    simulate(
        generate_hashpaths(Runs),
        ChooseN,
        Nodes,
        Strategy
    );
simulate(HashPaths, ChooseN, Nodes, Strategy) ->
    [
        choose(ChooseN, Strategy, HashPath, Nodes, #{})
    ||
        HashPath <- HashPaths
    ].

simulation_occurences(SimRes, Nodes) ->
    lists:foldl(
        fun(NearestNodes, Acc) ->
            lists:foldl(
                fun(Node, Acc2) ->
                    Acc2#{ Node => maps:get(Node, Acc2) + 1 }
                end,
                Acc,
                NearestNodes
            )
        end,
        #{ Node => 0 || Node <- Nodes },
        SimRes
    ).

simulation_distribution(SimRes, Nodes) ->
    maps:values(simulation_occurences(SimRes, Nodes)).

within_norms(SimRes, Nodes, TestSize) ->
    Distribution = simulation_distribution(SimRes, Nodes),
    % Check that the mean is `TestSize/length(Nodes)`
    Mean = hb_util:mean(Distribution),
    ?assert(Mean == (TestSize / length(Nodes))),
    % Check that the highest count is not more than 3 standard deviations
    % away from the mean.
    StdDev3 = Mean + 3 * hb_util:stddev(Distribution),
    ?assert(lists:max(Distribution) < StdDev3).