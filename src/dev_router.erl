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
%%       By-Base: According to the base message's hash.
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
                    {ok,
                        choose(
                            {base, hb_path:from_message(hashpath, R)},
                            Nodes,
                            Opts
                        )
                    }
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
choose(random, Nodes, _Opts) ->
    lists:nth(rand:uniform(length(Nodes)), Nodes);
choose({base, Hashpath}, Nodes, _Opts) ->
    lists:nth(binary_to_integer(Hashpath) rem length(Nodes), Nodes).