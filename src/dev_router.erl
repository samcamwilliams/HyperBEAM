%%% @doc A device that routes outbound messages from the node to their
%%% appropriate network recipients via HTTP. All messages are initially
%%% routed to a single process per node, which then load-balances them
%%% between downstream workers that perform the actual requests.
-module(dev_router).
-export([info/0]).

info() ->
    #{
        grouper => fun grouper/0,
        worker => fun worker/3
    }.

%% @doc Return the same group name for all computations on the router
%% device, regardless of the request or stated base message.
grouper() -> {ok, <<"Router/1.0">>}.

%% @doc The worker is a singleton process which directs requests downstream
%% to an individual Erlang process per request. Before doing so, the singleton
%% attaches its `Msg1` (routing state), such that all requests (regardless of
%% origin inside the node) have access to up-to-date redirection information.
worker(GroupName, Msg1, _Opts) ->
    receive
        {resolve, Listener, GroupName, Msg2, ListenerOpts} ->
            Parent = self(),
            spawn(
                fun() ->
                    case match(Msg1, Msg2, ListenerOpts) of
                        {add_route, Prio, Template} ->
                            Parent ! {add, Prio, Template};
                        {send, Node} ->
                            {ok, Msg3} = hb_http:post(Node, Msg2),
                            Listener ! {resolved, Parent, GroupName, Msg2, Msg3}
                    end
                end
            )
    end.

% If we have a route that has multiple resolving nodes in a bundle, check the
% load distribution strategy and choose a node. Supported strategies:
% ```
%       Random: Distribute load evenly across all nodes, non-deterministically.
%       By-Base: According to the base message's hash.
% '''
% `By-Base` will ensure that all traffic for the same hashpath is routed to the
% sam node, minimizing work duplication, while `Random` ensures a more even
% distribution of the requests.
match(M1, M2, Opts) ->
    case first_match(hb_converge:get(<<"Routes">>, M1, Opts), M2, Opts) of
        Node when is_binary(Node) ->
            {send, Node};
        not_found -> throw(no_viable_route_known);
        Msg ->
            Nodes = hb_converge:get(<<"Nodes">>, Msg, Opts),
            case hb_converge:get(<<"Strategy">>, Msg, Opts) of
                <<"random">> ->
                    {send, choose(random, Nodes, Opts)};
                <<"By-Base">> ->
                    {send,
                        choose(
                            {base, hb_path:from_message(hashpath, Msg)},
                            Nodes,
                            Opts
                        )
                    }
            end
    end.

%% @doc Find the first matching template in a list of known routes.


%% @doc Implements the load distribution strategies if given a cluster.
choose(random, Nodes, _Opts) ->
    lists:nth(rand:uniform(length(Nodes)), Nodes);
choose({base, Hashpath}, Nodes, _Opts) ->
    lists:nth(binary_to_integer(Hashpath) rem length(Nodes), Nodes).