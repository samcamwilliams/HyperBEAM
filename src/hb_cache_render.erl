%%% @doc A module that helps to render given Key graphs into the .dot files
-module(hb_cache_render).
-export([render/1, render/2]).
% Preparing data for testing
-export([prepare_unsigned_data/0, prepare_signed_data/0,
    prepare_deeply_nested_complex_message/0]).
-include("src/include/hb.hrl").

%% @doc Render the given Key into svg
render(StoreOrOpts) ->
    render(all, StoreOrOpts).
render(ToRender, StoreOrOpts) when is_map(StoreOrOpts) ->
    Store = hb_opts:get(store, no_viable_store, StoreOrOpts),
    render(ToRender, Store);
render(all, Store) ->
    {ok, Keys} = hb_store:list(Store, "/"),
    render(Keys, Store);
render(InitPath, Store) when is_binary(InitPath) ->
    {ok, Keys} = hb_store:list(Store, InitPath),
    render(Keys, Store);
render(Keys, Store) ->
    % Collect graph elements (nodes and arcs) by traversing the store
    Graph = collect_graph(Store, Keys),
    % Generate and view the graph visualization
    generate_dot_file(Graph),
    os:cmd("dot -Tsvg new_render_diagram.dot -o new_render_diagram.svg"),
    os:cmd("open new_render_diagram.svg"),
    ok.

%% @doc Main function to collect graph elements
collect_graph(Store, RootKeys) ->
    % Use a map to track nodes, arcs and visited paths (to avoid cycles)
    EmptyGraph = #{nodes => #{}, arcs => #{}, visited => #{}},
    % Process all root keys and get the final graph
    lists:foldl(
        fun(Key, Acc) -> traverse_store(Store, Key, undefined, Acc) end,
        EmptyGraph,
        RootKeys
    ).

%% @doc Traverse the store recursively to build the graph
traverse_store(Store, Key, Parent, Graph) ->
    % Get the path and check if we've already visited it
    JoinedPath = hb_store:join(Key),
    ResolvedPath = hb_store:resolve(Store, Key),
    % Skip if we've already processed this node
    case maps:get(visited, Graph, #{}) of
        #{JoinedPath := _} -> Graph;
        _ ->
            % Mark as visited to avoid cycles
            Graph1 = Graph#{visited => maps:put(JoinedPath, true, maps:get(visited, Graph, #{}))},
            % Process node based on its type
            case hb_store:type(Store, Key) of
                simple -> 
                    process_simple_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph1);
                composite -> 
                    process_composite_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph1);
                _ -> 
                    Graph1
            end
    end.

%% @doc Process a simple (leaf) node
process_simple_node(Store, _Key, Parent, ResolvedPath, JoinedPath, Graph) ->
    % Add the node to the graph
    Graph1 = add_node(Graph, ResolvedPath, "lightblue"),
    % If we have a parent, add an arc from parent to this node
    case Parent of
        undefined -> Graph1;
        ParentPath -> 
            Label = extract_label(JoinedPath),
            add_arc(Graph1, ParentPath, ResolvedPath, Label)
    end.

%% @doc Process a composite (directory) node
process_composite_node(Store, Key, Parent, ResolvedPath, JoinedPath, Graph) ->
    % Add the node to the graph
    Graph1 = add_node(Graph, ResolvedPath, "lightcoral"),
    % If we have a parent, add an arc from parent to this node
    Graph2 = case Parent of
        undefined -> Graph1;
        ParentPath -> 
            Label = extract_label(JoinedPath),
            add_arc(Graph1, ParentPath, ResolvedPath, Label)
    end,
    % Process children recursively
    case hb_store:list(Store, ResolvedPath) of
        {ok, SubItems} ->
            lists:foldl(
                fun(SubItem, Acc) ->
                    ChildKey = [ResolvedPath, SubItem],
                    traverse_store(Store, ChildKey, ResolvedPath, Acc)
                end,
                Graph2,
                SubItems
            );
        _ -> Graph2
    end.

%% @doc Add a node to the graph
add_node(Graph, ID, Color) ->
    Nodes = maps:get(nodes, Graph, #{}),
    Graph#{nodes => maps:put(ID, {ID, Color}, Nodes)}.

%% @doc Add an arc to the graph
add_arc(Graph, From, To, Label) ->
    ?event({insert_arc, {id1, From}, {id2, To}, {label, Label}}),
    Arcs = maps:get(arcs, Graph, #{}),
    Graph#{arcs => maps:put({From, To, Label}, true, Arcs)}.

%% @doc Extract a label from a path
extract_label(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [] -> Path;
        Parts -> 
            FilteredParts = [P || P <- Parts, P /= <<>>],
            case FilteredParts of
                [] -> Path;
                _ -> lists:last(FilteredParts)
            end
    end.

%% @doc Generate the DOT file from the graph
generate_dot_file(Graph) ->
    os:cmd("rm new_render_diagram.dot"),
    {ok, IoDevice} = file:open("new_render_diagram.dot", [write]),
    % Write graph header
    ok = file:write(IoDevice, <<"digraph filesystem {\n">>),
    ok = file:write(IoDevice, <<"  node [shape=circle];\n">>),
    % Write all nodes
    maps:foreach(
        fun(ID, {Label, Color}) ->
            io:format(
                IoDevice,
                <<"  \"~s\" [label=\"~s\", color=~s, style=filled];~n">>,
                [ID, hb_util:short_id(hb_util:bin(Label)), Color]
            )
        end,
        maps:get(nodes, Graph, #{})
    ),
    % Write all arcs
    maps:foreach(
        fun({From, To, Label}, _) ->
            io:format(
                IoDevice,
                <<"  \"~s\" -> \"~s\" [label=\"~s\"];~n">>,
                [From, To, hb_util:short_id(hb_util:bin(Label))]
            )
        end,
        maps:get(arcs, Graph, #{})
    ),
    % Write graph footer
    ok = file:write(IoDevice, <<"}\n">>),
    file:close(IoDevice).

get_test_store() ->
    hb_opts:get(store,
        no_viable_store,
        #{
            store => #{
                <<"store-module">> => hb_store_fs,
                <<"prefix">> => <<"cache-TEST/render-fs">>
            }
        }
    ).

% Test data preparation functions
prepare_unsigned_data() ->
    Opts = #{
        store => #{
            <<"store-module">> => hb_store_fs,
            <<"prefix">> => <<"cache-TEST/render-fs">>
        }
    },
    Item = test_unsigned(#{ <<"key">> => <<"Simple unsigned data item">> }),
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_signed_data() ->
    Opts = #{
        store => #{
            <<"store-module">> => hb_store_fs,
            <<"prefix">> => <<"cache-TEST/render-fs">>
        }
    },
    Wallet = ar_wallet:new(),
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }, Wallet),
    %% Write the simple unsigned item
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_deeply_nested_complex_message() ->
    Opts = #{
        store => #{
            <<"store-module">> => hb_store_fs,
            <<"prefix">> => <<"cache-TEST/render-fs">>
        }
    },
    Wallet = ar_wallet:new(),
    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Wallet),
    Outer =
        #{
            <<"level1">> =>
                hb_message:attest(
                    #{
                        <<"level2">> =>
                            #{
                                <<"level3">> => Level3SignedSubmessage,
                                <<"e">> => <<"f">>,
                                <<"z">> => [1,2,3]
                            },
                        <<"c">> => <<"d">>,
                        <<"g">> => [<<"h">>, <<"i">>],
                        <<"j">> => 1337
                    },
                    ar_wallet:new()
                ),
            <<"a">> => <<"b">>
        },
    %% Write the nested item
    {ok, _} = hb_cache:write(Outer, Opts).

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"data">> => Data
    }.

% test_signed(Data) -> test_signed(Data, ar_wallet:new()).
test_signed(Data, Wallet) ->
    hb_message:attest(test_unsigned(Data), Wallet).