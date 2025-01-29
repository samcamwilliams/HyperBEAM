%%% @doc A module that helps to render given Key graphs into the .dot files
-module(hb_cache_render).

-export([render/1, render/2, render/3]).

% Preparing data for testing
-export([prepare_unsigned_data/0, prepare_signed_data/0, prepare_deeply_nested_complex_message/0]).

-include("src/include/hb.hrl").

-include_lib("kernel/include/file.hrl").

%% @doc Render the given Key into svg
render(Key) ->
    render("TEST-cache-fs", Key).

render(BaseDir, Key) -> 
    {ok, IoDevice} = file:open("diagram.dot", [write]),
    ok = file:write(IoDevice, <<"digraph filesystem {\n">>),
    ok = file:write(IoDevice, <<"  node [shape=circle];\n">>),

    render(IoDevice, BaseDir, Key),
    ok = file:write(IoDevice, <<"}\n">>),
    
    file:close(IoDevice),
    os:cmd("dot -Tsvg diagram.dot -o dotfile.svg"),
    os:cmd("open dotfile.svg").

render(IoDevice, Dir, Key) ->
    Path = filename:join([Dir, Key]),
    case file:list_dir_all(Path) of
        {ok, SubKeys} -> 
            % We are inside the folder
            add_dir(IoDevice, Key),

            lists:foreach(
                fun(SubKey) ->
                    case type(filename:join([Path, SubKey])) of
                        {link, Link} -> 
                            % Follow the link and get the data
                            LinkName = filename:join([Path, SubKey]),
                            ok = add_link(IoDevice, LinkName, SubKey),
                            insert_arc(IoDevice, Key, LinkName, "contains"),
                            
                            % Get the data after link
                            LinkedKey = filename:basename(Link),
                            DirName = filename:dirname(Link),
                            insert_arc(IoDevice, LinkName, LinkedKey, "links-to"),

                            % Follow the link and render what it has
                            render(IoDevice, DirName, LinkedKey);

                        composite -> 
                            insert_arc(IoDevice, Key, SubKey, "contains"),
                            render(IoDevice, Path, SubKey);
                        raw -> 
                            insert_arc(IoDevice, Key, SubKey, "contains"),
                            add_data(IoDevice, SubKey)
                    end
                end,
                SubKeys
            );
        {error, enoent} ->
            ?event({path_not_found, {path, Path}});

        {error, enotdir} -> 
            add_data(IoDevice, Key)
    end.

% Helper functions
add_link(IoDevice, Id, Label) ->
    insert_circle(IoDevice, Id, Label, "green").

add_data(IoDevice, Id) ->
    insert_circle(IoDevice, Id, Id, "blue").

add_dir(IoDevice, Id) ->
    insert_circle(IoDevice, Id, Id, "yellow").

type(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} -> composite;
        {ok, #file_info{type = regular}} -> raw;
        _ ->
            case file:read_link(Path) of
                {ok, Link} ->
                    {link, Link};
                _ ->
                    not_found
            end
        end.

insert_arc(IoDevice, ID1, ID2, Label) ->
    ok = io:format(IoDevice, "  \"~s\" -> \"~s\" [label=\"~s\"];~n", [ID1, ID2, Label]).

insert_circle(IoDevice, ID, Label, Color) ->
    ok = io:format(IoDevice, "  \"~s\" [label=\"~s\", color=~s, style=filled];~n", [ID, Label, Color]).

% Preparing the test data
prepare_unsigned_data() ->
    Opts = #{store => {hb_store_fs,#{prefix => "TEST-cache-fs"}}},
    Item = test_unsigned(#{ <<"key">> => <<"Simple unsigned data item">> }),
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_signed_data() ->
    Opts = #{store => {hb_store_fs,#{prefix => "TEST-cache-fs"}}},
    Wallet = ar_wallet:new(),
    Item = test_signed(#{ <<"l2-test-key">> => <<"l2-test-value">> }, Wallet),
    %% Write the simple unsigned item
    {ok, _Path} = hb_cache:write(Item, Opts).

prepare_deeply_nested_complex_message() ->
    Opts = #{store => {hb_store_fs,#{prefix => "TEST-cache-fs"}}},
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