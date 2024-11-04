-module(ao_fs_store).
-behavior(ao_store).
-export([start/1, stop/1, reset/1]).
-export([type/2, read/2, write/3, list/2]).
-export([path/2, add_path/3]).
-export([make_group/2, make_link/3, resolve/2]).
-include_lib("kernel/include/file.hrl").
-include("include/ao.hrl").
-ao_debug(print).

%%% A key-value store abstraction, such that the underlying implementation
%%% can be swapped out easily. The default implementation is a file-based
%%% store.

start(#{ prefix := DataDir }) ->
    ok = filelib:ensure_dir(DataDir).

stop(#{ prefix := _DataDir }) ->
    ok.

reset(#{ prefix := DataDir }) ->
    os:cmd("rm -Rf " ++ DataDir).

%% @doc Read a key from the store, following symlinks as needed.
read(Opts, Key) ->
    read(ao_store_common:add_prefix(Opts, resolve(Opts, Key))).
read(Path) ->
    ?c({read, Path}),
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular}} ->
            {ok, _} = file:read_file(Path);
        _ ->
            case file:read_link(Path) of
                {ok, Link} ->
                    ?c({link_found, Path, Link}),
                    read(Link);
                _ ->
                    not_found
            end
    end.

write(Opts, PathComponents, Value) ->
    Path = ao_store_common:add_prefix(Opts, PathComponents),
    ?c({writing, Path, byte_size(Value)}),
    filelib:ensure_dir(Path),
    ok = file:write_file(Path, Value).

list(Opts, Path) ->
    file:list_dir(ao_store_common:add_prefix(Opts, Path)).

%% @doc Replace links in a path successively, returning the final path.
%% Each element of the path is resolved in turn, with the result of each
%% resolution becoming the prefix for the next resolution. This allows 
%% paths to resolve across many links. For example, a structure as follows:
%%
%%    /a/b/c: "Not the right data"
%%    /a/b -> /a/alt-b
%%    /a/alt-b/c: "Correct data"
%%
%% will resolve "a/b/c" to "Correct data".
resolve(Opts, RawPath) ->
    Res = resolve(Opts, "", filename:split(ao_store_common:join(RawPath))),
    ?c({resolved, RawPath, Res}),
    Res.
resolve(_, CurrPath, []) ->
    ao_store_common:join(CurrPath);
resolve(Opts, CurrPath, [Next|Rest]) ->
    PathPart = ao_store_common:join([CurrPath, Next]),
    ?c({resolving, {accumulated_path, CurrPath}, {next_segment, Next}, {generated_partial_path_to_test, PathPart}}),
    case file:read_link(ao_store_common:add_prefix(Opts, PathPart)) of
        {ok, RawLink} ->
            Link = ao_store_common:remove_prefix(Opts, RawLink),
            resolve(Opts, Link, Rest);
        _ ->
            resolve(Opts, PathPart, Rest)
    end.

type(#{ prefix := DataDir }, Key) ->
    type(ao_store_common:join([DataDir, Key])).
type(Path) ->
    case file:read_file_info(Joint = ao_store_common:join(Path)) of
        {ok, #file_info{type = directory}} -> composite;
        {ok, #file_info{type = regular}} -> simple;
        _ ->
            case file:read_link(Joint) of
                {ok, Link} ->
                    type(Link);
                _ ->
                    not_found
            end
    end.

make_group(#{ prefix := DataDir }, Path) ->
    P = ao_store_common:join([DataDir, Path]),
    ?c({making_group, P}),
    ok = filelib:ensure_dir(P).

make_link(_, Link, Link) -> ok;
make_link(Opts, Existing, New) ->
    ?c({symlink,
        ao_store_common:add_prefix(Opts, Existing),
        P2 = ao_store_common:add_prefix(Opts, New)}),
    filelib:ensure_dir(P2),
    file:make_symlink(
        ao_store_common:add_prefix(Opts, Existing),
        ao_store_common:add_prefix(Opts, New)
    ).

%% @doc Create a path from a list of path components.
path(#{ prefix := _DataDir }, Path) ->
    Path.

%% @doc Add two path components together.
add_path(#{ prefix := _DataDir }, Path1, Path2) ->
    Path1 ++ Path2.