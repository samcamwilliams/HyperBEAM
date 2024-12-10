-module(hb_store_fs).
-behavior(hb_store).
-export([start/1, stop/1, reset/1, scope/1]).
-export([type/2, read/2, write/3, list/2]).
-export([make_group/2, make_link/3, resolve/2]).
-include_lib("kernel/include/file.hrl").
-include("include/hb.hrl").

%%% A key-value store abstraction, such that the underlying implementation
%%% can be swapped out easily. The default implementation is a file-based
%%% store.

start(#{ prefix := DataDir }) ->
    ok = filelib:ensure_dir(DataDir).

stop(#{ prefix := _DataDir }) ->
    ok.

%% @doc The file-based store is always local, for now. In the future, we may
%% want to allow that an FS store is shared across a cluster and thus remote.
scope(_) -> local.

reset(#{ prefix := DataDir }) ->
    os:cmd("rm -Rf " ++ DataDir).

%% @doc Read a key from the store, following symlinks as needed.
read(Opts, Key) ->
    read(add_prefix(Opts, resolve(Opts, Key))).
read(Path) ->
    ?event({read, Path}),
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular}} ->
            {ok, _} = file:read_file(Path);
        _ ->
            case file:read_link(Path) of
                {ok, Link} ->
                    ?event({link_found, Path, Link}),
                    read(Link);
                _ ->
                    not_found
            end
    end.

write(Opts, PathComponents, Value) ->
    Path = add_prefix(Opts, PathComponents),
    ?event({writing, Path, byte_size(Value)}),
    filelib:ensure_dir(Path),
    ok = file:write_file(Path, Value).

list(Opts, Path) ->
    file:list_dir(add_prefix(Opts, Path)).

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
    Res = resolve(Opts, "", filename:split(hb_store:join(RawPath))),
    ?event({resolved, RawPath, Res}),
    Res.
resolve(_, CurrPath, []) ->
    hb_store:join(CurrPath);
resolve(Opts, CurrPath, [Next|Rest]) ->
    PathPart = hb_store:join([CurrPath, Next]),
    ?event({resolving, {accumulated_path, CurrPath}, {next_segment, Next}, {generated_partial_path_to_test, PathPart}}),
    case file:read_link(add_prefix(Opts, PathPart)) of
        {ok, RawLink} ->
            Link = remove_prefix(Opts, RawLink),
            resolve(Opts, Link, Rest);
        _ ->
            resolve(Opts, PathPart, Rest)
    end.

type(#{ prefix := DataDir }, Key) ->
    type(hb_store:join([DataDir, Key])).
type(Path) ->
    ?event({type, Path}),
    case file:read_file_info(Joint = hb_store:join(Path)) of
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
    P = hb_store:join([DataDir, Path]),
    ?event({making_group, P}),
    ok = filelib:ensure_dir(P).

make_link(_, Link, Link) -> ok;
make_link(Opts, Existing, New) ->
    ?event({symlink,
        add_prefix(Opts, Existing),
        P2 = add_prefix(Opts, New)}),
    filelib:ensure_dir(P2),
    file:make_symlink(
        add_prefix(Opts, Existing),
        add_prefix(Opts, New)
    ).

%% @doc Add the directory prefix to a path.
add_prefix(#{ prefix := Prefix }, Path) ->
    hb_store:join([Prefix, Path]).

%% @doc Remove the directory prefix from a path.
remove_prefix(#{ prefix := Prefix }, Path) ->
    hb_util:remove_common(Path, Prefix).