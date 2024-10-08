-module(ao_fs_store).
-behavior(ao_store).
-export([start/1, stop/1, reset/1]).
-export([type/2, read/2, write/3, path/2, add_path/3]).
-export([make_group/2, make_link/3]).
-include_lib("kernel/include/file.hrl").
-include("include/ao.hrl").

%%% A key-value store abstraction, such that the underlying implementation
%%% can be swapped out easily. The default implementation is a file-based
%%% store.

start(#{ dir := DataDir }) ->
    ok = filelib:ensure_dir(DataDir).

stop(#{ dir := _DataDir }) ->
    ok.

reset(#{ dir := DataDir }) ->
    os:cmd("rm -Rf " ++ DataDir).

%% @doc Read a key from the store, following symlinks as needed.
read(#{ dir := DataDir }, Key) ->
    read(filename:join(DataDir, Key)).
read(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular}} ->
            {ok, File} = file:read_file(Path),
            {ok, File};
        _ ->
            case file:read_link(Path) of
                {ok, Link} -> read(Link);
                _ -> not_found
            end
    end.

write(#{ dir := DataDir }, Key, Value) ->
    Path = filename:join(DataDir, Key),
    ?c({writing, Path, byte_size(Value)}),
    filelib:ensure_dir(Path),
    ok = file:write_file(Path, Value).

type(#{ dir := DataDir }, Key) ->
    type(filename:join(DataDir, Key)).
type(Path) ->
    ?c({type, Path}),
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} -> composite;
        {ok, #file_info{type = regular}} -> simple;
        _ ->
            case file:read_link(Path) of
                {ok, Link} -> type(Link);
                _ -> not_found
            end
    end.

make_group(#{ dir := DataDir }, Path) ->
    ?c({mkdir, Path}),
    ok = filelib:ensure_dir(filename:join(DataDir, Path)).

make_link(_, Link, Link) -> ok;
make_link(#{ dir := DataDir }, Existing, New) ->
    ?c({symlink, Existing, New}),
    ok = file:make_symlink(
        filename:join(DataDir, Existing),
        filename:join(DataDir, New)).

path(#{ dir := _DataDir }, Path) ->
    ?c({making_path, Path}),
    filename:join(Path).

add_path(#{ dir := _DataDir }, Path1, Path2) ->
    filename:join(Path1, Path2).
