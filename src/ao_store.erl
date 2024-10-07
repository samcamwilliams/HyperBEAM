-module(ao_store).
-export([behavior_info/1]).
-export([start/1, stop/1, reset/1]).
-export([type/2, read/2, write/3, path/2, add_path/3]).
-export([make_group/2, make_link/3]).

%%% A simple abstraction layer for AO key value store operations.
%%% This interface allows us to swap out the underlying store
%%% implementation later, or even at the configuration level if desired.

behavior_info(callbacks) ->
    [
        {start, 1}, {stop, 1}, {reset, 1}, {make_group, 2}, {make_link, 3},
        {type, 2}, {read, 2}, {write, 3},
        {path, 2}, {add_path, 3}
    ].

%%% Library wrapper implementations.

start({Mod, Opts}) -> Mod:start(Opts).

stop({Mod, Opts}) -> Mod:stop(Opts).

read({Mod, Opts}, Key) -> Mod:read(Opts, Key).

write({Mod, Opts}, Key, Value) -> Mod:write(Opts, Key, Value).  

make_group({Mod, Opts}, Path) -> Mod:make_group(Opts, Path).

make_link({Mod, Opts}, Existing, New) -> Mod:make_link(Opts, Existing, New).

reset({Mod, Opts}) -> Mod:reset(Opts).

type({Mod, Opts}, Path) -> Mod:type(Opts, Path).

path({Mod, Opts}, Path) -> Mod:path(Opts, Path).

add_path({Mod, Opts}, Path1, Path2) -> Mod:add_path(Opts, Path1, Path2).