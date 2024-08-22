-module(cu_erwamr).
-export([hello_world/0, init_runtime/0, cleanup_runtime/0]).

-include("src/include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

init() ->
    SoName = case code:priv_dir(hyperbeam) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, erwamr_nif]);
                _ ->
                    filename:join([priv, erwamr_nif])
            end;
        Dir ->
            filename:join(Dir, erwamr_nif)
    end,
    erlang:load_nif(SoName, 0).

hello_world() ->
    erlang:nif_error("NIF library not loaded").

init_runtime() ->
    erlang:nif_error("NIF library not loaded").

cleanup_runtime() ->
    erlang:nif_error("NIF library not loaded").

basic_test() ->
    hello_world().