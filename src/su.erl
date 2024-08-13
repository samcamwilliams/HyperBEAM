-module(su).
-export([start/0, build/0, c/1]).

start() ->
    application:start(supersu_app).

build() ->
    io:format("Building SuperSu...\n"),
    make:all([load]),
    io:format("SuperSu built.\n").

c(Value) ->
    io:format("~p\n", [Value]),
    Value.