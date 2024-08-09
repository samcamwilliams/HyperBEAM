-module(su).
-export([start/0, build/0, c/1]).

start() ->
    build(),
    io:format("Booting SuperSu!\n"),
    inets:start(),
    ssl:start(),
    su_data:init(),
    su_registry:start(),
    su_timestamp:start(),
    su_http:start().

build() ->
    io:format("Building SuperSu...\n"),
    make:all([load]),
    io:format("SuperSu built.\n").

c(Value) ->
    io:format("~p\n", [Value]),
    Value.