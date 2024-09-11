-module(ao).
-export([config/0, get/1, c/1, build/0]).

config() ->
    #{
        http_port => 8734,
        http_host => "localhost",
        gateway => "https://arweave.net",
        bundler => "https://up.arweave.net",
        su => "http://localhost:8734/su",
        mu => "http://localhost:8734/mu",
        cu => "https://cu24.ao-testnet.xyz",
        key_location => "hyperbeam-key.json",
        default_page_limit => 5,
        scheduler_location_ttl => 60 * 60 * 24 * 30
    }.

get(Key) ->
    maps:get(Key, config()).

c(X) ->
    io:format("===== DEBUG PRINT =====> ~80p~n", [X]),
    X.

build() ->
    r3:do(compile, [{dir, "src"}]).