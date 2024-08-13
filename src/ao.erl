-module(ao).
-export([config/0, get/1]).

config() ->
    #{
        http_port => 10451,
        http_host => "localhost",
        arweave_gateway => "https://arweave.net",
        arweave_bundler => "https://up.arweave.net",
        su => "http://localhost:10451/su",
        mu => "http://localhost:10451/mu",
        cu => "http://localhost:10451/cu"
    }.

get(Key) ->
    maps:get(Key, config()).