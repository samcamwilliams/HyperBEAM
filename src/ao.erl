-module(ao).
-export([config/0, get/1, get/2, c/1, c/2, build/0, profile/0]).
-export([wallet/0, wallet/1]).

-include("include/ar.hrl").

wallet() -> wallet(ao:get(key_location)).
wallet(Location) ->
    case file:read_file_info(Location) of
        {ok, _} -> ar_wallet:load_keyfile(Location);
        {error, _} -> ar_wallet:new_keyfile(?DEFAULT_KEY_TYPE, ao:get(key_location))
    end.

config() ->
    #{
        % Functional options
        http_port => 8734,
        http_host => "localhost",
        gateway => "https://arweave.net",
        bundler => "https://up.arweave.net",
        su => "http://localhost:8734/su",
        mu => "http://localhost:8734/mu",
        cu => "http://localhost:8734/cu",
        key_location => "hyperbeam-key.json",
        default_page_limit => 5,
        scheduler_location_ttl => 60 * 60 * 24 * 30,
        preloaded_devices =>
            #{
                <<"Checkpoint">> => dev_checkpoint,
                <<"Scheduler">> => dev_scheduler,
                <<"Cron">> => dev_cron,
                <<"Deduplicate">> => dev_dedup,
                <<"JSON-Interface">> => dev_json_iface,
                <<"Monitor">> => dev_monitor,
                <<"WASM64-pure">> => dev_wasm
            },
        loadable_devices => [],
        % Dev options
        profiling => true
    }.

get(Key) -> get(Key, undefined).
get(Key, Default) ->
    maps:get(Key, config(), Default).

c(X) -> c(X, "").
c(X, ModAtom) when is_atom(ModAtom) -> c(X, "[" ++ atom_to_list(ModAtom) ++ "]");
c(X, ModStr) ->
    io:format(standard_error, "===== DEBUG PRINT~p =====~s> ~80p~n", [self(), ModStr, X]),
    X.

build() ->
    r3:do(compile, [{dir, "src"}]).

profile() ->
    case ao:get(profiling) of
        false -> not_profiling;
        true ->
            eprof:stop_profiling(),
            eprof:analyze(total)
    end.