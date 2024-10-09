-module(ao).
-export([config/0, get/1, get/2, c/1, c/2, c/3, build/0, profile/0]).
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
        cache_dir => "data",
        default_page_limit => 5,
        scheduler_location_ttl => 60 * 60 * 24 * 30,
        preloaded_devices =>
            #{
                <<"Scheduler">> => dev_scheduler,
                <<"Cron">> => dev_cron,
                <<"Deduplicate">> => dev_dedup,
                <<"JSON-Interface">> => dev_json_iface,
                <<"Monitor">> => dev_monitor,
                <<"WASM64-pure">> => dev_wasm
            },
        loadable_devices => [],
        % Dev options
        profiling => true,
        store => {ao_fs_store, #{ dir => "TEST-data" }}
    }.

get(Key) -> get(Key, undefined).
get(Key, Default) ->
    maps:get(Key, config(), Default).

c(X) -> c(X, "").
c(X, ModAtom) when is_atom(ModAtom) -> c(X, "[" ++ atom_to_list(ModAtom) ++ "]");
c(X, Mod) -> c(X, Mod, undefined).
c(X, ModStr, undefined) -> c(X, ModStr, "");
c(X, ModStr, Line) ->
    Now = erlang:system_time(millisecond),
    Last = erlang:put(last_debug_print, Now),
    TSDiff = case Last of undefined -> 0; _ -> Now - Last end,
    io:format(standard_error, "===== DEBUG PRINT[~p ~p:~w ~pms] =====> ~80p~n",
        [self(), ModStr, Line, TSDiff, X]),
    X.

build() ->
    r3:do(compile, [{dir, "src"}]).

profile() ->
    case ao:get(profiling) of
        false -> not_profiling;
        true ->
            %eprof:stop_profiling(),
            eprof:analyze(total)
    end.