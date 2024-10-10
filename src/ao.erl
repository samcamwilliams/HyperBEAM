-module(ao).
-export([config/0, now/0, get/1, get/2, c/1, c/2, c/3, build/0, profile/1]).
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
        %%%%%%%% Functional options %%%%%%%%
        %% Scheduling mode: Determines when the SU should inform the recipient
        %% that an assignment has been scheduled for a message.
        %% Options: aggressive(!), local_confirmation, remote_confirmation
        scheduling_mode => aggressive, 
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
                <<"Scheduler">> => dev_scheduler,
                <<"Cron">> => dev_cron,
                <<"Deduplicate">> => dev_dedup,
                <<"JSON-Interface">> => dev_json_iface,
                <<"Monitor">> => dev_monitor,
                <<"WASM64-pure">> => dev_wasm
            },
        loadable_devices => [],
        % Dev options
        store => {ao_fs_store, #{ dir => "TEST-data" }},
        debug_print => true
    }.

get(Key) -> get(Key, undefined).
get(Key, Default) ->
    maps:get(Key, config(), Default).

c(X) -> c(X, "").
c(X, Mod) -> c(X, Mod, undefined).
c(X, ModStr, undefined) -> c(X, ModStr, "");
c(X, ModAtom, Line) when is_atom(ModAtom) ->
    case lists:member({ao_debug, [print]}, ModAtom:module_info(attributes)) of
        true -> debug_print(X, atom_to_list(ModAtom), Line);
        false -> 
            case lists:member({ao_debug, [no_print]}, ModAtom:module_info(attributes)) of
                false -> c(X, atom_to_list(ModAtom), Line);
                true -> X
            end
    end;
c(X, ModStr, Line) ->
    case ao:get(debug_print) of
        true -> debug_print(X, ModStr, Line);
        false -> X
    end.

debug_print(X, ModStr, Line) ->
    Now = erlang:system_time(millisecond),
    Last = erlang:put(last_debug_print, Now),
    TSDiff = case Last of undefined -> 0; _ -> Now - Last end,
    io:format(standard_error, "===== DEBUG PRINT[~p ~s:~w ~pms] =====> ~80p~n",
        [self(), ModStr, Line, TSDiff, X]),
    X.

now() ->
    erlang:system_time(millisecond).

build() ->
    r3:do(compile, [{dir, "src"}]).

profile(Fun) ->
    eprof:start_profiling([self()]),
    try
        Fun()
    after
        eprof:stop_profiling()
    end,
    eprof:analyze(total).