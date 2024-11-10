-module(ao).
%%% Configuration and environment:
-export([config/0, now/0, get/1, get/2, build/0]).
%%% Debugging tools:
-export([c/1, c/2, c/3, no_prod/3, read/1, read/2, debug_wait/3, profile/1]).
%%% Node wallet and address management:
-export([address/0, wallet/0, wallet/1]).

-include("include/ar.hrl").
-define(ENV_KEYS,
    #{
        key_location => {"AO_KEY", "hyperbeam-key.json"},
        http_port => {"AO_PORT", fun list_to_integer/1, "8734"},
        store =>
            {"AO_STORE",
                fun(Dir) ->
                    [
                        {
                            ao_fs_store,
                            #{ prefix => Dir },
                            #{ scope => local }
                        },
                        {
                            ao_remote_store,
                            #{ node => "http://localhost:8734" },
                            #{ scope => remote }
                        }
                    ]
                end,
                "TEST-data"
            }
    }
).

wallet() ->
    wallet(ao:get(key_location)).
wallet(Location) ->
    case file:read_file_info(Location) of
        {ok, _} -> ar_wallet:load_keyfile(Location);
        {error, _} -> ar_wallet:new_keyfile(?DEFAULT_KEY_TYPE, ao:get(key_location))
    end.

address() ->
    ar_util:encode(ar_wallet:to_address(wallet())).

config() ->
    #{
        %%%%%%%% Functional options %%%%%%%%
        %% Scheduling mode: Determines when the SU should inform the recipient
        %% that an assignment has been scheduled for a message.
        %% Options: aggressive(!), local_confirmation, remote_confirmation
        scheduling_mode => aggressive, 
        http_host => "localhost",
        gateway => "https://arweave.net",
        bundler => "https://up.arweave.net",
        nodes => #{
            compute =>
                #{
                    address() => "http://localhost:8734/cu",
                    <<"J-j0jyZ1YWhMBXtJMWHz-dl-mDcksoJSQo_Fq5loHUs">> =>
                        "http://localhost:8736/cu",
                    '_' => "http://localhost:8734/cu"
                },
            message => #{
                address() => "http://localhost:8734/mu",
                '_' => "http://localhost:8734/mu"
            },
            schedule => #{
                address() => "http://localhost:8734/su",
                '_' => "http://localhost:8734/su"
            }
        },
        key_location => "hyperbeam-key.json",
        default_page_limit => 5,
        scheduler_location_ttl => 60 * 60 * 24 * 30,
        preloaded_devices =>
            #{
                <<"Stack">> => {dev_stack, execute},
                <<"Scheduler">> => dev_scheduler,
                <<"Cron">> => dev_cron,
                <<"Deduplicate">> => dev_dedup,
                <<"JSON-Interface">> => dev_json_iface,
                <<"VFS">> => dev_vfs,
                <<"PODA">> => dev_poda,
                <<"Monitor">> => dev_monitor,
                <<"WASM64-pure">> => dev_wasm,
                <<"Multipass">> => dev_multipass,
                <<"Push">> => dev_mu,
                <<"Compute">> => dev_cu,
                <<"P4">> => dev_p4
            },
        default_device_stacks => [
            {<<"data">>, {<<"read">>, [dev_p4, dev_lookup]}},
            {<<"su">>, {<<"schedule">>, [dev_p4, dev_scheduler]}},
            {<<"cu">>, {<<"execute">>, [dev_p4, dev_cu]}},
            {<<"mu">>,
                {<<"push">>, [
                    dev_p4,
                    dev_mu,
                    dev_scheduler,
                    dev_cu,
                    dev_poda,
                    dev_mu
                ]}
            }
        ],
        % Dev options
        local_store =>
            [{ao_fs_store, #{ prefix => "TEST-data" }, #{ scope => local }}],
        mode => debug,
        debug_print => false
    }.

get(Key) -> get(Key, undefined).
get(Key, Default) ->
    case maps:get(Key, ?ENV_KEYS, false) of
        false -> config_lookup(Key, Default);
        {EnvKey, ValParser, DefaultValue} when is_function(ValParser) ->
            ValParser(os:getenv(EnvKey, DefaultValue));
        {EnvKey, DefaultValue} ->
            os:getenv(EnvKey, DefaultValue)
    end.

config_lookup(Key, Default) -> maps:get(Key, config(), Default).

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
    io:format(standard_error, "==AO_DEBUG==[~pms in ~p @ ~s:~w]==> ~s~n",
        [TSDiff, self(), ModStr, Line, debug_fmt(X)]),
    X.

%debug_fmt(X) when is_binary(X) andalso byte_size(X) == 32 ->
%    lists:flatten(io_lib:format("Bin: ~s", [ar_util:encode(X)]));
debug_fmt({X, Y}) when is_atom(X) and is_atom(Y) ->
    io_lib:format("~p: ~p", [X, Y]);
debug_fmt({X, Y}) ->
    io_lib:format("~p: ~s", [X, debug_fmt(Y)]);
debug_fmt({X, Y, Z}) ->
    io_lib:format("~s, ~s, ~s", [debug_fmt(X), debug_fmt(Y), debug_fmt(Z)]);
debug_fmt({X, Y, Z, W}) ->
    io_lib:format("~s, ~s, ~s, ~s",
        [debug_fmt(X), debug_fmt(Y), debug_fmt(Z), debug_fmt(W)]);
debug_fmt(Str = [X | _]) when is_integer(X) andalso X >= 32 andalso X < 127 ->
    lists:flatten(io_lib:format("~s", [Str]));
debug_fmt(X) ->
    lists:flatten(io_lib:format("~120p", [X])).

read(ID) -> read(ID, local).
read(ID, Scope) ->
    ao_cache:read_message(ao_store:scope(ao:get(store), Scope), ar_util:id(ID)).

no_prod(X, Mod, Line) ->
    case ao:get(mode) of
        prod ->
            io:format(standard_error,
                "================ NOT PROD READY =================~n", []),
            io:format(standard_error, "~w:~w:       ~p~n", [Mod, Line, X]),
            throw(X);
        _ -> X
    end.

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

debug_wait(T, Mod, Line) ->
    c({debug_wait, T, Mod, Line}),
    receive after T -> ok end.
