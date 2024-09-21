-module(su_registry).
-export([start/0, start/1, find/1, find/2, server/2, get_wallet/0, get_processes/0]).

-include("include/ar.hrl").

start() -> start(ao:get(key_location)).
start(WalletFile) ->
    Wallet =
        case file:read_file_info(WalletFile) of
            {ok, _} -> ar_wallet:load_keyfile(WalletFile);
            {error, _} -> ar_wallet:new_keyfile(?DEFAULT_KEY_TYPE, ao:get(key_location))
        end,
    register(?MODULE, spawn(fun() -> server(#{}, Wallet) end)).

find(ProcID) -> find(ProcID, false);
find(ProcID, GenIfNotHosted) ->
    ReplyPID = self(),
    ?MODULE ! {find, ProcID, ReplyPID, GenIfNotHosted},
    receive
        {process, Process} -> Process
    end.

get_wallet() ->
    ?MODULE ! {get_wallet, self()},
    receive
        {wallet, Wallet} -> Wallet
    end.

get_processes() ->
    ?MODULE ! {get_processes, self()},
    receive
        {processes, Processes} -> Processes
    end.

server(Registry, Wallet) ->
    receive
        {find, ProcID, ReplyPID, GenIfNotHosted} ->
            Process =
                case maps:find(ProcID, Registry) of
                    {ok, ExistingProcess} ->
                        case is_process_alive(ExistingProcess) of
                            true ->
                                ExistingProcess;
                            false ->
                                maybe_new_proc(ProcID, Wallet, GenIfNotHosted)
                        end;
                    error ->
                        maybe_new_proc(ProcID, Wallet, GenIfNotHosted)
                end,
            ReplyPID ! {process, Process},
            server(
                case Process of
                    not_found -> Registry;
                    NewProcess -> Registry#{ProcID => Process}
                end,
                Wallet
            );
        {get_wallet, ReplyPID} ->
            ReplyPID ! {wallet, Wallet},
            server(Registry, Wallet);
        {get_processes, ReplyPID} ->
            ReplyPID ! {processes, maps:keys(Registry)},
            server(Registry, Wallet)
    end.

maybe_new_proc(_, _, false) -> not_found;
maybe_new_proc(ProcID, Wallet, _) ->
    spawn(fun() -> su_process:start(ProcID, Wallet) end).