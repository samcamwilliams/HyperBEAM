-module(su_registry).
-export([start/0, start/1, find/1, server/2, get_wallet/0, get_processes/0]).

-define(DEFAULT_WALLET, "key.json").

start() -> start(?DEFAULT_WALLET).
start(WalletFile) ->
    Wallet =
        case file:read_file_info(WalletFile) of
            {ok, _} -> ar_wallet:load_keyfile(WalletFile);
            {error, _} -> ar_wallet:new_keyfile(?DEFAULT_WALLET, "su_key")
        end,
    register(?MODULE, spawn(fun() -> server(#{}, Wallet) end)).

find(ProcID) ->
    ReplyPID = self(),
    ?MODULE ! {find, ProcID, ReplyPID},
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
        {find, ProcID, ReplyPID} ->
            Process =
                case maps:find(ProcID, Registry) of
                    {ok, ExistingProcess} ->
                        case is_process_alive(ExistingProcess) of
                            true ->
                                ExistingProcess;
                            false ->
                                new_proc(ProcID, Wallet)
                        end;
                    error ->
                        new_proc(ProcID, Wallet)
                end,
            ReplyPID ! {process, Process},
            server(Registry#{ProcID => Process}, Wallet);
        {get_wallet, ReplyPID} ->
            ReplyPID ! {wallet, Wallet},
            server(Registry, Wallet);
        {get_processes, ReplyPID} ->
            ReplyPID ! {processes, maps:keys(Registry)},
            server(Registry, Wallet)
    end.

new_proc(ProcID, Wallet) ->
    spawn(fun() -> su_process:start(ProcID, Wallet) end).