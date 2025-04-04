%%% @doc A device that mimics an environment suitable for `legacynet' AO 
%%% processes, using HyperBEAM infrastructure. This allows existing `legacynet'
%%% AO process definitions to be used in HyperBEAM.
-module(dev_genesis_wasm).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%%% Timeout for legacy CU status check.
-define(STATUS_TIMEOUT, 100).

%% @doc Initialize the device.
init(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc All the `delegated-compute@1.0' device to execute the request. We then apply
%% the `patch@1.0' device, applying any state patches that the AO process may have
%% requested.
compute(Msg, Msg2, Opts) ->
    % Validate whether the genesis-wasm feature is enabled.
    case ensure_started(Opts) of
        true ->
            % Resolve the `delegated-compute@1.0` device.
            case hb_ao:resolve(Msg, {as, <<"delegated-compute@1.0">>, Msg2}, Opts) of
                {ok, Msg3} ->
                    % Resolve the `patch@1.0` device.
                    {ok, Msg4} =
                        hb_ao:resolve(
                            Msg3,
                            {as, <<"patch@1.0">>, Msg2},
                            Opts
                        ),
                    % Return the patched message.
                    {ok, Msg4};
                {error, Error} ->
                    % Return the error.
                    {error, Error}
            end;
        false ->
            % Return an error if the genesis-wasm feature is disabled.
            {error, #{
                <<"status">> => 500,
                <<"message">> =>
                    <<"HyperBEAM was not compiled with genesis-wasm@1.0 on "
                        "this node.">>
            }}
    end.

%% @doc Ensure the local `genesis-wasm@1.0' is live. If it not, start it.
ensure_started(Opts) ->
    % Check if the `genesis-wasm@1.0` device is already running. The presence
    % of the registered name implies its availability.
    ?event({ensure_started, genesis_wasm, self()}),
    case is_genesis_wasm_server_running(Opts) orelse (hb_features:genesis_wasm() andalso is_pid(hb_name:lookup(<<"genesis-wasm@1.0">>))) of
        true ->
            % If it is, do nothing.
            true;
        false ->
			% Create genesis_wasm cache dir, if it does not exist.
			[#{<<"prefix">> := CacheDir} | _] = hb_opts:get(store),
			WalletFile = hb_opts:get(priv_key_location, no_viable_key_location, Opts),
			DatabaseDir = io_lib:format(
				"~s/genesis-wasm-cache",
				[CacheDir]
			),
			DatabaseUrl = io_lib:format(
				"~s/ao-cache",
				[DatabaseDir]
			),
			Command = io_lib:format(
				"sh -c 'DB_URL=../../~s NODE_CONFIG_ENV=development WALLET_FILE=../../~s HB_URL=http://localhost:10000 UNIT_MODE=hbu PORT=6363 npm --prefix _build/genesis-wasm-server run dev'",
				[DatabaseUrl, WalletFile]
			),
			filelib:ensure_path(DatabaseDir),
			?event({genesis_wasm_command, {Command}}),
			% The device is not running, so we need to start it.
            PID =
                spawn(
                    fun() ->
                        ?event({genesis_wasm_starting, {pid, self()}}),
                        Port =
                            open_port(
                                {spawn, lists:flatten(Command)},
                                [binary, use_stdio, stderr_to_stdout]
                            ),
                        ?event({genesis_wasm_port_opened, {port, Port}}),
                        true = port_command(Port, iolist_to_binary(<<>>)),
                        ?event({genesis_wasm_port_command_sent, {port, Port}}),
                        collect_events(Port)
                    end
                ),
            hb_name:register(<<"genesis-wasm@1.0">>, PID),
            ?event({genesis_wasm_starting, {pid, PID}}),
            % Wait for the device to start.
            receive after 5000 -> ok end,
            ?event({genesis_wasm_started, {pid, PID}}),
            is_genesis_wasm_server_running(Opts)
    end.

is_genesis_wasm_server_running(Opts) ->
    ?event({genesis_wasm_checking_status, self()}),
    Parent = self(),
    PID = spawn(
        fun() ->
            ?event({genesis_wasm_checking_status, {pid, self()}}),
            Parent ! {ok, self(), status(Opts)}
        end
    ),
    receive
        {ok, PID, Status} -> Status
    after ?STATUS_TIMEOUT ->
        ?event({genesis_wasm_status_check, {timeout, {pid, PID}}}),
        erlang:exit(PID, kill),
        false
    end.

status(Opts) ->
    try hb_http:get(<<"http://localhost:6363/">>, Opts) of
        {ok, Res} ->
            ?event({genesis_wasm_status_check, {res, Res}}),
            true;
        Err ->
            ?event({genesis_wasm_status_check, {err, Err}}),
            false
    catch
        _:Err ->
            ?event({genesis_wasm_status_check, {connect_error, Err}}),
            false
    end.

%% @doc Collect events from the port and log them.
collect_events(Port) ->
    receive
        {Port, {data, Data}} ->
            ?event(genesis_wasm_event, {data, Data}),
            collect_events(Port);
        stop ->
            port_close(Port),
            ?event(genesis_wasm_stopped, {pid, self()}),
            ok
    end.

%% @doc Normalize the device.
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc Snapshot the device.
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.