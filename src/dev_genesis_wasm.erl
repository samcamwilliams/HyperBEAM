%%% @doc A device that mimics an environment suitable for `legacynet` AO 
%%% processes, using HyperBEAM infrastructure. This allows existing `legacynet`
%%% AO process definitions to be used in HyperBEAM.
-module(dev_genesis_wasm).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-export([ensure_started/0]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% @doc Initialize the device.
init(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc All the `delegated-compute@1.0` device to execute the request. We then apply
%% the `patch@1.0` device, applying any state patches that the AO process may have
%% requested.
compute(Msg, Msg2, Opts) ->
    % Validate whether the genesis-wasm feature is enabled.
    case hb_features:genesis_wasm() andalso ensure_started() of
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

%% @doc Ensure the local `genesis-wasm@1.0` is live. If it not, start it.
ensure_started() ->
    % Check if the `genesis-wasm@1.0` device is already running. The presence
    % of the registered name implies its availability.
    case hb_name:lookup(<<"genesis-wasm@1.0">>) of
        PID when is_pid(PID) ->
            % If it is, do nothing.
            true;
        undefined ->
            % The device is not running, so we need to start it.
            PID =
                spawn(
                    fun() ->
                        ?event({genesis_wasm_starting, {pid, self()}}),
                        Port =
                            open_port(
                                {spawn,
                                    "npm --prefix _build/genesis-wasm-server run dev"
                                },
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
            receive after 2000 -> ok end,
            ?event({genesis_wasm_started, {pid, PID}}),
            true
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