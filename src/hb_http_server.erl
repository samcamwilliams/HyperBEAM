%%% @doc A router that attaches a HTTP server to the Converge resolver.
%%% Because Converge is built to speak in HTTP semantics, this module
%%% only has to marshal the HTTP request into a message, and then
%%% pass it to the Converge resolver. 
%%% 
%%% `hb_http:reply/3' is used to respond to the client, handling the 
%%% process of converting a message back into an HTTP response.
%%% 
%%% The router uses an `Opts` message as its Cowboy initial state, 
%%% such that changing it on start of the router server allows for
%%% the execution parameters of all downstream requests to be controlled.
-module(hb_http_server).
-export([start/0, start/1, allowed_methods/2, init/2, set_opts/1]).
-export([start_test_node/0, start_test_node/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Starts the HTTP server. Optionally accepts an `Opts` message, which
%% is used as the source for server configuration settings, as well as the
%% `Opts` argument to use for all Converge resolution requests downstream.
start() ->
    start(#{
        store => hb_opts:get(store),
        wallet => hb_opts:get(wallet),
        port => 8734
    }).

start(Opts) ->
    {ok, Listener, _Port} = new_server(Opts),
    {ok, Listener}.

new_server(Opts) ->
    hb_http:start(),
    ServerID = rand:uniform(1000000000),
    Dispatcher =
        cowboy_router:compile(
            [
                % {HostMatch, list({PathMatch, Handler, InitialState})}
                {'_', [
                    {
                        "/metrics/[:registry]",
                        prometheus_cowboy2_handler,
                        #{}
                    },
                    {
                        '_',
                        ?MODULE,
                        % The default opts for executions from the HTTP API.
                        % We force a specific store, wallet, and that 
                        % hb_converge should return a regardless of whether 
                        % the result comes wrapped in one or not.
                        ServerID
                    }
                ]}
            ]
        ),
    {ok, Port, Listener} =
        case Protocol = hb_opts:get(protocol, no_proto, Opts) of
            http3 -> start_http3(ServerID, Dispatcher, Opts);
            http2 -> start_http2(ServerID, Dispatcher, Opts)
        end,
    ?event(debug,
        {http_server_started,
            {listener, Listener},
            {server_id, ServerID},
            {port, Port},
            {protocol, Protocol}
        }
    ),
    {ok, Listener, Port}.

start_http3(ServerID, Dispatcher, Opts) ->
    ?event(debug, {start_http3, ServerID}),
    {ok, Listener} = cowboy:start_quic(
        ServerID, 
        TransOpts = #{
            socket_opts => [
                {certfile, "test/test-tls.pem"},
                {keyfile, "test/test-tls.key"}
            ]
        },
        ProtoOpts = #{
            env => #{dispatch => Dispatcher, opts => Opts},
            metrics_callback =>
                fun prometheus_cowboy2_instrumenter:observe/1,
            stream_handlers => [cowboy_metrics_h, cowboy_stream_h],
            verify => verify_none
        }
    ),
    {ok, {_, GivenPort}} = quicer:sockname(Listener),
    ranch_server:set_new_listener_opts(
        ServerID,
        1024,
        ranch:normalize_opts(maps:to_list(TransOpts#{ port => GivenPort })),
        ProtoOpts,
        []
    ),
    ranch_server:set_addr(ServerID, {<<"localhost">>, GivenPort}),
    {ok, GivenPort, Listener}.

start_http2(ServerID, Dispatcher, Opts) ->
    ?event(debug, {start_http2, ServerID}),
    {ok, Listener} = cowboy:start_tls(
        ServerID,
        [
            {port, Port = hb_opts:get(port, 8734, Opts)},
            {alpn_preferred_protocols, [<<"h2">>]},
            {certfile, "test/test-tls.pem"},
            {keyfile, "test/test-tls.key"},
            {verify, verify_none}
        ],
        #{env => #{dispatch => Dispatcher, opts => Opts}}
    ),
    {ok, Port, Listener}.

init(Req, ServerID) ->
    Opts = cowboy:get_env(ServerID, opts, no_opts),
    % Parse the HTTP request into HyerBEAM's message format.
    MsgSingleton = hb_http:req_to_message(Req, Opts),
    ?event(http, {http_inbound, MsgSingleton}),
    % Execute the message through Converge Protocol.
    {ok, RawRes} = hb_converge:resolve(MsgSingleton, Opts),
    % Sign the transaction if it's not already signed.
    IsForceSigned = hb_opts:get(force_signed, false, Opts),
    Signed =
        case IsForceSigned andalso hb_message:signers(RawRes) of
            [] ->
                hb_message:sign(
                    RawRes, hb_opts:get(wallet, no_wallet, Opts));
            _ -> RawRes
        end,
    % Respond to the client.
    hb_http:reply(
        Req,
        hb_http:message_to_status(Signed),
        Signed
    ).

%% @doc Return the complete Ranch ETS table for the node for debugging.
ranch_ets() ->
    case ets:info(ranch_server) of
        undefined -> [];
        _ -> ets:tab2list(ranch_server)
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%% @doc Update the `Opts` map that the HTTP server uses for all future
%% requests.
set_opts(Opts) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, Opts),
    cowboy:set_env(ServerRef, opts, Opts).

%%% Tests

test_opts(Opts) ->
    rand:seed(default),
    % Generate a random port number between 42000 and 62000 to use
    % for the server.
    Port = 10000 + rand:uniform(20000),
    Wallet = ar_wallet:new(),
    Opts#{
        % Generate a random port number between 8000 and 9000.
        port => Port,
        store =>
            [
                {hb_store_fs,
                    #{prefix => "TEST-cache-" ++ integer_to_list(Port)}
                }
            ],
        wallet => Wallet
    }.

%% @doc Test that we can start the server, send a message, and get a response.
start_test_node() ->
    start_test_node(#{}).
start_test_node(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        debugger,
        ranch,
        cowboy,
        gun,
        prometheus,
        prometheus_cowboy,
        os_mon,
        rocksdb
    ]),
    hb:init(),
    hb_sup:start_link(Opts),
    ServerOpts = test_opts(Opts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>.

raw_http_access_test() ->
    URL = start_test_node(),
    TX =
        ar_bundles:serialize(
            hb_message:convert(
                #{
                    path => <<"Key1">>,
                    <<"Key1">> => #{ <<"Key2">> => <<"Value1">> }
                },
                tx,
                converge,
                #{}
            )
        ),
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(
            post,
            {iolist_to_binary(URL), [], "application/octet-stream", TX},
            [],
            [{body_format, binary}]
        ),
    Msg = hb_message:convert(ar_bundles:deserialize(Body), converge, tx, #{}),
    ?assertEqual(<<"Value1">>, hb_converge:get(<<"Key2">>, Msg, #{})).