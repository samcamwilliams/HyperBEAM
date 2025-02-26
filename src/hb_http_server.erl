%%% @doc A router that attaches a HTTP server to the Converge resolver.
%%% Because Converge is built to speak in HTTP semantics, this module
%%% only has to marshal the HTTP request into a message, and then
%%% pass it to the Converge resolver. 
%%% 
%%% `hb_http:reply/4' is used to respond to the client, handling the 
%%% process of converting a message back into an HTTP response.
%%% 
%%% The router uses an `Opts' message as its Cowboy initial state, 
%%% such that changing it on start of the router server allows for
%%% the execution parameters of all downstream requests to be controlled.
-module(hb_http_server).
-export([start/0, start/1, allowed_methods/2, init/2, set_opts/1, get_opts/1]).
-export([start_node/0, start_node/1, set_default_opts/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Starts the HTTP server. Optionally accepts an `Opts' message, which
%% is used as the source for server configuration settings, as well as the
%% `Opts' argument to use for all Converge resolution requests downstream.
start() ->
    ?event(http, {start_store, "mainnet-cache"}),
    Store = hb_opts:get(store, no_store, #{}),
    hb_store:start(Store),
    Loaded =
        case hb_opts:load(Loc = hb_opts:get(hb_config_location, <<"config.flat">>, #{})) of
            {ok, Conf} ->
                ?event(boot, {loaded_config, Loc, Conf}),
                Conf;
            {error, Reason} ->
                ?event(boot, {failed_to_load_config, Loc, Reason}),
                #{}
        end,
    PrivWallet =
        hb:wallet(
            hb_opts:get(
                priv_key_location,
                <<"hyperbeam-key.json">>,
                Loaded
            )
        ),
    FormattedConfig = hb_util:debug_fmt(Loaded, 2),
    io:format("~n"
        "===========================================================~n"
        "==    ██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗           ==~n"
        "==    ██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗          ==~n"
        "==    ███████║ ╚████╔╝ ██████╔╝█████╗  ██████╔╝          ==~n"
        "==    ██╔══██║  ╚██╔╝  ██╔═══╝ ██╔══╝  ██╔══██╗          ==~n"
        "==    ██║  ██║   ██║   ██║     ███████╗██║  ██║          ==~n"
        "==    ╚═╝  ╚═╝   ╚═╝   ╚═╝     ╚══════╝╚═╝  ╚═╝          ==~n"
        "==                                                       ==~n"
        "==        ██████╗ ███████╗ █████╗ ███╗   ███╗ VERSION:   ==~n"
        "==        ██╔══██╗██╔════╝██╔══██╗████╗ ████║      v~p. ==~n"
        "==        ██████╔╝█████╗  ███████║██╔████╔██║            ==~n"
        "==        ██╔══██╗██╔══╝  ██╔══██║██║╚██╔╝██║ EAT GLASS, ==~n"
        "==        ██████╔╝███████╗██║  ██║██║ ╚═╝ ██║ BUILD THE  ==~n"
        "==        ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝    FUTURE. ==~n"
        "===========================================================~n"
        "== Node activate at: ~s ==~n"
        "== Operator: ~s ==~n"
        "===========================================================~n"
        "== Config:                                               ==~n"
        "===========================================================~n"
        "   ~s~n"
        "===========================================================~n",
        [
            ?HYPERBEAM_VERSION,
            string:pad(
                lists:flatten(
                    io_lib:format(
                        "http://~s:~p",
                        [
                            hb_opts:get(host, <<"localhost">>, Loaded),
                            hb_opts:get(port, 8734, Loaded)
                        ]
                    )
                ),
                35, leading, $ 
            ),
            hb_util:human_id(ar_wallet:to_address(PrivWallet)),
            FormattedConfig
        ]
    ),
    
    start(
        Loaded#{
            priv_wallet => PrivWallet,
            store => Store,
            port => hb_opts:get(port, 8734, Loaded)
        }
    ).
start(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        prometheus,
        prometheus_cowboy,
        os_mon,
        rocksdb
    ]),
    hb:init(),
    BaseOpts = set_default_opts(Opts),
    {ok, Listener, _Port} = new_server(BaseOpts),
    {ok, Listener}.

new_server(RawNodeMsg) ->
    NodeMsg =
        maps:merge(
            hb_opts:default_message(),
            RawNodeMsg#{ only => local }
        ),
    hb_http:start(),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(
                    priv_wallet,
                    no_wallet,
                    NodeMsg
                )
            )
        ),
    % Put server ID into node message so it's possible to update current server
    % params
    NodeMsgWithID = maps:put(http_server, ServerID, NodeMsg),
    Dispatcher =
        cowboy_router:compile(
            [
                % {HostMatch, list({PathMatch, Handler, InitialState})}
                {'_', [
                    {"/", cowboy_static, {priv_file, hb, "index.html"}},
                    {
                        "/metrics/[:registry]",
                        prometheus_cowboy2_handler,
                        #{}
                    },
                    {'_', ?MODULE, ServerID}
                ]}
            ]
        ),
    ProtoOpts = #{
        env => #{dispatch => Dispatcher, node_msg => NodeMsgWithID},
        metrics_callback =>
            fun prometheus_cowboy2_instrumenter:observe/1,
        stream_handlers => [cowboy_metrics_h, cowboy_stream_h],
        max_connections => infinity,
        idle_timeout => hb_opts:get(idle_timeout, 300000, NodeMsg)
    },
    {ok, Port, Listener} =
        case Protocol = hb_opts:get(protocol, no_proto, NodeMsg) of
            http3 ->
                start_http3(ServerID, ProtoOpts, NodeMsg);
            Pro when Pro =:= http2; Pro =:= http1 ->
        		% The HTTP/2 server has fallback mode to 1.1 as necessary.
                start_http2(ServerID, ProtoOpts, NodeMsg);
            _ -> {error, {unknown_protocol, Protocol}}
        end,
    ?event(http,
        {http_server_started,
            {listener, Listener},
            {server_id, ServerID},
            {port, Port},
            {protocol, Protocol},
            {store, hb_opts:get(store, no_store, NodeMsg)}
        }
    ),
    {ok, Listener, Port}.

start_http3(ServerID, ProtoOpts, _NodeMsg) ->
    ?event(http, {start_http3, ServerID}),
    Parent = self(),
    ServerPID =
        spawn(fun() ->
            {ok, Listener} = cowboy:start_quic(
                ServerID, 
                TransOpts = #{
                    socket_opts => [
                        {certfile, "test/test-tls.pem"},
                        {keyfile, "test/test-tls.key"}
                    ]
                },
                ProtoOpts
            ),
            {ok, {_, GivenPort}} = quicer:sockname(Listener),
            ranch_server:set_new_listener_opts(
                ServerID,
                1024,
                ranch:normalize_opts(
                    maps:to_list(TransOpts#{ port => GivenPort })
                ),
                ProtoOpts,
                []
            ),
            ranch_server:set_addr(ServerID, {<<"localhost">>, GivenPort}),
            Parent ! {ok, GivenPort},
            receive stop -> stopped end
        end),
    receive {ok, GivenPort} -> {ok, GivenPort, ServerPID}
    after 2000 ->
        {error, {timeout, staring_http3_server, ServerID}}
    end.

start_http2(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http2, ServerID}),
    {ok, Listener} = cowboy:start_clear(
        ServerID,
        [
            {port, Port = hb_opts:get(port, 8734, NodeMsg)}
        ],
        ProtoOpts
    ),
    {ok, Port, Listener}.

%% @doc Entrypoint for all HTTP requests. Receives the Cowboy request option and
%% the server ID, which can be used to lookup the node message.
init(Req, ServerID) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> cors_reply(Req, ServerID);
        _ -> handle_request(Req, ServerID)
    end.

%% @doc Reply to CORS preflight requests.
cors_reply(Req, _ServerID) ->
    cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> =>
            <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>
    }, Req).

%% @doc Handle all non-CORS preflight requests as Converge requests. Execution 
%% starts by parsing the HTTP request into HyerBEAM's message format, then
%% passing the message directly to `meta@1.0` which handles calling Converge in
%% the appropriate way.
handle_request(Req, ServerID) ->
    NodeMsg = get_opts(#{ http_server => ServerID }),
    ?event(http, {http_inbound, Req}),
    % Parse the HTTP request into HyerBEAM's message format.
    ReqSingleton = hb_http:req_to_tabm_singleton(Req, NodeMsg),
    ?event(http, {parsed_singleton, ReqSingleton}),
    {ok, Res} = dev_meta:handle(NodeMsg, ReqSingleton),
    hb_http:reply(Req, Res, NodeMsg).

%% @doc Return the list of allowed methods for the HTTP server.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>], Req, State}.

%% @doc Update the `Opts' map that the HTTP server uses for all future
%% requests.
set_opts(Opts) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, Opts),
    ok = cowboy:set_env(ServerRef, node_msg, Opts).

get_opts(NodeMsg) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, NodeMsg),
    cowboy:get_env(ServerRef, node_msg, no_node_msg).

set_default_opts(Opts) ->
    % Create a temporary opts map that does not include the defaults.
    TempOpts = Opts#{ only => local },
    % Generate a random port number between 10000 and 30000 to use
    % for the server.
    Port =
        case hb_opts:get(port, no_port, TempOpts) of
            no_port ->
                rand:seed(exsplus, erlang:timestamp()),
                10000 + rand:uniform(20000);
            PassedPort -> PassedPort
        end,
    Wallet =
        case hb_opts:get(priv_wallet, no_viable_wallet, TempOpts) of
            no_viable_wallet -> ar_wallet:new();
            PassedWallet -> PassedWallet
        end,
    Store =
        case hb_opts:get(store, no_store, TempOpts) of
            no_store ->
                {hb_store_fs,
                    #{
                        prefix =>
                            <<"TEST-cache-", (integer_to_binary(Port))/binary>>
                    }
                };
            PassedStore -> PassedStore
        end,
    ?event({set_default_opts,
        {given, TempOpts},
        {port, Port},
        {store, Store},
        {wallet, Wallet}
    }),
    Opts#{
        port => Port,
        store => Store,
        priv_wallet => Wallet,
        address => hb_util:human_id(ar_wallet:to_address(Wallet)),
        force_signed => true
    }.

%% @doc Test that we can start the server, send a message, and get a response.
start_node() ->
    start_node(#{}).
start_node(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
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
    ServerOpts = set_default_opts(Opts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>.