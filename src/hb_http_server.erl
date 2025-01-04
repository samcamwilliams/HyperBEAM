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
-export([start/0, start/1, allowed_methods/2, init/2]).
-export([start_test_node/0, start_test_node/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    hb_http:start(),
    Port = hb_opts:get(port, no_port, Opts),
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
                        Opts
                    }
                ]}
            ]
        ),
    Res = cowboy:start_clear(
        {?MODULE, Port}, 
        [{port, Port}],
        #{
            env => #{dispatch => Dispatcher},
            metrics_callback =>
                fun prometheus_cowboy2_instrumenter:observe/1,
            stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
        }
    ),
    ?event(debug, {cowboy_start_clear, {port, Port}, {res, Res}}),
    Res.

init(Req, Opts) ->
    % Parse the HTTP request into HyerBEAM's message format.
    MsgSingleton = hb_http:req_to_message(Req, Opts),
    ?event({executing_msg_from_http, MsgSingleton}),
    % Execute the message through Converge Protocol.
    {ok, RawRes} = hb_converge:resolve(MsgSingleton, Opts),
    ?event({http_result_generated, RawRes}),
    % Sign the transaction if it's not already signed.
    Signed =
        case hb_opts:get(force_signed, false, Opts) andalso hb_message:signers(RawRes) of
            [] ->
                hb_message:sign(RawRes, hb_opts:get(wallet, no_wallet, Opts));
            _ -> RawRes
        end,
    % Respond to the client.
    hb_http:reply(
        Req,
        hb_http:message_to_status(Signed),
        Signed
    ).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%%% Tests

test_opts(Opts) ->
    rand:seed(default),
    % Generate a random port number between 42000 and 62000 to use
    % for the server.
    Port = 42000 + rand:uniform(20000),
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
        cowboy,
        prometheus,
        prometheus_cowboy,
        os_mon,
        rocksdb
    ]),
    ServerOpts = test_opts(Opts),
    start(ServerOpts),
    Port = hb_opts:get(port, no_port, ServerOpts),
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