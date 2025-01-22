%%% @doc This module implements the relay device, which is responsible for
%%% relaying messages between nodes and other HTTP(S) endpoints.
%%%
%%% It can be called in either `call` or `cast` mode. In `call` mode, it
%%% returns a `{ok, Result}` tuple, where `Result` is the response from the 
%%% remote peer to the message sent. In `cast` mode, the invocation returns
%%% immediately, and the message is relayed asynchronously. No response is given
%%% and the device returns `{ok, <<"OK">>}`.
-module(dev_relay).
-export([call/3, cast/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Execute a `call` request, relaying the request message exactly as given
%% in the `M2` argument. The response from the peer is returned as the result.
call(_M1, M2, Opts) ->
    % Get the route for the message
    case dev_router:route(#{}, M2, Opts) of
        {ok, URL} ->
            % Send the message to the node
            case hb_converge:get(<<"path">>, M2, Opts) of
                <<"http", Sec, _/binary>> ->
                    % The request is a direct HTTP URL, so we need to split the
                    % URL into a host and path.
                    URI = uri_string:parse(URL),
                    Port =
                        case maps:get(port, URI, undefined) of
                            undefined ->
                                % If no port is specified, use 80 for HTTP and 443
                                % for HTTPS.
                                case Sec of
                                    $s -> <<"443">>;
                                    _ -> <<"80">>
                                end;
                            X -> integer_to_binary(X)
                        end,
                    Host = maps:get(host, URI, <<"localhost">>),
                    _Protocol = maps:get(scheme, URI, <<"http">>), % Fixme: Remove if not needed
                    Node = << Host/binary, ":", Port/binary  >>,
                    Method = hb_converge:get(<<"method">>, M2, Opts),
                    Path = maps:get(path, URI, <<"/">>),
                    ?event({relay, {node, Node}, {method, Method}, {path, Path}}),
                    hb_http:request(
                        Method,
                        Node,
                        Path,
                        M2,
                        Opts
                    );
                _ ->
                    {error, {unsupported_protocol, URL}}
            end;
        {error, Reason} ->
            {error, {no_viable_route, Reason}}
    end.

%% @doc Execute a `cast` request, relaying the request message exactly as given
%% in the `M2` argument asynchronously -- without waiting for a response.
cast(_M1, M2, Opts) ->
    % Check that we can route the message, before forking asynchronously
    case dev_router:route(#{}, M2, Opts) of
        {ok, Node} ->
            % Spawn a new process to send the message to the node
            spawn(fun() -> hb_http:post(Node, M2, Opts) end),
            {ok, <<"OK">>};
        {error, Reason} ->
            {error, {no_viable_route, Reason}}
    end.

%%% Tests

call_get_test() ->
    application:ensure_all_started([hb]),
    {ok, #{<<"body">> := Body}} =
        call(
            #{},
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"https://www.google.com/">>
            },
            #{ protocol => http2 }
        ),
    ?assertEqual(true, byte_size(Body) > 10_000).