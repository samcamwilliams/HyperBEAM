%%% Hyperbeam's core HTTP request/reply functionality. The functions in this
%%% module generally take a message request from their caller and return a
%%% response in message form, as granted by the peer. This module is mostly
%%% used by hb_client, but can also be used by other modules that need to make
%%% HTTP requests.
-module(hb_http).
-export([start/0]).
-export([get/2, get/3, post/3, post/4, request/2, request/4, request/5]).
-export([reply/3, reply/4]).
-export([message_to_status/1, status_code/1, req_to_tabm_singleton/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
    httpc:set_options([{max_keep_alive_length, 0}]),
    ok.

%% @doc Gets a URL via HTTP and returns the resulting message in deserialized
%% form.
get(Node, Opts) -> get(Node, <<"/">>, Opts).
get(Node, PathBin, Opts) when is_binary(PathBin) ->
    get(Node, #{ <<"path">> => PathBin }, Opts);
get(Node, Message, Opts) ->
    request(
        <<"GET">>,
        Node,
        hb_converge:get(<<"path">>, Message, <<"/">>, Opts),
        Message,
        Opts
    ).

%% @doc Posts a message to a URL on a remote peer via HTTP. Returns the
%% resulting message in deserialized form.
post(Node, Message, Opts) ->
    post(Node,
        hb_converge:get(
            <<"path">>,
            Message,
            <<"/">>,
            Opts#{ topic => converge_internal }
        ),
        Message,
        Opts
    ).
post(Node, Path, Message, Opts) ->
    case request(<<"POST">>, Node, Path, Message, Opts) of
        {ok, Res} ->
            ?event(http, {post_response, Res}),
            {ok, Res};
        Error -> Error
    end.

%% @doc Posts a binary to a URL on a remote peer via HTTP, returning the raw
%% binary body.
request(Message, Opts) ->
    % Special case: We are not given a peer and a path, so we need to
    % preprocess the URL to find them.
    {ok, Method, Peer, Path, MessageToSend} = message_to_request(Message, Opts),
    request(Method, Peer, Path, MessageToSend, Opts).
request(Method, Peer, Path, Opts) ->
    request(Method, Peer, Path, #{}, Opts).
request(Method, Config, Path, Message, Opts) when is_map(Config) ->
    multirequest(Config, Method, Path, Message, Opts);
request(Method, Peer, Path, RawMessage, Opts) ->
    Req =
        prepare_request(
            hb_opts:get(format, http, Opts),
            Method,
            Peer,
            Path,
            RawMessage,
            Opts
        ),
    case ar_http:req(Req, Opts) of
        {ok, Status, Headers, Body} when Status >= 200, Status < 400 ->
            ?event(
                {
                    http_response,
                    {req, Req},
                    {response,
                        #{
                            status => Status,
                            headers => Headers,
                            body => Body
                        }
                    }
                }
            ),
            HeaderMap = maps:from_list(Headers),
            NormHeaderMap = hb_converge:normalize_keys(HeaderMap),
            {
                case Status of
                    201 -> created;
                    _ -> ok
                end,
                case maps:get(<<"content-type">>, NormHeaderMap, undefined) of
                    <<"application/x-ans-104">> ->
                        ar_bundles:deserialize(Body);
                    _ ->
                        hb_message:convert(
                            HeaderMap#{
                                <<"body">> => Body
                            },
                            <<"structured@1.0">>,
                            <<"httpsig@1.0">>,
                            Opts
                        )
                end
            };
        {ok, Status, _Headers, Body} when Status == 400 ->
            ?event(
                {http_got_client_error,
                    {req, Req},
                    {response, #{status => Status, body => Body}}
                }),
            {error, Body};
        {ok, Status, _Headers, Body} when Status > 400 ->
            ?event(
                {http_got_server_error,
                    {req, Req},
                    {response, #{status => Status, body => Body}}
                }
            ),
            {unavailable, Body};
        Response ->
            ?event(
                {http_error,
                    {req, Req},
                    {response, Response}
                }
            ),
            Response
    end.

%% @doc Given a message, return the information needed to make the request.
message_to_request(M, Opts) ->
    Method = hb_converge:get(<<"method">>, M, <<"GET">>, Opts),
    % We must remove the path and host from the message, because they are not
    % valid for outbound requests. The path is retrieved from the route, and
    % the host should already be known to the caller.
    MsgWithoutMeta = maps:without([<<"path">>, <<"host">>], M),
    % Get the route for the message
    case dev_router:route(#{}, M, Opts) of
        {ok, URL} when is_binary(URL) ->
            % The request is a direct HTTP URL, so we need to split the
            % URL into a host and path.
            URI = uri_string:parse(URL),
            ?event(http, {parsed_uri, {uri, {explicit, URI}}}),
            Port =
                case maps:get(port, URI, undefined) of
                    undefined ->
                        % If no port is specified, use 80 for HTTP and 443
                        % for HTTPS.
                        case URL of
                            <<"https", _/binary>> -> <<"443">>;
                            _ -> <<"80">>
                        end;
                    X -> integer_to_binary(X)
                end,
            Protocol = maps:get(scheme, URI, <<"https">>),
            Host = maps:get(host, URI, <<"localhost">>),
            Node = << Protocol/binary, "://", Host/binary, ":", Port/binary  >>,
            PathParts = [maps:get(path, URI, <<"/">>)] ++
                case maps:get(query, URI, <<>>) of
                    <<>> -> [];
                    Query -> [<<"?", Query/binary>>]
                end,
            Path = iolist_to_binary(PathParts),
            ?event(http, {relay, {node, Node}, {method, Method}, {path, Path}}),
            {ok, Method, Node, Path, MsgWithoutMeta};
        {ok, Route} ->
            % The result is a route, so we leave it to `request` to handle it.
            Path = hb_converge:get(<<"path">>, M, <<"/">>, Opts),
            {ok, Method, Route, Path, MsgWithoutMeta};
        {error, Reason} ->
            {error, {no_viable_route, Reason}}
    end.

%% @doc Turn a set of request arguments into a request message, formatted in the
%% preferred format.
prepare_request(Format, Method, Peer, Path, RawMessage, Opts) ->
    Message = hb_converge:normalize_keys(RawMessage),
    BinPeer = if is_binary(Peer) -> Peer; true -> list_to_binary(Peer) end,
    BinPath = hb_path:normalize(hb_path:to_binary(Path)),
    ReqBase = #{ peer => BinPeer, path => BinPath, method => Method },
    case Format of
        http ->
            FullEncoding = #{ <<"body">> := Body } =
                hb_message:convert(Message, <<"httpsig@1.0">>, Opts),
            Headers = maps:without([<<"body">>], FullEncoding),
            maps:merge(ReqBase, #{ headers => Headers, body => Body });
        ans104 ->
            ReqBase#{
                headers => [{<<"content-type">>, <<"application/x-ans-104">>}],
                body => ar_bundles:serialize(hb_message:convert(Message, tx, #{}))
            }
    end.

%% @doc Dispatch the same HTTP request to many nodes. Can be configured to
%% await responses from all nodes or just one, and to halt all requests after
%% after it has received the required number of responses, or to leave all
%% requests running until they have all completed. Default: Race for first
%% response.
%%
%% Expects a config message of the following form:
%%      /Nodes/1..n: Hostname | #{ hostname => Hostname, address => Address }
%%      /Responses: Number of responses to gather
%%      /Stop-After: Should we stop after the required number of responses?
%%      /Parallel: Should we run the requests in parallel?
multirequest(Config, Method, Path, Message, Opts) ->
    Nodes = hb_converge:get(<<"peers">>, Config, #{}, Opts),
    Responses = hb_converge:get(<<"responses">>, Config, 1, Opts),
    StopAfter = hb_converge:get(<<"stop-after">>, Config, true, Opts),
    case hb_converge:get(<<"parallel">>, Config, false, Opts) of
        false ->
            serial_multirequest(
                Nodes, Responses, Method, Path, Message, Opts);
        true ->
            parallel_multirequest(
                Nodes, Responses, StopAfter, Method, Path, Message, Opts)
    end.

serial_multirequest(_Nodes, 0, _Method, _Path, _Message, _Opts) -> [];
serial_multirequest([Node | Nodes], Remaining, Method, Path, Message, Opts) ->
    case request(Method, Node, Path, Message, Opts) of
        {Status, Res} when Status == ok; Status == error ->
            [
                {Status, Res}
            |
                serial_multirequest(Nodes, Remaining - 1, Method, Path, Message, Opts)
            ];
        _ ->
            serial_multirequest(Nodes, Remaining, Method, Path, Message, Opts)
    end.

%% @doc Dispatch the same HTTP request to many nodes in parallel.
parallel_multirequest(Nodes, Responses, StopAfter, Method, Path, Message, Opts) ->
    Ref = make_ref(),
    Parent = self(),
    Procs = lists:map(
        fun(Node) ->
            spawn(
                fun() ->
                    Res = request(Method, Node, Path, Message, Opts),
                    receive no_reply -> stopping
                    after 0 -> Parent ! {Ref, self(), Res}
                    end
                end
            )
        end,
        Nodes
    ),
    parallel_responses([], Procs, Ref, Responses, StopAfter, Opts).

%% @doc Collect the necessary number of responses, and stop workers if
%% configured to do so.
parallel_responses(Res, Procs, Ref, 0, false, _Opts) ->
    lists:foreach(fun(P) -> P ! no_reply end, Procs),
    empty_inbox(Ref),
    {ok, Res};
parallel_responses(Res, Procs, Ref, 0, true, _Opts) ->
    lists:foreach(fun(P) -> exit(P, kill) end, Procs),
    empty_inbox(Ref),
    Res;
parallel_responses(Res, Procs, Ref, Awaiting, StopAfter, Opts) ->
    receive
        {Ref, Pid, {Status, NewRes}} when Status == ok; Status == error ->
            parallel_responses(
                [NewRes | Res],
                lists:delete(Pid, Procs),
                Ref,
                Awaiting - 1,
                StopAfter,
                Opts
            );
        {Ref, Pid, _} ->
            parallel_responses(
                Res,
                lists:delete(Pid, Procs),
                Ref,
                Awaiting,
                StopAfter,
                Opts
            )
    end.

%% @doc Empty the inbox of the current process for all messages with the given
%% reference.
empty_inbox(Ref) ->
    receive
        {Ref, _} -> empty_inbox(Ref)
    after 0 -> ok
    end.

%% @doc Reply to the client's HTTP request with a message.
reply(Req, Message, Opts) ->
    reply(Req, message_to_status(Message), Message, Opts).
reply(Req, Status, RawMessage, Opts) ->
    Message = hb_converge:normalize_keys(RawMessage),
    {ok, EncodedHeaders, EncodedBody} = prepare_reply(Message, Opts),
    ?event(http,
        {replying,
            {status, Status},
            {path, maps:get(<<"path">>, Req, undefined_path)},
            {raw_message, RawMessage},
            {enc_headers, EncodedHeaders},
            {enc_body, EncodedBody}
        }
    ),
    % Cowboy handles cookies in headers separately, so we need to manipulate
    % the request to set the cookies such that they will be sent over the wire
    % unmodified.
    SetCookiesReq =
        case maps:get(<<"set-cookie">>, EncodedHeaders, undefined) of
            undefined -> Req#{ resp_headers => EncodedHeaders };
            Cookies ->
                Req#{
                    resp_headers => EncodedHeaders,
                    resp_cookies => #{ <<"__HB_SET_COOKIE">> => Cookies }
                }
        end,
    Req2 = cowboy_req:stream_reply(Status, #{}, SetCookiesReq),
    Req3 = cowboy_req:stream_body(EncodedBody, nofin, Req2),
    {ok, Req3, no_state}.

%% @doc Generate the headers and body for a HTTP response message.
prepare_reply(Message, Opts) ->
    case hb_opts:get(format, http, Opts) of
        http ->
            EncMessage =
                hb_message:convert(
                    Message,
                    <<"httpsig@1.0">>,
                    <<"structured@1.0">>,
                    #{ topic => converge_internal }
                ),
            {ok,
                maps:without([<<"body">>], EncMessage),
                maps:get(<<"body">>, EncMessage, <<>>)
            };
        ans104 ->
            {ok,
                #{
                    <<"content-type">> => <<"application/octet-stream">>
                },
                ar_bundles:serialize(hb_message:convert(Message, tx, Opts))
            }
    end.

%% @doc Get the HTTP status code from a transaction (if it exists).
message_to_status(Item) ->
    case dev_message:get(<<"status">>, Item) of
        {ok, RawStatus} ->
            case is_integer(RawStatus) of
                true -> RawStatus;
                false -> binary_to_integer(RawStatus)
            end;
        _ -> 200
    end.

%% @doc Convert an HTTP status code to an atom.
status_code(ok) -> 200;
status_code(error) -> 400;
status_code(created) -> 201;
status_code(unavailable) -> 503;
status_code(Status) -> Status.

%% @doc Convert a cowboy request to a normalized message.
req_to_tabm_singleton(Req, Opts) ->
    case cowboy_req:header(<<"content-type">>, Req) of
        <<"application/x-ans-104">> ->
            {ok, Body} = read_body(Req),
            hb_message:convert(ar_bundles:deserialize(Body), <<"structured@1.0">>, <<"ans104@1.0">>, Opts);
        _ ->
            http_sig_to_tabm_singleton(Req, Opts)
    end.

http_sig_to_tabm_singleton(Req = #{ headers := RawHeaders }, Opts) ->
    {ok, Body} = read_body(Req),
    SignedHeaders = remove_unsigned_fields(RawHeaders, Opts),
    HeadersWithPath =
        case maps:get(<<"path">>, RawHeaders, undefined) of
            undefined ->
                SignedHeaders#{
                    <<"path">> =>
                        iolist_to_binary(
                            cowboy_req:uri(
                                Req,
                                #{
                                    host => undefined,
                                    port => undefined,
                                    scheme => undefined
                                }
                            )
                    )
                };
            _ -> SignedHeaders
        end,
    HeadersWithMethod = HeadersWithPath#{ <<"method">> => cowboy_req:method(Req) },
    ?event(http, {recvd_req_with_headers, {raw, RawHeaders}, {headers, HeadersWithMethod}}),
    HTTPEncoded =
        (maps:without([<<"content-length">>], HeadersWithMethod))#{
            <<"body">> => Body
        },
    dev_codec_httpsig_conv:from(HTTPEncoded).

remove_unsigned_fields(RawHeaders, Opts) ->
    ForceSignedRequests = hb_opts:get(force_signed_requests, false, Opts),
    Sig = maps:get(<<"signature">>, RawHeaders, undefined),
    case ForceSignedRequests orelse Sig /= undefined of
        true -> do_remove_unsigned_fields(RawHeaders);
        false -> RawHeaders
    end.

do_remove_unsigned_fields(RawHeaders) ->
    % Every signature ought to have the same signature base
    % And so we just parse out the first signature input,
    % and use it to determine the signed components, stripping
    % the components that are not signed
    [{_SigInputName, SigInput} | _] = dev_codec_structured_conv:parse_dictionary(
        maps:get(<<"signature-input">>, RawHeaders)
    ),
    {list, ComponentIdentifiers, _SigParams} = SigInput,
    BinComponentIdentifiers = lists:map(
        fun({item, {_Kind, CI}, _Params}) -> CI end,
        ComponentIdentifiers    
    ),
    SignedHeaders = maps:with(
        [<<"signature">>, <<"signature-input">>] ++ BinComponentIdentifiers,
        RawHeaders
    ),
    ?event(http, {sanitizing, {component_identifiers, BinComponentIdentifiers}, {sanitized_headers, SignedHeaders}}),
    SignedHeaders.

%% @doc Helper to grab the full body of a HTTP request, even if it's chunked.
read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

%%% Tests

% id_case_is_preserved_test() ->
%     URL = hb_http_server:start_node(),
%     TestID = hb_util:human_id(crypto:strong_rand_bytes(32)),
%     {ok, Res} =
%         post(
%             URL,
%             #{
%                 TestID => <<"value1">>,
%                 <<"path">> => <<TestID/binary>>
%             },
%             #{}
%         ),
%     ?assertEqual(<<"value1">>, hb_converge:get(TestID, Res, #{})).

simple_converge_resolve_unsigned_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"/key1">>, <<"key1">> => <<"Value1">> },
    {ok, Res} = post(URL, TestMsg, #{}),
    ?assertEqual(<<"Value1">>, hb_converge:get(<<"body">>, Res, #{})).

simple_converge_resolve_signed_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"/key1">>, <<"key1">> => <<"Value1">> },
    Wallet = hb:wallet(),
    {ok, Res} =
        post(
            URL,
            hb_message:attest(TestMsg, Wallet),
            #{}
        ),
    ?assertEqual(<<"Value1">>, hb_converge:get(<<"body">>, Res, #{})).

nested_converge_resolve_test() ->
    URL = hb_http_server:start_node(),
    Wallet = hb:wallet(),
    {ok, Res} =
        post(
            URL,
            hb_message:attest(#{
                <<"path">> => <<"/key1/key2/key3">>,
                <<"key1">> =>
                    #{<<"key2">> =>
                        #{
                            <<"key3">> => <<"Value2">>
                        }
                    }
            }, Wallet),
            #{}
        ),
    ?assertEqual(<<"Value2">>, hb_converge:get(<<"body">>, Res, #{})).

wasm_compute_request(ImageFile, Func, Params) ->
    wasm_compute_request(ImageFile, Func, Params, <<"">>).
wasm_compute_request(ImageFile, Func, Params, ResultPath) ->
    {ok, Bin} = file:read_file(ImageFile),
    Wallet = hb:wallet(),
    hb_message:attest(#{
        <<"path">> => <<"/init/compute/results", ResultPath/binary>>,
        <<"device">> => <<"WASM-64@1.0">>,
        <<"wasm-function">> => Func,
        <<"wasm-params">> => Params,
        <<"body">> => Bin
    }, Wallet).

run_wasm_unsigned_test() ->
    Node = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0]),
    {ok, Res} = post(Node, Msg, #{}),
    ?assertEqual(6.0, hb_converge:get(<<"output/1">>, Res, #{})).

run_wasm_signed_test() ->
    URL = hb_http_server:start_node(#{force_signed => true}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_converge:get(<<"output/1">>, Res, #{})).

get_deep_unsigned_wasm_state_test() ->
    URL = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(
        <<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_converge:get(<<"/output/1">>, Res, #{})).

get_deep_signed_wasm_state_test() ->
    URL = hb_http_server:start_node(#{force_signed => true}),
    Msg = wasm_compute_request(
        <<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"/output">>),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_converge:get(<<"1">>, Res, #{})).