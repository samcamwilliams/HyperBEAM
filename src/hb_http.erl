%%% Hyperbeam's core HTTP request/reply functionality. The functions in this
%%% module generally take a message request from their caller and return a
%%% response in message form, as granted by the peer. This module is mostly
%%% used by hb_client, but can also be used by other modules that need to make
%%% HTTP requests.
-module(hb_http).
-export([start/0]).
-export([get/2, get/3, post/3, post/4, request/2, request/4, request/5]).
-export([message_to_request/2, reply/4, accept_to_codec/2]).
-export([req_to_tabm_singleton/3]).
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
        hb_ao:get(<<"path">>, Message, <<"/">>, Opts),
        Message,
        Opts
    ).

%% @doc Posts a message to a URL on a remote peer via HTTP. Returns the
%% resulting message in deserialized form.
post(Node, Path, Opts) when is_binary(Path) ->
    post(Node, #{ <<"path">> => Path }, Opts);
post(Node, Message, Opts) ->
    post(Node,
        hb_ao:get(
            <<"path">>,
            Message,
            <<"/">>,
            Opts#{ topic => ao_internal }
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
    {ok, Method, Peer, Path, MessageToSend, NewOpts} =
        message_to_request(Message, Opts),
    request(Method, Peer, Path, MessageToSend, NewOpts).
request(Method, Peer, Path, Opts) ->
    request(Method, Peer, Path, #{}, Opts).
request(Method, Config = #{ <<"nodes">> := Nodes }, Path, Message, Opts) when is_list(Nodes) ->
    % The request has a `route' (see `dev_router' for more details), so we use the
    % `multirequest' functionality, rather than a single request.
    multirequest(Config, Method, Path, Message, Opts);
request(Method, #{ <<"opts">> := ReqOpts, <<"uri">> := URI }, _Path, Message, Opts) ->
    % The request has a set of additional options, so we apply them to the
    % request.
    MergedOpts =
        hb_maps:merge(
            Opts,
            hb_opts:mimic_default_types(ReqOpts, new_atoms, Opts),
            Opts
        ),
    % We also recalculate the request. The order of precidence here is subtle:
    % We favor the args given to the function, but the URI rules take precidence
    % over that.
    {ok, NewMethod, Node, NewPath, NewMsg, NewOpts} =
        message_to_request(
            Message#{ <<"path">> => URI, <<"method">> => Method },
            MergedOpts
        ),
    request(NewMethod, Node, NewPath, NewMsg, NewOpts);
request(Method, Peer, Path, RawMessage, Opts) ->
    ?event({request, {method, Method}, {peer, Peer}, {path, Path}, {message, RawMessage}}),
    Req =
        prepare_request(
            hb_maps:get(
                <<"codec-device">>,
                RawMessage,
                <<"httpsig@1.0">>,
                Opts
            ),
            Method,
            Peer,
            Path,
            RawMessage,
            Opts
        ),
    StartTime = os:system_time(millisecond),
    % Perform the HTTP request.
    {_ErlStatus, Status, Headers, Body} = hb_http_client:req(Req, Opts),
    % Process the response.
    EndTime = os:system_time(millisecond),
    ?event(http_outbound,
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
        },
        Opts
    ),
    % Convert the set-cookie headers into a cookie message, if they are present.
    % We do this by extracting the set-cookie headers and converting them into a
    % cookie message if they are present.
    SetCookieLines =
        [
            KeyVal
        ||
            {<<"set-cookie">>, KeyVal} <- Headers
        ],
    MaybeSetCookie =
        case SetCookieLines of
            [] -> #{};
            _ ->
                ?event(
                    debug_cookie,
                    {normalizing_setcookie_headers,
                        {set_cookie_lines, [ {string, Line} || Line <- SetCookieLines ]}
                    },
                    Opts
                ),
                {ok, MsgWithCookies} =
                    dev_codec_cookie:from(
                        #{ <<"set-cookie">> => SetCookieLines },
                        #{},
                        Opts
                    ),
                ?event(debug_cookie, {msg_with_cookies, MsgWithCookies}),
                MsgWithCookies
        end,
    % Merge the set-cookie message into the header map, which itself is
    % constructed from the header key-value pair list.
    HeaderMap = hb_maps:merge(hb_maps:from_list(Headers), MaybeSetCookie, Opts),
    NormHeaderMap = hb_ao:normalize_keys(HeaderMap, Opts),
    ?event(http_outbound,
        {normalized_response_headers, {norm_header_map, NormHeaderMap}},
        Opts
    ),
    BaseStatus =
        case Status of
            201 -> created;
            X when X < 400 -> ok;
            X when X < 500 -> error;
            _ -> failure
        end,
    ?event(http_short,
        {received,
            {status, Status},
            {duration, EndTime - StartTime},
            {method, Method},
            {peer, Peer},
            {path, {string, Path}},
            {body_size, byte_size(Body)}
        }),
    ReturnAOResult =
        hb_opts:get(http_only_result, true, Opts) andalso
        hb_maps:get(<<"ao-result">>, NormHeaderMap, false, Opts),
    case ReturnAOResult of
        Key when is_binary(Key) ->
            Msg = http_response_to_httpsig(Status, NormHeaderMap, Body, Opts),
            ?event(http_outbound, {result_is_single_key, {key, Key}, {msg, Msg}}, Opts),
            case {Key, hb_maps:get(Key, Msg, undefined, Opts)} of
                {<<"body">>, undefined} -> {BaseStatus, <<>>};
                {_, undefined} ->
                    {failure,
                        <<
                            "Result key '",
                            Key/binary,
                            "' not found in response from '",
                            Peer/binary,
                            "' for path '",
                            Path/binary,
                            "': ",
                            Body/binary
                        >>
                    };
                {_, Value} -> {BaseStatus, Value}
            end;
        false ->
            case hb_maps:get(<<"codec-device">>, NormHeaderMap, <<"httpsig@1.0">>, Opts) of
                <<"httpsig@1.0">> ->
                    ?event(http_outbound, {result_is_httpsig, {body, Body}}, Opts),
                    {
                        BaseStatus,
                        http_response_to_httpsig(Status, NormHeaderMap, Body, Opts)
                    };
                <<"ans104@1.0">> ->
                    ?event(http_outbound, {result_is_ans104, {body, Body}}, Opts),
                    Deserialized = ar_bundles:deserialize(Body),
                    % We don't need to add the status to the message, because
                    % it is already present in the encoded ANS-104 message.
                    {
                        BaseStatus,
                        hb_message:convert(
                            Deserialized,
                            <<"structured@1.0">>,
                            <<"ans104@1.0">>,
                            Opts
                        )
                    }
            end
    end.

%% @doc Convert a HTTP response to a httpsig message.
http_response_to_httpsig(Status, HeaderMap, Body, Opts) ->
    (hb_message:convert(
        hb_maps:merge(
            HeaderMap#{ <<"status">> => hb_util:bin(Status) },
            case Body of
                <<>> -> #{};
                _ -> #{ <<"body">> => Body }
            end,
			Opts
        ),
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        <<"httpsig@1.0">>,
        Opts
    ))#{ <<"status">> => hb_util:int(Status) }.

%% @doc Given a message, return the information needed to make the request.
message_to_request(M, Opts) ->
    % Get the route for the message
    Res = route_to_request(M, RouteRes = dev_router:route(M, Opts), Opts),
    ?event(debug_http, {route_res, {route_res, RouteRes}, {full_res, Res}, {msg, M}}),
    Res.

%% @doc Parse a `dev_router:route' response and return a tuple of request
%% parameters.
route_to_request(M, {ok, URI}, Opts) when is_binary(URI) ->
    route_to_request(M, {ok, #{ <<"uri">> => URI, <<"opts">> => #{} }}, Opts);
route_to_request(M, {ok, #{ <<"uri">> := XPath, <<"opts">> := ReqOpts}}, Opts) ->
    % The request is a direct HTTP URL, so we need to split the path into a
    % host and path.
    URI = uri_string:parse(XPath),
    ?event(http_outbound, {parsed_uri, {uri, {explicit, URI}}}),
    Method = hb_ao:get(<<"method">>, M, <<"GET">>, Opts),
    % We must remove the path and host from the message, because they are not
    % valid for outbound requests. The path is retrieved from the route, and
    % the host should already be known to the caller.
    MsgWithoutMeta = hb_maps:without([<<"path">>, <<"host">>], M, Opts),
    Port =
        case maps:get(port, URI, undefined) of
            undefined ->
                % If no port is specified, use 80 for HTTP and 443
                % for HTTPS.
                case XPath of
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
    ?event(http_outbound, {parsed_req, {node, Node}, {method, Method}, {path, Path}}),
    {ok, Method, Node, Path, MsgWithoutMeta, hb_util:deep_merge(Opts, ReqOpts, Opts)};
route_to_request(M, {ok, Routes}, Opts) ->
    ?event(http_outbound, {found_routes, {req, M}, {routes, Routes}}),
    % The result is a route, so we leave it to `request' to handle it.
    Path = hb_ao:get(<<"path">>, M, <<"/">>, Opts),
    Method = hb_ao:get(<<"method">>, M, <<"GET">>, Opts),
    % We must remove the path and host from the message, because they are not
    % valid for outbound requests. The path is retrieved from the route, and
    % the host should already be known to the caller.
    MsgWithoutMeta = hb_maps:without([<<"path">>, <<"host">>], M, Opts),
    {ok, Method, Routes, Path, MsgWithoutMeta, Opts};
route_to_request(M, {error, Reason}, _Opts) ->
    {error, {no_viable_route, {reason, Reason}, {message, M}}}.

%% @doc Turn a set of request arguments into a request message, formatted in the
%% preferred format. This function honors the `accept-bundle' option, if it is
%% already present in the message, and sets it to `true' if it is not.
prepare_request(Format, Method, Peer, Path, RawMessage, Opts) ->
    Message = hb_ao:normalize_keys(RawMessage, Opts),
    % Generate a `cookie' key for the message, if an unencoded cookie is
    % present.
    {MaybeCookie, WithoutCookie} =
        case dev_codec_cookie:extract(Message, #{}, Opts) of
            {ok, NoCookies} when map_size(NoCookies) == 0 ->
                {#{}, Message};
            {ok, _Cookies} ->
                {ok, #{ <<"cookie">> := CookieLines }} =
                    dev_codec_cookie:to(
                        Message,
                        #{ <<"format">> => <<"cookie">> },
                        Opts
                    ),
                {ok, CookieReset} = dev_codec_cookie:reset(Message, Opts),
                ?event(http, {cookie_lines, CookieLines}),
                {
                    #{ <<"cookie">> => CookieLines },
                    CookieReset
                }
        end,
    % Remove the private components from the message, if they are present.
    WithoutPriv = hb_private:reset(WithoutCookie),
    % Add the `accept-bundle: true' key to the message, if the caller has not
    % set an explicit preference.
    WithAcceptBundle =
        case hb_maps:get(<<"accept-bundle">>, Message, not_found, Opts) of
            not_found -> WithoutPriv#{ <<"accept-bundle">> => true };
            _ -> WithoutPriv
        end,
    % Determine the `ao-peer-port' from the message to send or the node message.
    % `port_external' can be set in the node message to override the port that
    % the peer node should receive. This allows users to proxy requests to their
    % HB node from another port.
    WithSelfPort =
        WithAcceptBundle#{
            <<"ao-peer-port">> =>
                hb_maps:get(
                    <<"ao-peer-port">>,
                    WithAcceptBundle,
                    hb_opts:get(
                        port_external,
                        hb_opts:get(port, undefined, Opts),
                        Opts
                    ),
                    Opts
                )
        },
    BinPeer = if is_binary(Peer) -> Peer; true -> list_to_binary(Peer) end,
    BinPath = hb_path:normalize(hb_path:to_binary(Path)),
    ReqBase = #{ peer => BinPeer, path => BinPath, method => Method },
    case Format of
        <<"httpsig@1.0">> ->
            FullEncoding =
                hb_message:convert(
                    WithSelfPort,
                    #{
                        <<"device">> => <<"httpsig@1.0">>,
                        <<"bundle">> => true
                    },
                    Opts
                ),
            Body = hb_maps:get(<<"body">>, FullEncoding, <<>>, Opts),
            Headers = hb_maps:without([<<"body">>], FullEncoding, Opts),
			?event(http, {request_headers, {explicit, {headers, Headers}}}),
			?event(http, {request_body, {explicit, {body, Body}}}),
            hb_maps:merge(
                ReqBase,
                #{ headers => maps:merge(MaybeCookie, Headers), body => Body },
                Opts
            );
        <<"ans104@1.0">> ->
            ?event(debug_accept, {request_message, {message, Message}}),
            {ok, FilteredMessage} =
                case hb_message:signers(Message, Opts) of
                    [] -> WithSelfPort;
                    _ ->
                        hb_message:with_only_committed(WithSelfPort, Opts)
                end,
            ReqBase#{
                headers =>
                    MaybeCookie#{
                        <<"codec-device">> => <<"ans104@1.0">>,
                        <<"content-type">> => <<"application/ans104">>,
                        <<"accept-bundle">> =>
                            hb_util:bin(
                                hb_ao:get(
                                    <<"accept-bundle">>,
                                    WithSelfPort,
                                    true,
                                    Opts
                                )
                            )
                    },
                body =>
                    ar_bundles:serialize(
                        hb_message:convert(
                            FilteredMessage,
                            #{
                                <<"device">> => <<"ans104@1.0">>,
                                <<"bundle">> => true
                            },
                            Opts
                        )
                    )
            };
        _ ->
            ReqBase#{
                headers =>
                    maps:merge(MaybeCookie, maps:without([<<"body">>], Message)),
                body => maps:get(<<"body">>, Message, <<>>)
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
    #{
        nodes := Nodes,
        responses := Responses,
        stop_after := StopAfter,
        accept_status := Statuses,
        parallel := Parallel
    } = multirequest_opts(Config, Message, Opts),
    ?event(http,
        {multirequest_opts_parsed,
            {config, Config},
            {message, Message}
        }),
    AllResults =
        if Parallel ->
            parallel_multirequest(
                Nodes, Responses, StopAfter, Method, Path, Message, Statuses, Opts);
        true ->
            serial_multirequest(
                Nodes, Responses, Method, Path, Message, Statuses, Opts)
        end,
    ?event(http, {multirequest_results, {results, AllResults}}),
    case AllResults of
        [] -> {error, no_viable_responses};
        Results -> if Responses == 1 -> hd(Results); true -> Results end
    end.

%% @doc Get the multirequest options from the config or message. The options in 
%% the message take precidence over the options in the config.
multirequest_opts(Config, Message, Opts) ->
    Opts#{
        nodes =>
            multirequest_opt(<<"nodes">>, Config, Message, #{}, Opts),
        responses =>
            multirequest_opt(<<"responses">>, Config, Message, 1, Opts),
        stop_after =>
            multirequest_opt(<<"stop-after">>, Config, Message, true, Opts),
        accept_status =>
            multirequest_opt(<<"accept-status">>, Config, Message, <<"All">>, Opts),
        parallel =>
            multirequest_opt(<<"parallel">>, Config, Message, false, Opts)
    }.

%% @doc Get a value for a multirequest option from the config or message.
multirequest_opt(Key, Config, Message, Default, Opts) ->
    hb_ao:get_first(
        [
            {Message, <<"multirequest-", Key/binary>>},
            {Config, Key}
        ],
        Default,
        Opts#{ hashpath => ignore }
    ).

%% @doc Serially request a message, collecting responses until the required
%% number of responses have been gathered. Ensure that the statuses are
%% allowed, according to the configuration.
serial_multirequest(_Nodes, 0, _Method, _Path, _Message, _Statuses, _Opts) -> [];
serial_multirequest([], _, _Method, _Path, _Message, _Statuses, _Opts) -> [];
serial_multirequest([Node|Nodes], Remaining, Method, Path, Message, Statuses, Opts) ->
    {ErlStatus, Res} = request(Method, Node, Path, Message, Opts),
    BaseStatus = hb_ao:get(<<"status">>, Res, Opts),
    case (ErlStatus == ok) andalso allowed_status(BaseStatus, Statuses) of
        true ->
            ?event(http, {admissible_status, {response, Res}}),
            [
                {ErlStatus, Res}
            |
                serial_multirequest(Nodes, Remaining - 1, Method, Path, Message, Statuses, Opts)
            ];
        false ->
            ?event(http, {inadmissible_status, {response, Res}}),
            serial_multirequest(Nodes, Remaining, Method, Path, Message, Statuses, Opts)
    end.

%% @doc Dispatch the same HTTP request to many nodes in parallel.
parallel_multirequest(Nodes, Responses, StopAfter, Method, Path, Message, Statuses, Opts) ->
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
    parallel_responses([], Procs, Ref, Responses, StopAfter, Statuses, Opts).

%% @doc Check if a status is allowed, according to the configuration.
allowed_status(_, <<"All">>) -> true;
allowed_status(_ResponseMsg = #{ <<"status">> := Status }, Statuses) ->
    allowed_status(Status, Statuses);
allowed_status(Status, Statuses) when is_integer(Statuses) ->
    allowed_status(Status, [Statuses]);
allowed_status(Status, Statuses) when is_binary(Status) ->
    allowed_status(binary_to_integer(Status), Statuses);
allowed_status(Status, Statuses) when is_binary(Statuses) ->
    % Convert the statuses to a list of integers.
    allowed_status(
        Status,
        lists:map(fun binary_to_integer/1, binary:split(Statuses, <<",">>))
    );
allowed_status(Status, Statuses) when is_list(Statuses) ->
    lists:member(Status, Statuses).

%% @doc Collect the necessary number of responses, and stop workers if
%% configured to do so.
parallel_responses(Res, Procs, Ref, 0, false, _Statuses, _Opts) ->
    lists:foreach(fun(P) -> P ! no_reply end, Procs),
    empty_inbox(Ref),
    {ok, Res};
parallel_responses(Res, Procs, Ref, 0, true, _Statuses, _Opts) ->
    lists:foreach(fun(P) -> exit(P, kill) end, Procs),
    empty_inbox(Ref),
    Res;
parallel_responses(Res, Procs, Ref, Awaiting, StopAfter, Statuses, Opts) ->
    receive
        {Ref, Pid, {Status, NewRes}} ->
            case allowed_status(Status, Statuses) of
                true ->
                    parallel_responses(
                        [NewRes | Res],
                        lists:delete(Pid, Procs),
                        Ref,
                        Awaiting - 1,
                        StopAfter,
                        Statuses,
                        Opts
                    );
                false ->
                    parallel_responses(
                        Res,
                        lists:delete(Pid, Procs),
                        Ref,
                        Awaiting,
                        StopAfter,
                        Statuses,
                        Opts
                    )
            end
    end.

%% @doc Empty the inbox of the current process for all messages with the given
%% reference.
empty_inbox(Ref) ->
    receive {Ref, _} -> empty_inbox(Ref) after 0 -> ok end.

%% @doc Reply to the client's HTTP request with a message.
reply(Req, TABMReq, Message, Opts) ->
    Status =
        case hb_ao:get(<<"status">>, Message, Opts) of
            not_found -> 200;
            S-> S
        end,
    reply(Req, TABMReq, Status, Message, Opts).
reply(Req, TABMReq, BinStatus, RawMessage, Opts) when is_binary(BinStatus) ->
    reply(Req, TABMReq, binary_to_integer(BinStatus), RawMessage, Opts);
reply(InitReq, TABMReq, Status, RawMessage, Opts) ->
    KeyNormMessage = hb_ao:normalize_keys(RawMessage, Opts),
    {ok, Req, Message} = reply_handle_cookies(InitReq, KeyNormMessage, Opts),
    {ok, HeadersBeforeCors, EncodedBody} =
        encode_reply(
            Status,
            TABMReq,
            Message,
            Opts
        ),
    % Get the CORS request headers from the message, if they exist.
    ReqHdr = cowboy_req:header(<<"access-control-request-headers">>, Req, <<"">>),
    HeadersWithCors = add_cors_headers(HeadersBeforeCors, ReqHdr, Opts),
    EncodedHeaders = hb_private:reset(HeadersWithCors),
    ?event(http,
        {http_replying,
            {status, {explicit, Status}},
            {path, hb_maps:get(<<"path">>, Req, undefined_path, Opts)},
            {raw_message, RawMessage},
            {enc_headers, {explicit, EncodedHeaders}},
            {enc_body, EncodedBody}
        }
    ),
    ReqBeforeStream = Req#{ resp_headers => EncodedHeaders },
    PostStreamReq = cowboy_req:stream_reply(Status, #{}, ReqBeforeStream),
    cowboy_req:stream_body(EncodedBody, nofin, PostStreamReq),
    EndTime = os:system_time(millisecond),
    ?event(http, {reply_headers, {explicit, PostStreamReq}}),
    ?event(http_short,
        {sent,
            {status, Status},
            {duration, EndTime - hb_maps:get(start_time, Req, undefined, Opts)},
            {method, cowboy_req:method(Req)},
            {path,
                {string,
                    uri_string:percent_decode(
                        hb_ao:get(<<"path">>, TABMReq, <<"[NO PATH]">>, Opts)
                    )
                }
            },
            {body_size, byte_size(EncodedBody)}
        }
    ),
    {ok, PostStreamReq, no_state}.

%% @doc Handle replying with cookies if the message contains them. Returns the
%% new Cowboy `Req` object, and the message with the cookies removed. Both
%% `set-cookie' and `cookie' fields are treated as viable sources of cookies.
reply_handle_cookies(Req, Message, Opts) ->
    {ok, Cookies} = dev_codec_cookie:extract(Message, #{}, Opts),
    ?event(debug_cookie, {encoding_reply_cookies, {explicit, Cookies}}),
    case Cookies of
        NoCookies when map_size(NoCookies) == 0 -> {ok, Req, Message};
        _ ->
            % The internal values of the `cookie' field will be stored in the
            % `priv_store' by default, so we let `dev_codec_cookie:opts/1'
            % reset the options.
            {ok, #{ <<"set-cookie">> := SetCookieLines }} =
                dev_codec_cookie:to(
                    Message,
                    #{ <<"format">> => <<"set-cookie">> },
                    Opts
                ),
            ?event(debug_cookie, {outbound_set_cookie_lines, SetCookieLines}),
            % Add the cookies to the response headers.
            FinalReq =
                lists:foldl(
                    fun(FullCookieLine, ReqAcc) ->
                        [CookieRef, _] = binary:split(FullCookieLine, <<"=">>),
                        RespCookies = maps:get(resp_cookies, ReqAcc, #{}),
                        % Note: Cowboy handles cookies peculiarly. The key
                        % given in the `resp_cookies' map is not used directly
                        % in the response headers. Nonetheless, we use the
                        % key parsed from the cookie line as the key, but do not
                        % be surprised if while debugging you see a different
                        % key created by Cowboy in the response headers.
                        ReqAcc#{
                            resp_cookies =>
                                RespCookies#{ CookieRef => FullCookieLine }
                        }
                    end,
                    Req,
                    SetCookieLines
                ),
            {ok, CookieReset} = dev_codec_cookie:reset(Message, Opts),
            {
                ok,
                FinalReq,
                CookieReset
            }
    end.

%% @doc Add permissive CORS headers to a message, if the message has not already
%% specified CORS headers.
add_cors_headers(Msg, ReqHdr, Opts) ->
    CorHeaders = #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-expose-headers">> => <<"*">>
    },
     WithAllowHeaders = case ReqHdr of
        <<>> -> CorHeaders;
        _ -> CorHeaders#{
             <<"access-control-allow-headers">> => ReqHdr
        }
    end,
    % Keys in the given message will overwrite the defaults listed below if 
    % included, due to `hb_maps:merge''s precidence order.
    hb_maps:merge(WithAllowHeaders, Msg, Opts).

%% @doc Generate the headers and body for a HTTP response message.
encode_reply(Status, TABMReq, Message, Opts) ->
    Codec = accept_to_codec(TABMReq, Opts),
    ?event(http, {encoding_reply, {codec, Codec}, {message, Message}}),
    BaseHdrs =
        hb_maps:merge(
            #{
                <<"codec-device">> => Codec
            },
            case codec_to_content_type(Codec, Opts) of
                    undefined -> #{};
                    CT -> #{ <<"content-type">> => CT }
            end,
			Opts
        ),
    AcceptBundle =
        hb_util:atom(
            hb_maps:get(<<"accept-bundle">>, TABMReq, false, Opts)
        ),
    % Codecs generally do not need to specify headers outside of the content-type,
    % aside the default `httpsig@1.0' codec, which expresses its form in HTTP
    % documents, and subsequently must set its own headers.
    case {Status, Codec, AcceptBundle} of
        {500, <<"httpsig@1.0">>, false} ->
            ?event(debug_accept,
                {returning_500_error,
                    {status, Status},
                    {codec, Codec},
                    {bundle, AcceptBundle}
                }
            ),
            {ok, ErrMsg} =
                dev_hyperbuddy:return_error(Message, Opts),
            {ok,
                maps:without([<<"body">>], ErrMsg),
                maps:get(<<"body">>, ErrMsg, <<>>)
            };
        {404, <<"httpsig@1.0">>, false} ->
            {ok, ErrMsg} =
                dev_hyperbuddy:return_file(
                    <<"hyperbuddy@1.0">>,
                    <<"404.html">>
                ),
            {ok,
                maps:without([<<"body">>], ErrMsg),
                maps:get(<<"body">>, ErrMsg, <<>>)
            };
        {_, <<"httpsig@1.0">>, _} ->
            TABM =
                hb_message:convert(
                    Message,
                    tabm,
                    <<"structured@1.0">>,
                    Opts#{ topic => ao_internal }
                ),
            {ok, EncMessage} =
                dev_codec_httpsig:to(
                    TABM,
                    case AcceptBundle of
                        true ->
                            #{
                                <<"bundle">> => true
                            };
                        false ->
                            #{
                                <<"index">> =>
                                    hb_opts:get(generate_index, true, Opts)
                            }
                    end,
                    Opts
                ),
            {
                ok,
                hb_maps:without([<<"body">>], EncMessage, Opts),
                hb_maps:get(<<"body">>, EncMessage, <<>>, Opts)
            };
        {_, <<"ans104@1.0">>, _} ->
            % The `ans104@1.0' codec is a binary format, so we must serialize
            % the message to a binary before sending it.
            {
                ok,
                BaseHdrs,
                ar_bundles:serialize(
                    hb_message:convert(
                        hb_message:with_only_committers(
                            Message,
                            hb_message:signers(Message, Opts),
							Opts
                        ),
                        #{
                            <<"device">> => <<"ans104@1.0">>,
                            <<"bundle">> =>
                                hb_util:atom(
                                    hb_ao:get(
                                        <<"accept-bundle">>,
                                        {as, <<"message@1.0">>, TABMReq},
                                        true,
                                        Opts
                                    )
                                )
                        },
                        <<"structured@1.0">>,
                        Opts#{ topic => ao_internal }
                    )
                )
            };
        _ ->
            % Other codecs are already in binary format, so we can just convert
            % the message to the codec. We also include all of the top-level 
            % fields, except for maps and lists, in the message and return them 
            % as headers.
            ExtraHdrs = hb_maps:filter(fun(_, V) ->
                not is_map(V) andalso 
                not is_list(V)
            end, Message, Opts),
            % Encode all header values as strings.
            EncodedExtraHdrs = maps:map(fun(K, V) ->
                case is_binary(V) of
                    true -> V;
                    false -> hb_util:bin(V)
                end
            end, ExtraHdrs),
            {ok,
                hb_maps:merge(EncodedExtraHdrs, BaseHdrs, Opts),
                hb_message:convert(
                    Message,
                    Codec,
                    tabm,
                    Opts#{ topic => ao_internal }
                )
            }
    end.

%% @doc Calculate the codec name to use for a reply given its initiating Cowboy
%% request, the parsed TABM request, and the response message. The precidence
%% order for finding the codec is:
%% 1. The `accept-codec' field in the message
%% 2. The `accept' field in the request headers
%% 3. The default codec
%% Options can be specified in mime-type format (`application/*') or in
%% AO device format (`device@1.0').
accept_to_codec(TABMReq, Opts) ->
    Singleton = lists:last(hb_singleton:from(TABMReq, Opts)),
    Accept = hb_ao:get(<<"accept">>, Singleton, <<"*/*">>, Opts),
    ?event(only, {accept_to_codec, {tabm_req, TABMReq}}),
    AcceptCodec =
        hb_ao:get(
            <<"accept-codec">>,
            Singleton,
            mime_to_codec(Accept, Opts),
			Opts
        ),
    case AcceptCodec of
        not_specified ->
            % We hold off until confirming that the codec is not directly in the
            % message before calling `hb_opts:get/3', as it is comparatively
            % expensive.
            default_codec(Opts);
        _ -> AcceptCodec
    end.

%% @doc Find a codec name from a mime-type.
mime_to_codec(<<"application/", Mime/binary>>, Opts) ->
    Name =
        case binary:match(Mime, <<"@">>) of
            nomatch -> << Mime/binary, "@1.0" >>;
            _ -> Mime
        end,
    try 
        DeviceId = hb_ao:message_to_device(#{ <<"device">> => Name }, Opts),
        hb_ao:get_device_name(DeviceId, Opts)
    catch _:Error ->
        ?event(http, {accept_to_codec_error, {name, Name}, {error, Error}}),
        default_codec(Opts)
    end;

mime_to_codec(<<"device/", Name/binary>>, _Opts) -> Name;
mime_to_codec(_, _Opts) -> not_specified.

%% @doc Return the default codec for the given options.
default_codec(Opts) ->
    hb_opts:get(default_codec, <<"httpsig@1.0">>, Opts).

%% @doc Call the `content-type' key on a message with the given codec, using
%% a fast-path for options that are not needed for this one-time lookup.
codec_to_content_type(Codec, Opts) ->
    FastOpts =
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-cache">>, <<"no-store">>],
            cache_lookup_hueristics => false,
            load_remote_devices => false,
            error_strategy => continue
        },
    case hb_ao:get(<<"content-type">>, #{ <<"device">> => Codec }, FastOpts) of
        not_found -> undefined;
        CT -> CT
    end.

%% @doc Convert a cowboy request to a normalized message.
req_to_tabm_singleton(Req, Body, Opts) ->
    case cowboy_req:header(<<"codec-device">>, Req, <<"httpsig@1.0">>) of
        <<"httpsig@1.0">> ->
			?event({req_to_tabm_singleton, {request, {explicit, Req}, {body, {string, Body}}}}),
            httpsig_to_tabm_singleton(Req, Body, Opts);
        <<"ans104@1.0">> ->
            Item = ar_bundles:deserialize(Body),
            ?event(debug_accept,
                {deserialized_ans104,
                    {item, Item},
                    {exact, {explicit, Item}}
                }
            ),
            case ar_bundles:verify_item(Item) of
                true ->
                    ?event(ans104, {valid_ans104_signature, Item}),
                    ANS104 =
                        hb_message:convert(
                            Item,
                            <<"structured@1.0">>,
                            <<"ans104@1.0">>,
                            Opts
                        ),
                    normalize_unsigned(Req, ANS104, Opts);
                false ->
                    throw({invalid_ans104_signature, Item})
            end;
        Codec ->
            % Assume that the codec stores the encoded message in the `body' field.
            Decoded =
                hb_message:convert(
                    Body,
                    <<"structured@1.0">>,
                    Codec,
                    Opts
                ),
            ?event(
                {verifying_encoded_message,
                    {body, {string, Body}},
                    {decoded, Decoded}
                }
            ),
            case hb_message:verify(Decoded, all) of
                true ->
                    normalize_unsigned(Req, Decoded, Opts);
                false ->
                    throw({invalid_signature, Decoded})
            end
    end.

%% @doc HTTPSig messages are inherently mixed into the transport layer, so they
%% require special handling in order to be converted to a normalized message.
%% In particular, the signatures are verified if present and required by the 
%% node configuration. Additionally, non-committed fields are removed from the
%% message if it is signed, with the exception of the `path' and `method' fields.
httpsig_to_tabm_singleton(Req = #{ headers := RawHeaders }, Body, Opts) ->
    {ok, SignedMsg} =
        hb_message:with_only_committed(
            hb_message:convert(
                RawHeaders#{ <<"body">> => Body },
                <<"structured@1.0">>,
                <<"httpsig@1.0">>,
                Opts
            ),
            Opts
        ),
    ForceSignedRequests = hb_opts:get(force_signed_requests, false, Opts),
    case (not ForceSignedRequests) orelse hb_message:verify(SignedMsg, all, Opts) of
        true ->
            ?event(http_verify, {verified_signature, SignedMsg}),
            Signers = hb_message:signers(SignedMsg, Opts),
            case Signers =/= [] andalso hb_opts:get(store_all_signed, false, Opts) of
                true ->
                    ?event(http_verify, {storing_signed_from_wire, SignedMsg}),
                    {ok, _} =
                        hb_cache:write(SignedMsg,
                            Opts#{
                                store =>
                                    #{
                                        <<"store-module">> => hb_store_fs,
                                        <<"name">> => <<"cache-http">>
                                    }
                            }
                        );
                false ->
                    do_nothing
            end,
            normalize_unsigned(Req, SignedMsg, Opts);
        false ->
            ?event(http_verify,
                {invalid_signature,
                    {raw, RawHeaders},
                    {signed, SignedMsg},
                    {force, ForceSignedRequests}
                }
            ),
            throw({invalid_signature, SignedMsg})
    end.

%% @doc Add the method and path to a message, if they are not already present.
%% Remove browser-added fields that are unhelpful during processing (for example,
%% `content-length').
%% The precidence order for finding the path is:
%% 1. The path in the message
%% 2. The path in the request URI
normalize_unsigned(Req = #{ headers := RawHeaders }, Msg, Opts) ->
    ?event({adding_method_and_path_from_request, {explicit, Req}}),
    Method = cowboy_req:method(Req),
    MsgPath =
        hb_ao:get(
            <<"path">>,
            Msg,
            maps:get(
                <<"path">>, 
                RawHeaders,
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
            ),
            Opts
        ),
    WithoutPeer =
        (remove_unless_signed([<<"content-length">>], Msg, Opts))#{
            <<"method">> => Method,
            <<"path">> => MsgPath,
            <<"accept-bundle">> =>
                maps:get(
                    <<"accept-bundle">>,
                    Msg,
                    maps:get(<<"accept-bundle">>, RawHeaders, false)
                )
        },
    % Parse and add the cookie from the request, if present. We reinstate the
    % `cookie' field in the message, as it is not typically signed, yet should
    % be honored by the node anyway.
    {ok, WithCookie} =
        case maps:get(<<"cookie">>, RawHeaders, undefined) of
            undefined -> {ok, WithoutPeer};
            Cookie ->
                dev_codec_cookie:from(
                    WithoutPeer#{ <<"cookie">> => Cookie },
                    Req,
                    Opts
                )
        end,
    % If the body is empty and unsigned, we remove it.
    NormalBody =
        case hb_maps:get(<<"body">>, WithCookie, undefined, Opts) of
            <<"">> -> remove_unless_signed(<<"body">>, WithCookie, Opts);
            _ -> WithCookie
        end,
    case hb_maps:get(<<"ao-peer-port">>, NormalBody, undefined, Opts) of
        undefined -> NormalBody;
        P2PPort ->
            % Calculate the peer address from the request. We honor the 
            % `x-real-ip' header if it is present.
            RealIP =
                case hb_maps:get(<<"x-real-ip">>, RawHeaders, undefined, Opts) of
                    undefined ->
                        {{A, B, C, D}, _} = cowboy_req:peer(Req),
                        hb_util:bin(
                            io_lib:format(
                                "~b.~b.~b.~b",
                                [A, B, C, D]
                            )
                        );
                    IP -> IP
                end,
            Peer = <<RealIP/binary, ":", (hb_util:bin(P2PPort))/binary>>,
            (remove_unless_signed(<<"ao-peer-port">>, NormalBody, Opts))#{
                <<"ao-peer">> => Peer
            }
    end.

%% @doc Remove all keys from the message unless they are signed.
remove_unless_signed(Key, Msg, Opts) when not is_list(Key) ->
    remove_unless_signed([Key], Msg, Opts);
remove_unless_signed(Keys, Msg, Opts) ->
    SignedKeys = hb_message:committed(Msg, all, Opts),
    maps:without(
        lists:filter(fun(K) -> not lists:member(K, SignedKeys) end, Keys),
        Msg
    ).

%%% Tests

simple_ao_resolve_unsigned_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"/key1">>, <<"key1">> => <<"Value1">> },
    ?assertEqual({ok, <<"Value1">>}, post(URL, TestMsg, #{})).

simple_ao_resolve_signed_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"/key1">>, <<"key1">> => <<"Value1">> },
    Wallet = hb:wallet(),
    {ok, Res} =
        post(
            URL,
            hb_message:commit(TestMsg, Wallet),
            #{}
        ),
    ?assertEqual(<<"Value1">>, Res).

nested_ao_resolve_test() ->
    URL = hb_http_server:start_node(),
    Wallet = hb:wallet(),
    {ok, Res} =
        post(
            URL,
            hb_message:commit(#{
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
    ?assertEqual(<<"Value2">>, Res).

wasm_compute_request(ImageFile, Func, Params) ->
    wasm_compute_request(ImageFile, Func, Params, <<"">>).
wasm_compute_request(ImageFile, Func, Params, ResultPath) ->
    {ok, Bin} = file:read_file(ImageFile),
    Wallet = hb:wallet(),
    hb_message:commit(#{
        <<"path">> => <<"/init/compute/results", ResultPath/binary>>,
        <<"device">> => <<"wasm-64@1.0">>,
        <<"function">> => Func,
        <<"parameters">> => Params,
        <<"body">> => Bin
    }, Wallet).

run_wasm_unsigned_test() ->
    Node = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0]),
    {ok, Res} = post(Node, Msg, #{}),
    ?event({res, Res}),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, #{})).

run_wasm_signed_test() ->
    Opts = #{ priv_wallet => hb:wallet() },
    URL = hb_http_server:start_node(#{force_signed => true}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, hb_message:commit(Msg, Opts), Opts),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, #{})).

get_deep_unsigned_wasm_state_test() ->
    URL = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_ao:get(<<"/output/1">>, Res, #{})).

get_deep_signed_wasm_state_test() ->
    URL = hb_http_server:start_node(#{force_signed => true}),
    Msg =
        wasm_compute_request(
            <<"test/test-64.wasm">>,
            <<"fac">>,
            [3.0],
            <<"/output">>
        ),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_ao:get(<<"1">>, Res, #{})).

cors_get_test() ->
    URL = hb_http_server:start_node(),
    {ok, Res} = get(URL, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(
        <<"*">>,
        hb_ao:get(<<"access-control-allow-origin">>, Res, #{})
    ).

ans104_wasm_test() ->
    URL = hb_http_server:start_node(#{force_signed => true}),
    {ok, Bin} = file:read_file(<<"test/test-64.wasm">>),
    Wallet = hb:wallet(),
    Msg = hb_message:commit(#{
        <<"accept-codec">> => <<"ans104@1.0">>,
        <<"codec-device">> => <<"ans104@1.0">>,
        <<"device">> => <<"wasm-64@1.0">>,
        <<"function">> => <<"fac">>,
        <<"parameters">> => [3.0],
        <<"body">> => Bin
    }, Wallet, #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }),
    ?assert(hb_message:verify(Msg, all, #{})),
    ?event({msg, {explicit, Msg}}),
    {ok, Res} = post(URL, Msg#{ <<"path">> => <<"/init/compute/results">> }, #{}),
    ?event({res, Res}),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, #{})),
    ok.

send_large_signed_request_test() ->
    % Note: If the signature scheme ever changes, we will need to run the 
    % following to get a freshly signed request.
    %    file:write_file(
    %        "test/large-message.eterm",
    %        hb_util:bin(
    %            io_lib:format(
    %               "~p.", 
    %                [
    %                    hb_cache:ensure_all_loaded(hb_message:commit(
    %                        hb_message:uncommitted(hd(hb_util:ok(
    %                            file:consult(<<"test/large-message.eterm">>)
    %                       ))),
    %                        #{ priv_wallet => hb:wallet() }
    %                    ))
    %                ]
    %            )
    %        )
    %    ).
    {ok, [Req]} = file:consult(<<"test/large-message.eterm">>),
    % Get the short trace length from the node message in the large, stored
    % request. 
    ?assertMatch(
        {ok, 5},
        post(
            hb_http_server:start_node(),
            <<"/node-message/short_trace_len">>,
            Req,
            #{ http_client => httpc }
        )
    ).

index_test() ->
    NodeURL = hb_http_server:start_node(),
    {ok, Res} =
        get(
            NodeURL,
            #{
                <<"path">> => <<"/~test-device@1.0/load">>,
                <<"accept-bundle">> => false
            },
            #{}
        ),
    ?assertEqual(<<"i like turtles!">>, hb_ao:get(<<"body">>, Res, #{})).

index_request_test() ->
    URL = hb_http_server:start_node(),
    {ok, Res} =
        get(
            URL,
            #{
                <<"path">> => <<"/~test-device@1.0/load?name=dogs">>,
                <<"accept-bundle">> => false
            },
            #{}
        ),
    ?assertEqual(<<"i like dogs!">>, hb_ao:get(<<"body">>, Res, #{})).