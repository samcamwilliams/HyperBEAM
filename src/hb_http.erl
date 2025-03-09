%%% Hyperbeam's core HTTP request/reply functionality. The functions in this
%%% module generally take a message request from their caller and return a
%%% response in message form, as granted by the peer. This module is mostly
%%% used by hb_client, but can also be used by other modules that need to make
%%% HTTP requests.
-module(hb_http).
-export([start/0]).
-export([get/2, get/3, post/3, post/4, request/2, request/4, request/5]).
-export([reply/4, accept_to_codec/2]).
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
request(Method, Config = #{ <<"nodes">> := Nodes }, Path, Message, Opts) when is_list(Nodes) ->
    % The request has a `route` (see `dev_router` for more details), so we use the
    % `multirequest` functionality, rather than a single request.
    multirequest(Config, Method, Path, Message, Opts);
request(Method, #{ <<"opts">> := NodeOpts, <<"uri">> := URI }, _Path, Message, Opts) ->
    % The request has a set of additional options, so we apply them to the
    % request.
    MergedOpts = maps:merge(Opts, NodeOpts),
    % We also recalculate the request. The order of precidence here is subtle:
    % We favor the args given to the function, but the URI rules take precidence
    % over that.
    {ok, NewMethod, Node, NewPath, NewMsg} =
        message_to_request(
            Message#{ <<"path">> => URI, <<"method">> => Method },
            MergedOpts
        ),
    request(NewMethod, Node, NewPath, NewMsg, MergedOpts);
request(Method, Peer, Path, RawMessage, Opts) ->
    ?event(http, {request, {method, Method}, {peer, Peer}, {path, Path}, {message, RawMessage}}),
    Req =
        prepare_request(
            hb_converge:get(
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
    ?event(http, {req, Req}),
    case hb_http_client:req(Req, Opts) of
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
                case maps:get(<<"codec-device">>, NormHeaderMap, <<"httpsig@1.0">>) of
                    <<"ans104@1.0">> ->
                        Deserialized = ar_bundles:deserialize(Body),
                        % We don't need to add the status to the message, because
                        % it is already present in the encoded ANS-104 message.
                        hb_message:convert(
                            Deserialized,
                            <<"structured@1.0">>,
                            <<"ans104@1.0">>,
                            Opts
                        );
                    <<"httpsig@1.0">> ->
                        hb_message:convert(
                            maps:merge(
                                HeaderMap#{ <<"status">> => hb_util:bin(Status) },
                                case Body of
                                    <<>> -> #{};
                                    _ -> #{ <<"body">> => Body }
                                end
                            ),
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
                    {response, #{status => Status, body => {explicit, Body}}}
                }),
            {error, Body};
        {ok, Status, _Headers, Body} when Status > 400 ->
            ?event(
                {http_got_server_error,
                    {req, Req},
                    {response, #{status => Status, body => Body}}
                }
            ),
            {error, Body};
        Response ->
            ?event(
                {http_error,
                    {req, Req},
                    {response, {explicit, Response}}
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
    case dev_router:route(M, Opts) of
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
            ?event(http, {parsed_req, {node, Node}, {method, Method}, {path, Path}}),
            {ok, Method, Node, Path, MsgWithoutMeta};
        {ok, Routes} ->
            ?event(http, {found_routes, {req, M}, {routes, Routes}}),
            % The result is a route, so we leave it to `request` to handle it.
            Path = hb_converge:get(<<"path">>, M, <<"/">>, Opts),
            {ok, Method, Routes, Path, MsgWithoutMeta};
        {error, Reason} ->
            {error, {no_viable_route, Reason, {message, M}}}
    end.

%% @doc Turn a set of request arguments into a request message, formatted in the
%% preferred format.
prepare_request(Format, Method, Peer, Path, RawMessage, Opts) ->
    Message = hb_converge:normalize_keys(RawMessage),
    BinPeer = if is_binary(Peer) -> Peer; true -> list_to_binary(Peer) end,
    BinPath = hb_path:normalize(hb_path:to_binary(Path)),
    ReqBase = #{ peer => BinPeer, path => BinPath, method => Method },
    case Format of
        <<"httpsig@1.0">> ->
            FullEncoding =
                hb_message:convert(Message, <<"httpsig@1.0">>, Opts),
            Body = maps:get(<<"body">>, FullEncoding, <<>>),
            Headers = maps:without([<<"body">>], FullEncoding),
            maps:merge(ReqBase, #{ headers => Headers, body => Body });
        <<"ans104@1.0">> ->
            ReqBase#{
                headers =>
                    #{
                        <<"codec-device">> => <<"ans104@1.0">>,
                        <<"content-type">> => <<"application/ans104">>
                    },
                body =>
                    ar_bundles:serialize(
                        hb_message:convert(Message, <<"ans104@1.0">>, Opts)
                    )
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
    MultiOpts = #{
        nodes := Nodes,
        responses := Responses,
        stop_after := StopAfter,
        accept_status := Statuses,
        parallel := Parallel
    } = multirequest_opts(Config, Message, Opts),
    ?event(http,
        {multirequest_opts_parsed,
            {config, Config},
            {message, Message},
            {multirequest_opts, MultiOpts}
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
    hb_converge:get_first(
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
    BaseStatus = hb_converge:get(<<"status">>, Res, Opts),
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
        case hb_converge:get(<<"status">>, Message, Opts) of
            not_found -> 200;
            S-> S
        end,
    reply(Req, TABMReq, Status, Message, Opts).
reply(Req, TABMReq, BinStatus, RawMessage, Opts) when is_binary(BinStatus) ->
    reply(Req, TABMReq, binary_to_integer(BinStatus), RawMessage, Opts);
reply(Req, TABMReq, Status, RawMessage, Opts) ->
    Message = hb_converge:normalize_keys(RawMessage),
    {ok, HeadersBeforeCors, EncodedBody} = encode_reply(TABMReq, Message, Opts),
    % Get the CORS request headers from the message, if they exist.
    ReqHdr = cowboy_req:header(<<"access-control-request-headers">>, Req, <<"">>),
    HeadersWithCors = add_cors_headers(HeadersBeforeCors, ReqHdr),
    EncodedHeaders = hb_private:reset(HeadersWithCors),
    ?event(http,
        {replying,
            {status, {explicit, Status}},
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

%% @doc Add permissive CORS headers to a message, if the message has not already
%% specified CORS headers.
add_cors_headers(Msg, ReqHdr) ->
    CorHeaders = #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>
    },
     WithAllowHeaders = case ReqHdr of
        <<>> -> CorHeaders;
        _ -> CorHeaders#{
             <<"access-control-allow-headers">> => ReqHdr
        }
    end,
    % Keys in the given message will overwrite the defaults listed below if 
    % included, due to `maps:merge`'s precidence order.
    maps:merge(WithAllowHeaders, Msg).

%% @doc Generate the headers and body for a HTTP response message.
encode_reply(TABMReq, Message, Opts) ->
    Codec = accept_to_codec(TABMReq, Opts),
    ?event(http, {encoding_reply, {codec, Codec}, {message, Message}}),
    BaseHdrs =
        maps:merge(
            #{
                <<"codec-device">> => Codec
            },
            case codec_to_content_type(Codec, Opts) of
                    undefined -> #{};
                    CT -> #{ <<"content-type">> => CT }
            end
        ),
    % Codecs generally do not need to specify headers outside of the content-type,
    % aside the default `httpsig@1.0` codec, which expresses its form in HTTP
    % documents, and subsequently must set its own headers.
    case Codec of
        <<"httpsig@1.0">> ->
            EncMessage =
                hb_message:convert(
                    Message,
                    <<"httpsig@1.0">>,
                    <<"structured@1.0">>,
                    #{ topic => converge_internal }
                ),
            {
                ok,
                maps:without([<<"body">>], EncMessage),
                maps:get(<<"body">>, EncMessage, <<>>)
            };
        <<"ans104@1.0">> ->
            % The `ans104@1.0` codec is a binary format, so we must serialize
            % the message to a binary before sending it.
            {
                ok,
                BaseHdrs,
                ar_bundles:serialize(
                    hb_message:convert(
                        hb_message:with_only_attestors(
                            Message,
                            hb_message:signers(Message)
                        ),
                        <<"ans104@1.0">>,
                        <<"structured@1.0">>,
                        Opts#{ topic => converge_internal }
                    )
                )
            };
        _ ->
            % Other codecs are already in binary format, so we can just convert
            % the message to the codec. We also include all of the top-level 
            % fields in the message and return them as headers.
            ExtraHdrs = maps:filter(fun(_, V) -> not is_map(V) end, Message),
            ?event(debug, {extra_headers, {headers, {explicit, ExtraHdrs}}, {message, Message}}),
            {ok,
                maps:merge(BaseHdrs, ExtraHdrs),
                hb_message:convert(
                    Message,
                    Codec,
                    <<"structured@1.0">>,
                    Opts#{ topic => converge_internal }
                )
            }
    end.

%% @doc Calculate the codec name to use for a reply given its initiating Cowboy
%% request, the parsed TABM request, and the response message. The precidence
%% order for finding the codec is:
%% 1. The `accept-codec` field in the message
%% 2. The `accept` field in the request headers
%% 3. The default codec
%% Options can be specified in mime-type format (`application/*`) or in
%% AO device format (`device@1.0').
accept_to_codec(TABMReq, Opts) ->
    AcceptCodec =
        maps:get(
            <<"accept-codec">>,
            TABMReq,
            mime_to_codec(maps:get(<<"accept">>, TABMReq, <<"*/*">>), Opts)
        ),
    ?event(http, {accept_to_codec, AcceptCodec}),
    case AcceptCodec of
        not_specified ->
            % We hold off until confirming that the codec is not directly in the
            % message before calling `hb_opts:get/3`, as it is comparatively
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
    try hb_converge:message_to_device(#{ <<"device">> => Name }, Opts)
    catch _:Error ->
        ?event(http, {accept_to_codec_error, {name, Name}, {error, Error}}),
        default_codec(Opts)
    end;
mime_to_codec(<<"device/", Name/binary>>, _Opts) -> Name;
mime_to_codec(_, _Opts) -> not_specified.

%% @doc Return the default codec for the given options.
default_codec(Opts) ->
    hb_opts:get(default_codec, <<"httpsig@1.0">>, Opts).

%% @doc Call the `content-type` key on a message with the given codec, using
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
    case hb_converge:get(<<"content-type">>, #{ <<"device">> => Codec }, FastOpts) of
        not_found -> undefined;
        CT -> CT
    end.

%% @doc Convert a cowboy request to a normalized message.
req_to_tabm_singleton(Req, Body, Opts) ->
    case cowboy_req:header(<<"codec-device">>, Req, <<"httpsig@1.0">>) of
        <<"httpsig@1.0">> ->
            http_sig_to_tabm_singleton(Req, Body, Opts);
        Codec ->
            hb_message:convert(
                ar_bundles:deserialize(Body),
                <<"structured@1.0">>,
                Codec,
                Opts
            )
    end.

%% @doc HTTPSig messages are inherently mixed into the transport layer, so they
%% require special handling in order to be converted to a normalized message.
%% In particular, the signatures are verified if present and required by the 
%% node configuration. Additionally, non-attested fields are removed from the
%% message if it is signed, with the exception of the `path` and `method` fields.
http_sig_to_tabm_singleton(Req = #{ headers := RawHeaders }, Body, Opts) ->
    Msg = dev_codec_httpsig_conv:from(
        RawHeaders#{ <<"body">> => Body }
    ),
    {ok, SignedMsg} =
        dev_codec_httpsig:reset_hmac(
            hb_util:ok(remove_unsigned_fields(Msg, Opts))
        ),
    ForceSignedRequests = hb_opts:get(force_signed_requests, true, Opts),
    case (not ForceSignedRequests) orelse hb_message:verify(SignedMsg) of
        true ->
            ?event(http_verify, {verified_signature, SignedMsg}),
            case hb_opts:get(store_all_signed, false, Opts) of
                true ->
                    ?event(http_verify, {storing_signed_from_wire, SignedMsg}),
                    hb_cache:write(Msg,
                        Opts#{
                            store =>
                                {hb_store_fs, #{ prefix => "store-inputs" }}
                        }
                    );
                false ->
                    do_nothing
            end,
            maybe_add_unsigned(Req, SignedMsg, Opts);
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
%% The precidence order for finding the path is:
%% 1. The path in the message
%% 2. The path in the request URI
maybe_add_unsigned(Req = #{ headers := RawHeaders }, Msg, Opts) ->
    Method = cowboy_req:method(Req),
    MsgPath =
        hb_converge:get(
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
    Msg#{ <<"method">> => Method, <<"path">> => MsgPath }.

remove_unsigned_fields(Msg, _Opts) ->
    case hb_message:signers(Msg) of
        [] -> {ok, Msg};
        _ -> hb_message:with_only_attested(Msg)
    end.

%%% Tests

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

cors_get_test() ->
    URL = hb_http_server:start_node(),
    {ok, Res} = get(URL, <<"/~meta@1.0/info/address">>, #{}),
    ?assertEqual(
        <<"*">>,
        hb_converge:get(<<"access-control-allow-origin">>, Res, #{})
    ).

ans104_wasm_test() ->
    URL = hb_http_server:start_node(#{force_signed => true}),
    {ok, Bin} = file:read_file(<<"test/test-64.wasm">>),
    Wallet = hb:wallet(),
    Msg = hb_message:attest(#{
        <<"path">> => <<"/init/compute/results">>,
        <<"accept-codec">> => <<"ans104@1.0">>,
        <<"codec-device">> => <<"ans104@1.0">>,
        <<"device">> => <<"WASM-64@1.0">>,
        <<"wasm-function">> => <<"fac">>,
        <<"wasm-params">> => [3.0],
        <<"body">> => Bin
    }, Wallet, <<"ans104@1.0">>),
    ?event({msg, Msg}),
    {ok, Res} = post(URL, Msg, #{}),
    ?event({res, Res}),
    ?assertEqual(6.0, hb_converge:get(<<"output/1">>, Res, #{})).