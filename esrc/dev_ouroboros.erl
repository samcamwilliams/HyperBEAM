%%% @doc A first-class HyperBEAM device that boots a local Ouroboros API
%%% server on demand and proxies flat device routes into it.
-module(dev_ouroboros).
-export([info/1, index/3, proxy/4, init/3, compute/3, normalize/3, snapshot/3]).

-include("include/hb.hrl").

-define(DEVICE_NAME, <<"ouroboros@1.0">>).
-define(PATH_PREFIX, <<"/~ouroboros@1.0">>).
-define(DEFAULT_PORT, 7789).
-define(HEALTH_TIMEOUT_MS, 60000).

info(_) ->
    #{
        <<"status">> => 404,
        <<"content-type">> => <<"text/plain; charset=utf-8">>,
        <<"body">> => <<"Not found">>,
        <<"index">> => fun index/3,
        handlers => #{
            <<"compute">> => fun compute/3,
            <<"init">> => fun init/3,
            <<"normalize">> => fun normalize/3,
            <<"snapshot">> => fun snapshot/3
        },
        default => fun proxy/4,
        excludes => [<<"keys">>, <<"set">>]
    }.

index(_Base, _Req, _Opts) ->
    {ok, error_response(404, <<"Not found">>)}.

init(Base, _Req, Opts) ->
    {ok, sanitize_state(Base, Opts)}.

normalize(Base, _Req, Opts) ->
    {ok, sanitize_state(Base, Opts)}.

snapshot(Base, _Req, Opts) ->
    {ok, sanitize_snapshot(Base, Opts)}.

compute(Base, Req, Opts) ->
    case ensure_started(Opts) of
        {ok, ServiceUrl} ->
            case compute_process_id(Base, Req, Opts) of
                undefined ->
                    {error, not_found};
                ProcessId ->
                    ComputePath =
                        <<"/compute?process_id=", (url_encode(ProcessId))/binary>>,
                    Args = #{
                        peer => ServiceUrl,
                        path => ComputePath,
                        method => <<"POST">>,
                        headers => #{
                            <<"content-type">> => <<"application/json">>,
                            <<"accept">> => <<"application/json">>
                        },
                        body => hb_json:encode(Req)
                    },
                    case hb_http_client:request(Args, Opts) of
                        {ok, Status, _Headers, ResponseBody}
                                when Status >= 200, Status < 300 ->
                            CleanBase = sanitize_state(Base, Opts),
                            {ok,
                                hb_ao:set(
                                    CleanBase,
                                    #{
                                        <<"results">> => #{
                                            <<"status">> => Status,
                                            <<"body">> =>
                                                decode_json_or_body(ResponseBody)
                                        }
                                    },
                                    Opts
                                )};
                        {ok, Status, _Headers, ResponseBody} ->
                            {error,
                                #{
                                    <<"status">> => Status,
                                    <<"body">> => ResponseBody
                                }};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

proxy(undefined, _Base, _Req, _Opts) ->
    {error, not_found};
proxy(<<>>, _Base, _Req, _Opts) ->
    {error, not_found};
proxy(Key, _Base, Req, Opts) ->
    case ensure_started(Opts) of
        {ok, ServiceUrl} ->
            ProxyPath = proxy_path(Key, Req, Opts),
            Headers = forward_request_headers(Req, Opts),
            Method = hb_util:bin(hb_maps:get(<<"method">>, Req, <<"GET">>, Opts)),
            Body = forward_request_body(Req, Opts),
            Args = #{
                peer => ServiceUrl,
                path => ProxyPath,
                method => Method,
                headers => Headers,
                body => Body
            },
            case hb_http_client:request(Args, Opts) of
                {ok, Status, ResponseHeaders, ResponseBody} ->
                    {ok,
                        (response_headers_map(ResponseHeaders))#{
                            <<"status">> => Status,
                            <<"body">> => ResponseBody
                        }};
                {error, Reason} ->
                    {ok,
                        error_response(
                            502,
                            iolist_to_binary(io_lib:format("~p", [Reason]))
                        )}
            end;
        {error, Reason} ->
            {ok, error_response(500, Reason)}
    end.

ensure_started(Opts) ->
    Port = service_port(Opts),
    case is_ouroboros_server_running(Port) of
        true ->
            {ok, service_url(Port)};
        false ->
            case hb_name:lookup(?DEVICE_NAME) of
                Pid when is_pid(Pid) ->
                    Pid ! stop,
                    hb_name:unregister(?DEVICE_NAME),
                    timer:sleep(100);
                _ ->
                    ok
            end,
            _ =
                hb_name:singleton(
                    ?DEVICE_NAME,
                    fun() -> launch_server(Opts) end
                ),
            case
                hb_util:wait_until(
                    fun() -> is_ouroboros_server_running(Port) end,
                    ?HEALTH_TIMEOUT_MS
                )
            of
                true -> {ok, service_url(Port)};
                false -> {error, <<"Ouroboros server did not become healthy.">>}
            end
    end.

launch_server(Opts) ->
    ServerDir = ouroboros_server_dir(Opts),
    case filelib:is_dir(ServerDir) of
        false ->
            ?event({ouroboros_server_missing, {dir, {string, ServerDir}}}),
            receive stop -> ok end;
        true ->
            PortNum = integer_to_list(service_port(Opts)),
            NodeUrl = service_node_url(Opts),
            DataDir =
                filename:absname(
                    hb_util:list(
                        hb_opts:get(
                            ouroboros_data_dir,
                            "cache-mainnet/ouroboros",
                            Opts
                        )
                    )
                ),
            filelib:ensure_path(filename:join(DataDir, "placeholder")),
            Command =
                io_lib:format(
                    "cd ~s && npm run start",
                    [shell_escape(ServerDir)]
                ),
            Env = [
                {"OUROBOROS_PORT", PortNum},
                {"OUROBOROS_DATA_PATH", DataDir},
                {"OUROBOROS_HYPERBEAM_MANAGED", "false"},
                {"OUROBOROS_HYPERBEAM_URL", hb_util:list(NodeUrl)},
                {"HB_URL", hb_util:list(NodeUrl)},
                {"OUROBOROS_HYPERBEAM_BROWSER_PATH", "/"},
                {"OUROBOROS_PUBLIC_BASE_PATH", hb_util:list(?PATH_PREFIX)}
            ],
            Port =
                open_port(
                    {spawn_executable, "/bin/bash"},
                    [
                        binary,
                        use_stdio,
                        stderr_to_stdout,
                        {args, ["-lc", lists:flatten(Command)]},
                        {env, Env}
                    ]
                ),
            collect_events(Port)
    end.

service_port(Opts) ->
    hb_opts:get(ouroboros_port, ?DEFAULT_PORT, Opts).

service_url(Port) ->
    <<"http://127.0.0.1:", (integer_to_binary(Port))/binary>>.

service_node_url(Opts) ->
    Port = public_node_port(Opts),
    Host = configured_node_host(Opts),
    <<"http://", Host/binary, ":", (integer_to_binary(Port))/binary>>.

compute_process_id(Base, Req, Opts) ->
    hb_ao:get_first(
        [
            {Req, <<"process">>},
            {Req, <<"target">>},
            {Base, <<"process/id">>},
            {Base, <<"process">>},
            {Base, <<"id">>}
        ],
        undefined,
        Opts
    ).

public_node_port(Opts) ->
    hb_opts:get(port_external, hb_opts:get(port, 8734, Opts), Opts).

configured_node_host(Opts) ->
    sanitize_host(hb_util:bin(hb_opts:get(node_host, <<"localhost">>, Opts))).

sanitize_state(Base, Opts) ->
    hb_maps:without(
        [
            <<"results">>,
            <<"results+link">>,
            <<"snapshot">>,
            <<"snapshot+link">>
        ],
        Base,
        Opts
    ).

sanitize_snapshot(Base, Opts) ->
    sanitize_state(Base, Opts).

forwarded_host(Req, Opts) ->
    RawHost =
        hb_util:bin(
            hb_maps:get(<<"host">>, Req, configured_node_host(Opts), Opts)
        ),
    Host = sanitize_host(RawHost),
    case host_has_port(Host) of
        true -> Host;
        false ->
            Port = integer_to_binary(public_node_port(Opts)),
            <<Host/binary, ":", Port/binary>>
    end.

sanitize_host(Host) when is_binary(Host) ->
    NoScheme =
        case binary:split(Host, <<"://">>) of
            [_Scheme, Rest] -> Rest;
            _ -> Host
        end,
    case binary:split(NoScheme, <<"/">>) of
        [Base | _] -> Base;
        _ -> NoScheme
    end.

host_has_port(<<"[", _/binary>> = Host) ->
    binary:match(Host, <<"]:">>) =/= nomatch;
host_has_port(Host) ->
    binary:match(Host, <<":">>) =/= nomatch.

ouroboros_server_dir(Opts) ->
    case hb_opts:get(ouroboros_server_dir, undefined, Opts) of
        undefined ->
            {ok, Cwd} = file:get_cwd(),
            Default = filename:absname(filename:join([Cwd, "..", "ouroboros"])),
            case filelib:is_dir(Default) of
                true -> Default;
                false -> filename:absname(filename:join([Cwd, "ouroboros"]))
            end;
        Dir ->
            filename:absname(hb_util:list(Dir))
    end.

is_ouroboros_server_running(Port) ->
    URL = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/health",
    case httpc:request(get, {URL, []}, [{timeout, 2000}], [{body_format, binary}]) of
        {ok, {{_, Status, _}, _Headers, _Body}} when Status >= 200, Status < 400 ->
            true;
        _ ->
            false
    end.

proxy_path(Key, Req, Opts) ->
    BasePath = <<"/", (hb_util:bin(Key))/binary>>,
    case request_query_string(Req, Opts) of
        <<>> -> BasePath;
        Query -> <<BasePath/binary, "?", Query/binary>>
    end.

request_query_string(Req, Opts) ->
    QueryPairs = lists:sort(request_query_pairs(Req, Opts)),
    iolist_to_binary(lists:join("&", QueryPairs)).

request_query_pairs(Req, Opts) ->
    hb_maps:fold(
        fun(Key, Value, Acc) ->
            case should_forward_query_param(Key, Value) of
                true -> [encode_query_pair(Key, Value) | Acc];
                false -> Acc
            end
        end,
        [],
        Req,
        Opts
    ).

should_forward_query_param(Key, Value)
        when is_binary(Key), Value =/= undefined ->
    not is_internal_key(Key) andalso not is_header_key(Key);
should_forward_query_param(_, _) ->
    false.

forward_request_body(Req, Opts) ->
    case hb_maps:get(<<"body">>, Req, <<>>, Opts) of
        Body when is_binary(Body) -> Body;
        undefined -> <<>>;
        Other -> hb_json:encode(Other)
    end.

forward_request_headers(Req, Opts) ->
    Headers =
        hb_maps:fold(
            fun(Key, Value, Acc) ->
                case should_forward_header(Key, Value) of
                    true -> Acc#{ Key => header_value(Value) };
                    false -> Acc
                end
            end,
            #{},
            Req,
            Opts
        ),
    Headers#{ <<"host">> => forwarded_host(Req, Opts) }.

should_forward_header(Key, Value)
        when is_binary(Key), Value =/= undefined ->
    is_header_key(Key)
        andalso not lists:member(
            Key,
            [<<"content-length">>, <<"transfer-encoding">>, <<"connection">>]
        );
should_forward_header(_, _) ->
    false.

is_internal_key(Key) ->
    lists:member(
        Key,
        [
            <<"accept-bundle">>,
            <<"accept-codec">>,
            <<"ao-core">>,
            <<"body">>,
            <<"codec-device">>,
            <<"device">>,
            <<"method">>,
            <<"path">>,
            <<"priv">>,
            <<"request">>,
            <<"signature">>,
            <<"signature-input">>,
            <<"status">>,
            <<"type">>
        ]
    ).

is_header_key(Key) ->
    binary:match(Key, <<"-">>) =/= nomatch
        orelse lists:member(
            Key,
            [
                <<"accept">>,
                <<"authorization">>,
                <<"cache-control">>,
                <<"content-type">>,
                <<"cookie">>,
                <<"host">>,
                <<"if-modified-since">>,
                <<"if-none-match">>,
                <<"origin">>,
                <<"pragma">>,
                <<"range">>,
                <<"referer">>,
                <<"user-agent">>
            ]
        ).

encode_query_pair(Key, Value) ->
    EncKey = url_encode(Key),
    EncValue = url_encode(query_value(Value)),
    <<EncKey/binary, "=", EncValue/binary>>.

query_value(Value) when is_binary(Value) -> Value;
query_value(Value) when is_integer(Value) -> integer_to_binary(Value);
query_value(true) -> <<"true">>;
query_value(false) -> <<"false">>;
query_value(Value) when is_list(Value) -> iolist_to_binary(Value);
query_value(Value) -> hb_json:encode(Value).

url_encode(Value) ->
    hb_util:bin(uri_string:quote(hb_util:list(Value))).

header_value(Value) when is_binary(Value) -> Value;
header_value(Value) when is_integer(Value) -> integer_to_binary(Value);
header_value(true) -> <<"true">>;
header_value(false) -> <<"false">>;
header_value(Value) when is_list(Value) -> iolist_to_binary(Value);
header_value(Value) -> hb_util:bin(Value).

response_headers_map(Headers) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            BinKey = hb_util:bin(string:lowercase(hb_util:list(Key))),
            BinValue = hb_util:bin(Value),
            case maps:get(BinKey, Acc, undefined) of
                undefined -> Acc#{ BinKey => BinValue };
                Existing when is_list(Existing) ->
                    Acc#{ BinKey => Existing ++ [BinValue] };
                Existing ->
                    Acc#{ BinKey => [Existing, BinValue] }
            end
        end,
        #{},
        Headers
    ).

error_response(Status, Body) ->
    #{
        <<"status">> => Status,
        <<"content-type">> => <<"text/plain; charset=utf-8">>,
        <<"body">> => Body
    }.

decode_json_or_body(Body) when is_binary(Body) ->
    try hb_json:decode(Body)
    catch _:_ -> Body
    end;
decode_json_or_body(Body) ->
    Body.

collect_events(Port) ->
    collect_events(Port, <<>>).

collect_events(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_events(Port, log_server_events(<<Acc/binary, Data/binary>>));
        stop ->
            try port_close(Port)
            catch _:_ -> ok
            end,
            ok;
        {'EXIT', Port, _Reason} ->
            ok
    end.

log_server_events(Bin) when is_binary(Bin) ->
    log_server_events(binary:split(Bin, <<"\n">>, [global]));
log_server_events([Remaining]) ->
    Remaining;
log_server_events([Line | Rest]) ->
    ?event(ouroboros_server, {server_logged, {string, Line}}),
    log_server_events(Rest).

shell_escape(Path) ->
    lists:flatten(io_lib:format("'~s'", [string:replace(Path, "'", "'\"'\"'", all)])).
