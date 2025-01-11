%%% Hyperbeam's core HTTP request/reply functionality. The functions in this
%%% module generally take a message request from their caller and return a
%%% response in message form, as granted by the peer. This module is mostly
%%% used by hb_client, but can also be used by other modules that need to make
%%% HTTP requests.
-module(hb_http).
-export([start/0]).
-export([get/1, get/2, post/2, post/3, request/3, request/4]).
-export([reply/2, reply/3]).
-export([message_to_status/1, req_to_message/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
    httpc:set_options([{max_keep_alive_length, 0}]),
    ok.

%% @doc Gets a URL via HTTP and returns the resulting message in deserialized
%% form.
get(Node) -> get(Node, <<"/">>).
get(Node, Path) ->
    case request(get, Node, Path, #{}) of
        {ok, Body} ->
            {ok,
                hb_message:convert(
                    ar_bundles:deserialize(Body),
                    converge,
                    tx,
                    #{}
                )
            };
        Error -> Error
    end.

%% @doc Posts a message to a URL on a remote peer via HTTP. Returns the
%% resulting message in deserialized form.
post(Node, Message) ->
    post(Node, <<"/">>, Message).
post(Node, Path, Message) when not is_binary(Message) ->
    ?event(
        {
            http_post,
            Node,
            Path,
            hb_util:id(Message, unsigned),
            hb_util:id(Message, signed)
        }
    ),
    post(Node, Path, ar_bundles:serialize(hb_message:convert(Message, tx, #{})));
post(Node, Path, Message) ->
    case request(post, Node, Path, Message) of
        {ok, Res} ->
            {ok,
                hb_message:convert(
                    ar_bundles:deserialize(Res),
                    converge,
                    tx,
                    #{}
                )
            };
        Error -> Error
    end.

%% @doc Posts a binary to a URL on a remote peer via HTTP, returning the raw
%% binary body.
request(Method, Node, Path) ->
    request(Method, Node, Path, #{}).
request(Method, Config, Path, Message) when is_map(Config) ->
    multirequest(Config, Method, Path, Message);
request(Method, Node, Path, Message) ->
    BinNode = if is_binary(Node) -> Node; true -> list_to_binary(Node) end,
    BinPath = hb_path:normalize(hb_path:to_binary(Path)),
    FullPath = <<BinNode/binary, BinPath/binary>>,
    Req =
        case Method of
            post -> {FullPath, [], "application/octet-stream", Message};
            get -> {FullPath, []}
        end,
    case httpc:request(Method, Req, [], [{body_format, binary}]) of
        {ok, {{_, Status, _}, _, Body}} when Status >= 200, Status < 300 ->
            ?event({http_got, BinNode, BinPath, Status}),
            {
                case Status of
                    201 -> created;
                    _ -> ok
                end,
                Body
            };
        {ok, {{_, Status, _}, _, Body}} when Status == 400 ->
            ?event({http_got_client_error, BinNode, BinPath}),
            {error, Body};
        {ok, {{_, Status, _}, _, Body}} when Status > 400 ->
            ?event({http_got_server_error, BinNode, BinPath}),
            {unavailable, Body};
        Response ->
            ?event({http_error, BinNode, BinPath, Response}),
            {error, Response}
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
multirequest(Config, Method, Path, Message) ->
    Nodes = hb_converge:get(<<"Nodes">>, Config, #{}),
    Responses = hb_converge:get(<<"Responses">>, Config, 1),
    StopAfter = hb_converge:get(<<"Stop-After">>, Config, true),
    case hb_converge:get(<<"Parallel">>, Config, false) of
        false ->
            serial_multirequest(
                Nodes, Responses, Method, Path, Message);
        true ->
            parallel_multirequest(
                Nodes, Responses, StopAfter, Method, Path, Message)
    end.

serial_multirequest(_Nodes, 0, _Method, _Path, _Message) -> [];
serial_multirequest([Node | Nodes], Remaining, Method, Path, Message) ->
    case request(Method, Node, Path, Message) of
        {Status, Res} when Status == ok; Status == error ->
            [
                {Status, Res}
            |
                serial_multirequest(Nodes, Remaining - 1, Method, Path, Message)
            ];
        _ ->
            serial_multirequest(Nodes, Remaining, Method, Path, Message)
    end.

%% @doc Dispatch the same HTTP request to many nodes in parallel.
parallel_multirequest(Nodes, Responses, StopAfter, Method, Path, Message) ->
    Ref = make_ref(),
    Parent = self(),
    Procs = lists:map(
        fun(Node) ->
            spawn(
                fun() ->
                    Res = request(Method, Node, Path, Message),
                    receive no_reply -> stopping
                    after 0 -> Parent ! {Ref, self(), Res}
                    end
                end
            )
        end,
        Nodes
    ),
    parallel_responses([], Procs, Ref, Responses, StopAfter).

%% @doc Collect the necessary number of responses, and stop workers if
%% configured to do so.
parallel_responses(Res, Procs, Ref, 0, false) ->
    lists:foreach(fun(P) -> P ! no_reply end, Procs),
    empty_inbox(Ref),
    {ok, Res};
parallel_responses(Res, Procs, Ref, 0, true) ->
    lists:foreach(fun(P) -> exit(P, kill) end, Procs),
    empty_inbox(Ref),
    Res;
parallel_responses(Res, Procs, Ref, Awaiting, StopAfter) ->
    receive
        {Ref, Pid, {Status, NewRes}} when Status == ok; Status == error ->
            parallel_responses(
                [NewRes | Res],
                lists:delete(Pid, Procs),
                Ref,
                Awaiting - 1,
                StopAfter
            );
        {Ref, Pid, _} ->
            parallel_responses(
                Res,
                lists:delete(Pid, Procs),
                Ref,
                Awaiting,
                StopAfter
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
reply(Req, Message) ->
    reply(Req, message_to_status(Message), Message).
reply(Req, Status, Message) ->
    TX = hb_message:convert(Message, tx, converge, #{}),
    ?event(
        {replying,
            {status, Status},
            {path, maps:get(path, Req, undefined_path)},
            {tx, TX}
        }
    ),
    Req2 = cowboy_req:reply(
        Status,
        #{<<"Content-Type">> => <<"application/octet-stream">>},
        ar_bundles:serialize(TX),
        Req
    ),
    {ok, Req2, no_state}.

%% @doc Get the HTTP status code from a transaction (if it exists).
message_to_status(Item) ->
    case dev_message:get(<<"Status">>, Item) of
        {ok, RawStatus} ->
            case is_integer(RawStatus) of
                true -> RawStatus;
                false -> binary_to_integer(RawStatus)
            end;
        _ -> 200
    end.

%% @doc Convert a cowboy request to a normalized message.
req_to_message(Req, Opts) ->
    {ok, Body} = read_body(Req),
    hb_message:convert(ar_bundles:deserialize(Body), converge, tx, Opts).

%% @doc Helper to grab the full body of a HTTP request, even if it's chunked.
read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

%%% Tests

simple_converge_resolve_test() ->
    URL = hb_http_server:start_test_node(),
    {ok, Res} =
        post(
            URL,
            #{
                path => <<"Key1">>,
                <<"Key1">> =>
                    #{<<"Key2">> =>
                        #{
                            <<"Key3">> => <<"Value2">>
                        }
                    }
            }
        ),
    ?assertEqual(<<"Value2">>, hb_converge:get(<<"Key2/Key3">>, Res, #{})).

wasm_compute_request(ImageFile, Func, Params) ->
    {ok, Bin} = file:read_file(ImageFile),
    #{
        path => <<"Init/Compute/Results">>,
        device => <<"WASM-64/1.0">>,
        <<"WASM-Function">> => Func,
        <<"WASM-Params">> => Params,
        <<"Image">> => Bin
    }.

run_wasm_unsigned_test() ->
    Node = hb_http_server:start_test_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [10]),
    {ok, Res} = post(Node, Msg),
    ?assertEqual(ok, hb_converge:get(<<"Type">>, Res, #{})).

run_wasm_signed_test() ->
    URL = hb_http_server:start_test_node(#{force_signed => true}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [10]),
    {ok, Res} = post(URL, Msg),
    ?assertEqual(ok, hb_converge:get(<<"Type">>, Res, #{})).

% http_scheduling_test() ->
%     % We need the rocksdb backend to run for hb_cache module to work
%     application:ensure_all_started(hb),
%     pg:start(pg),
%     <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>>
%         = crypto:strong_rand_bytes(12),
%     rand:seed(exsplus, {I1, I2, I3}),
%     URL = hb_http_server:start_test_node(#{force_signed => true}),
%     Msg1 = dev_scheduler:test_process(),
%     Proc = hb_converge:get(process, Msg1, #{ hashpath => ignore }),
%     ProcID = hb_util:id(Proc),
%     {ok, Res} =
%         hb_converge:resolve(
%             Msg1,
%             #{
%                 path => <<"Append">>,
%                 <<"Method">> => <<"POST">>,
%                 <<"Message">> => Proc
%             },
%             #{}
%         ),
%     MsgX = #{
%         device => <<"Scheduler/1.0">>,
%         path => <<"Append">>,
%         <<"Process">> => Proc,
%         <<"Message">> =>
%             #{
%                 <<"Target">> => ProcID,
%                 <<"Type">> => <<"Message">>,
%                 <<"Test-Val">> => 1
%             }
%     },
%     Res = post(URL, MsgX),
%     ?event(debug, {post_result, Res}),
%     Msg3 = #{
%         path => <<"Slot">>,
%         <<"Method">> => <<"GET">>,
%         <<"Process">> => ProcID
%     },
%     SlotRes = post(URL, Msg3),
%     ?event(debug, {slot_result, SlotRes}).
