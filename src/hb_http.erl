%%% Hyperbeam's core HTTP request/reply functionality. The functions in this
%%% module generally take a message request from their caller and return a
%%% response in message form, as granted by the peer. This module is mostly
%%% used by hb_client, but can also be used by other modules that need to make
%%% HTTP requests.
-module(hb_http).
-export([start/0]).
-export([get/1, get/2, get_binary/1]).
-export([post/2, post/3, post_binary/2]).
-export([reply/2, reply/3]).
-export([message_to_status/1, req_to_message/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
    httpc:set_options([{max_keep_alive_length, 0}]),
    ok.

%% @doc Gets a URL via HTTP and returns the resulting message in deserialized
%% form.
get(Host, Path) -> ?MODULE:get(Host ++ Path).
get(URL) ->
    case get_binary(URL) of
        {ok, Res} ->
            {ok, hb_message:convert(ar_bundles:deserialize(Res), converge, tx, #{})};
        Error -> Error
    end.

%% @doc Gets a URL via HTTP and returns the raw binary body. Abstracted such that
%% we can easily swap out the HTTP client library later.
get_binary(URL) ->
    ?event({http_getting, URL}),
    NormURL = iolist_to_binary(URL),
    case httpc:request(get, {NormURL, []}, [], [{body_format, binary}]) of
        {ok, {{_, 500, _}, _, Body}} ->
            ?event({http_got_server_error, URL}),
            {error, Body};
        {ok, {{_, _, _}, _, Body}} ->
            ?event({http_got, URL}),
            {ok, Body}
    end.

%% @doc Posts a message to a URL on a remote peer via HTTP. Returns the
%% resulting message in deserialized form.
post(Host, Path, Message) -> post(Host ++ Path, Message).
post(URL, Message) when not is_binary(Message) ->
    ?event(
        {
            http_post,
            hb_util:id(Message, unsigned),
            hb_util:id(Message, signed),
            URL
        }
    ),
    post(URL, ar_bundles:serialize(hb_message:convert(Message, tx, #{})));
post(URL, Message) ->
    case post_binary(URL, Message) of
        {ok, Res} ->
            {ok, hb_message:convert(ar_bundles:deserialize(Res), converge, tx, #{})};
        Error -> Error
    end.

%% @doc Posts a binary to a URL on a remote peer via HTTP, returning the raw
%% binary body.
post_binary(URL, Message) ->
    case httpc:request(
        post,
        {iolist_to_binary(URL), [], "application/octet-stream", Message},
        [{timeout, 300}, {connect_timeout, 300}],
        [{body_format, binary}]
    ) of
        {ok, {{_, Status, _}, _, Body}} when Status == 200; Status == 201 ->
            {
                case Status of
                    200 -> ok;
                    201 -> created
                end,
                Body
            };
        Response ->
            ?event({http_post_error, URL, Response}),
            {error, Response}
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
    URL = hb_http_server:start_test_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [10]),
    {ok, Res} = post(URL, Msg),
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
