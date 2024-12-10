-module(hb_http).
-export([start/0]).
-export([get/1, get/2, get_binary/1]).
-export([post/2, post/3, post_binary/2]).
-export([reply/2, reply/3]).
-export([tx_to_status/1, req_to_tx/1]).
-include("include/hb.hrl").
-hb_debug(print).

%%% Hyperbeam's core HTTP request/reply functionality. The functions in this
%%% module generally take a message in request and return a response in message
%%% form. This module is mostly used by hb_client, but can also be used by other
%%% modules that need to make HTTP requests.

start() ->
    httpc:set_options([{max_keep_alive_length, 0}]).

%% @doc Gets a URL via HTTP and returns the resulting message in deserialized
%% form.
get(Host, Path) -> ?MODULE:get(Host ++ Path).
get(URL) ->
    case get_binary(URL) of
        {ok, Res} -> {ok, ar_bundles:deserialize(Res)};
        Error -> Error
    end.

%% @doc Gets a URL via HTTP and returns the raw binary body. Abstracted such that
%% we can easily swap out the HTTP client library later.
get_binary(URL) ->
    ?event({http_getting, URL}),
    case httpc:request(get, {iolist_to_binary(URL), []}, [], [{body_format, binary}]) of
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
    ?event({http_post, hb_util:id(Message, unsigned), hb_util:id(Message, signed), URL}),
    post(URL, ar_bundles:serialize(ar_bundles:normalize(Message)));
post(URL, Message) ->
    case post_binary(URL, Message) of
        {ok, Res} -> {ok, ar_bundles:deserialize(Res)};
        Error -> Error
    end.

%% @doc Posts a binary to a URL on a remote peer via HTTP, returning the raw
%% binary body.
post_binary(URL, Message) ->
    case httpc:request(
        post,
        {iolist_to_binary(URL), [], "application/octet-stream", Message},
        [],
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
    reply(Req, tx_to_status(Message), Message).
reply(Req, Status, Message) ->
    ?event(
        {
            replying,
            Status,
            maps:get(path, Req, undefined_path),
            case is_record(Message, tx) of
                true -> hb_util:id(Message);
                false -> data_body
            end
        }
    ),
    Req2 = cowboy_req:reply(
        Status,
        #{<<"Content-Type">> => <<"application/octet-stream">>},
        ar_bundles:serialize(Message),
        Req
    ),
    {ok, Req2, no_state}.

%% @doc Get the HTTP status code from a transaction (if it exists).
tx_to_status(Item) ->
    case lists:keyfind(<<"Status">>, 1, Item#tx.tags) of
        {_, RawStatus} ->
            case is_integer(RawStatus) of
                true -> RawStatus;
                false -> binary_to_integer(RawStatus)
            end;
        false -> 200
    end.

%% @doc Convert a cowboy request to a normalized transaction.
req_to_tx(Req) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    {ok, Body} = read_body(Req),
    QueryTags = cowboy_req:parse_qs(Req),
    #tx {
        tags = [
            {<<"Method">>, Method},
            {<<"Path">>, Path}
        ] ++ QueryTags,
        data =
            case Body of
                <<>> -> <<>>;
                Body -> #{ <<"1">> => ar_bundles:deserialize(Body) }
            end
    }.

%% @doc Helper to grab the full body of a HTTP request, even if it's chunked.
read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.