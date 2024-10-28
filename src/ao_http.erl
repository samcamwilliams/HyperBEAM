-module(ao_http).
-export([start/0, get/1, get/2, post/3, reply/2, reply/3]).
-export([tx_to_status/1, req_to_tx/1]).
-include("include/ao.hrl").

start() ->
    httpc:set_options([{max_keep_alive_length, 0}]).

get(Host, Path) -> ?MODULE:get(Host ++ Path).
get(URL) ->
    ?c({http_getting, URL}),
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            ?c({http_got, URL}),
            {ok, ar_bundles:deserialize(Body)};
        Response ->
            {error, Response}
    end.

post(Host, Path, Item) -> post(Host ++ Path, Item).
post(URL, Item) ->
    ?c({http_post, ar_util:id(Item#tx.id), URL}),
    case httpc:request(
        post,
        {URL, [], "application/octet-stream", ar_bundles:serialize(Item)},
        [],
        [{body_format, binary}]
    ) of
        {ok, {{_, Status, _}, _, Body}} when Status == 200; Status == 201 ->
            {
                case Status of
                    200 -> ok;
                    201 -> created
                end,
                ar_bundles:deserialize(Body)
            };
        Response ->
            ?c({http_post_error, URL, Response}),
            {error, Response}
    end.

reply(Req, Item) ->
    reply(Req, tx_to_status(Item), Item).
reply(Req, Status, Item) ->
    ?c(
        {replying,
            Status,
            maps:get(method, Req, undef_method),
            maps:get(path, Req, undef_path),
            Ref = case is_record(Item, tx) of true -> ar_util:id(Item#tx.id); false -> data_body end}
    ),
    % TODO: Should we return Req or Req2? Req2 sometimes seems to have issues,
    % but logically appears to be the correct choice.
    Req2 = cowboy_req:reply(
        Status,
        #{<<"Content-Type">> => <<"application/octet-stream">>},
        ar_bundles:serialize(Item),
        Req
    ),
    ?c({replied, Status, Ref}),
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

read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.