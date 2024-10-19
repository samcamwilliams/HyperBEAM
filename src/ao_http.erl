-module(ao_http).
-export([get/1, get/2, post/3, reply/2, reply/3]).

-include("include/ao.hrl").

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
            ?c({http_post_got, URL}),
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

reply(Req, Item) -> reply(Req, 200, Item).
reply(Req, Status, Item) ->
    ?c(
        {replying,
            Status,
            maps:get(method, Req, undef_method),
            maps:get(path, Req, undef_path),
            Ref = case is_record(Item, tx) of true -> ar_util:id(Item#tx.id); false -> data_body end}
    ),
    % TODO: Should we return Req or Req2?
    Req2 = cowboy_req:reply(
        Status,
        #{<<"Content-Type">> => <<"application/octet-stream">>},
        ar_bundles:serialize(Item),
        Req
    ),
    ?c({replied, Status, Ref}),
    {ok, Req}.
