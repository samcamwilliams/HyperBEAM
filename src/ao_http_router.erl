-module(ao_http_router).
-export([start/1, allowed_methods/2, read_body/1, init/2]).
-include("include/ao.hrl").

start(Mods) ->
    ao_http:start(),
    application:ensure_all_started(cowboy),
    Dispatcher = cowboy_router:compile(
        [
            {'_',
                lists:flatten(
                    lists:map(
                        fun(Mod) ->
                            {Namespace, Routes} = Mod:routes(),
                            lists:map(
                                fun(Route) ->
                                    {Namespace ++ Route, ?MODULE, Mod}
                                end,
                                Routes
                            )
                        end,
                        Mods
                    )
                )
            }
        ]
    ),
    cowboy:start_clear(
        ?MODULE,
        [{port, ao:get(http_port)}],
        #{env => #{dispatch => Dispatcher}}
    ).

init(Req, Mod) ->
    Start = ao:now(),
    Method = cowboy_req:method(Req),
    [_Mod | SplitPath] = split_path(cowboy_req:path(Req)),
    case Mod:handle(Method, SplitPath, Req) of
        {ok, Req2} ->
            ?c({request_handled, ao:now() - Start}),
            {ok, Req2, <<>>};
        Other -> Other
    end.

split_path(Path) ->
    binary:split(Path, <<"/">>, [global, trim_all]).

read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.