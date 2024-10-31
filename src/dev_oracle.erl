-module(dev_oracle).
-export([execute/2]).
-include("include/ao.hrl").
-ao_debug(print).

execute(_, S = #{ pass := 3, results := Results, wallet := Wallet }) ->
    ?c(oracle_is_running),
    #{ <<"/Outbox">> := Outbox } = Results,
    NewOutbox = maps:map(
        fun(_, OutboxMsg) ->
            case lists:keyfind(<<"Action">>, 1, OutboxMsg#tx.tags) of
                {<<"Action">>, <<"Get-Oracle-Data">>} ->
                    ar_bundles:print(OutboxMsg),
                    ar_bundles:sign_item(get_data(OutboxMsg), Wallet);
                _ ->
                    OutboxMsg
            end
        end,
        Outbox),
    NewResults = maps:put(<<"/Outbox">>, NewOutbox, Results),
    {ok, S#{ results => NewResults }};
execute(Msg, S) ->
    {ok, S}.

get_data(TX = #tx { data = Location }) ->
    case httpc:request(Location) of
        {ok, {{_, 200, _}, _, Body}} ->
            ?c({got, Body}),
            #tx{ tags = [{<<"Action">>, <<"Receive-Data">>}], data = list_to_binary(Body) };
        _ ->
            #tx{ tags = [{<<"Action">>, <<"Receive-Data">>}], data = <<"GET FAILED">> }
    end.
