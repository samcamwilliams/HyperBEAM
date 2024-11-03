-module(ao_http_router).
-export([start/0, allowed_methods/2, init/2]).
-include("include/ao.hrl").

start() ->
    ao_http:start(),
    application:ensure_all_started(cowboy),
    Dispatcher = cowboy_router:compile([{'_', [{'_', ?MODULE, init_state}]}]),
    cowboy:start_clear(
        ?MODULE,
        [{port, ao:get(http_port)}],
        #{env => #{dispatch => Dispatcher}}
    ).

init(Req, _) ->
    case cu_device:call(dev_meta, execute, [ao_http:req_to_tx(Req)]) of
        {ok, ResultingMessage} when is_record(ResultingMessage, tx) or is_map(ResultingMessage) ->
            % If the device returns a message (either normalized or not),
            % we normalize and serialize it, returning it to the client.
            % If the message contains a `Status` tag, we use it as the HTTP
            % status code, otherwise we default to 200.
            NormMessage = ar_bundles:normalize(ResultingMessage),
            Signed =
                case ar_bundles:is_signed(NormMessage) of
                    true -> NormMessage;
                    false -> ar_bundles:sign_item(NormMessage, ao:wallet())
                end,
            ao_http:reply(
                Req,
                ao_http:tx_to_status(Signed),
                Signed
            );
        no_match ->
            % If the device returns no_match, we return a 404.
            ao_http:reply(Req, 500, #{});
        Other ->
            % If the device returns anything else, we return it as is.
            % The assumption is that this would be a return from an upstream
            % cowboy interaction (new state, etc).
            Other
    end.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.