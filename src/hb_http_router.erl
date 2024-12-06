-module(hb_http_router).
-export([start/0, allowed_methods/2, init/2]).
-include("include/hb.hrl").

start() ->
    hb_http:start(),
    Dispatcher =
        cowboy_router:compile(
            [
                % {HostMatch, list({PathMatch, Handler, InitialState})}
                {'_', [
                    {
                        "/metrics/[:registry]",
                        prometheus_cowboy2_handler,
                        #{}
                    },
                    {
                        '_',
                        ?MODULE,
                        % The default state/opts for executions from the
                        % HTTP API. This would be the appropriate place
                        % to add components (a differently scoped store,
                        % a different wallet, etc.) to execution for
                        % remote clients.
                        #{
                            store => hb_opts:get(store),
                            wallet => hb_opts:get(wallet),
                            error_strategy => hb_opts:get(client_error_strategy)
                        }
                    }
                ]}
            ]
        ),

    cowboy:start_clear(
        ?MODULE,
        [{port, hb_opts:get(http_port)}],

        #{
            env => #{dispatch => Dispatcher},
            metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
            stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
        }
    ).

init(Req, State) ->
    Path = cowboy_req:path(Req),
    ?event({http_called_with_path, Path}),
    % Note: We pass the state twice in the call below: Once for the device
    % to optionally have it if useful, and once for the hb_pam module
    % itself. This lets us send execution environment parameters as well as
    % the request itself to the device.
    case hb_pam:resolve(dev_meta, execute, [hb_http:req_to_tx(Req), State], State) of
        {ok, ResultingMessage} when is_record(ResultingMessage, tx) or is_map(ResultingMessage) ->
            % If the device returns a message (either normalized or not),
            % we normalize and serialize it, returning it to the client.
            % If the message contains a `Status` tag, we use it as the HTTP
            % status code, otherwise we default to 200.
            NormMessage = ar_bundles:normalize(ResultingMessage),
            Signed =
                case ar_bundles:is_signed(NormMessage) of
                    true -> NormMessage;
                    false -> ar_bundles:sign_item(NormMessage, hb:wallet())
                end,
            hb_http:reply(
                Req,
                hb_http:tx_to_status(Signed),
                Signed
            );
        no_match ->
            % If the device returns no_match, we return a 404.
            ?event({could_not_match_path_to_device, Path}),
            hb_http:reply(
                Req,
                #tx{
                    tags = [{<<"Status">>, <<"500">>}],
                    data = <<"No matching device found.">>
                }
            )
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.