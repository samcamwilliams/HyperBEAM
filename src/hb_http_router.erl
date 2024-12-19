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
                        % The default opts for executions from the HTTP API.
                        % We force a specific store, wallet, and that 
                        % hb_converge should return a regardless of whether 
                        % the result comes wrapped in one or not.
                        #{
                            store => hb_opts:get(store),
                            wallet => hb_opts:get(wallet)
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
            metrics_callback =>
                fun prometheus_cowboy2_instrumenter:observe/1,
            stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
        }
    ).

init(Req, Opts) ->
    % Parse the HTTP request into HyerBEAM's message format.
    MsgSingleton = hb_http:req_to_message(Req),
    ?event({executing_msg_from_http, MsgSingleton}),
    % Execute the message through Converge Protocol.
    RawRes = hb_converge:resolve(MsgSingleton, Opts),
    ?event({http_result_generated, RawRes}),
    % Normalize the response.
    NormMsg = normalize_response(RawRes),
    % Transform the message into an ANS-104 transaction.
    ResTX = hb_message:message_to_tx(NormMsg),
    NormTX = ar_bundles:normalize(ResTX),
    % Sign the transaction if it's not already signed.
    Signed =
        case ar_bundles:is_signed(NormTX) of
            true -> NormTX;
            false -> ar_bundles:sign_item(NormTX, hb:wallet())
        end,
    % Respond to the client.
    hb_http:reply(
        Req,
        hb_http:message_to_status(Signed),
        Signed
    ).

%% @doc Ensure that a `hb_converge:resolve' result is normalized
%% into a HTTP message.
normalize_response(RawRes) when not is_tuple(RawRes) ->
    #{ body => RawRes };
normalize_response({ok, Message}) ->
    Message;
normalize_response(Tuple) when is_tuple(Tuple) ->
    normalize_response(tuple_to_list(Tuple));
normalize_response([Status, Body]) ->
    #{ status => Status, body => Body };
normalize_response([Status, Body | Details]) ->
    (normalize_response([Status, Body]))#{ details => Details }.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.