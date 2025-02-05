%%% @doc This module implements the relay device, which is responsible for
%%% relaying messages between nodes and other HTTP(S) endpoints.
%%%
%%% It can be called in either `call` or `cast` mode. In `call` mode, it
%%% returns a `{ok, Result}` tuple, where `Result` is the response from the 
%%% remote peer to the message sent. In `cast` mode, the invocation returns
%%% immediately, and the message is relayed asynchronously. No response is given
%%% and the device returns `{ok, <<"OK">>}`.
%%% 
%%% Example usage:
%%% 
%%% ```
%%%     curl /~relay@.1.0/call?method=GET?0.path=https://www.arweave.net/
%%% ```
-module(dev_relay).
-export([call/3, cast/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Execute a `call` request using a node's routes.
%% 
%% Supports the following options:
%% - `target`: The target message to relay. Defaults to the original message.
%% - `relay-path`: The path to relay the message to. Defaults to the original path.
%% - `method`: The method to use for the request. Defaults to the original method.
%% - `requires-sign`: Whether the request requires signing before dispatching.
%% Defaults to `false`.
call(M1, RawM2, Opts) ->
    {ok, BaseTarget} = hb_message:find_target(M1, RawM2, Opts),
    RelayPath =
        hb_converge:get_first(
            [
                {RawM2, <<"relay-path">>},
                {M1, <<"relay-path">>}
            ],
            Opts
        ),
    TargetMod1 =
        case RelayPath of
            not_found -> BaseTarget;
            RPath ->
                ?event({setting_path, {base_target, BaseTarget}, {relay_path, {explicit, RPath}}}),
                hb_converge:set(BaseTarget, <<"path">>, RPath, Opts#{ topic => debug })
        end,
    TargetMod2 =
        case hb_converge:get(<<"requires-sign">>, BaseTarget, false, Opts) of
            true -> hb_message:attest(TargetMod1, Opts);
            false -> TargetMod1
        end,
    Client =
        case hb_converge:get(<<"http-client">>, BaseTarget, Opts) of
            not_found -> hb_opts:get(relay_http_client, Opts);
            RequestedClient -> RequestedClient
        end,
    ?event({relaying_message, TargetMod2}),
    % Let `hb_http:request/2` handle finding the peer and dispatching the request.
    hb_http:request(TargetMod2, Opts#{ http_client => Client }).

%% @doc Execute a request in the same way as `call/3`, but asynchronously. Always
%% returns <<"OK">>.
cast(M1, M2, Opts) ->
    spawn(fun() -> call(M1, M2, Opts) end),
    {ok, <<"OK">>}.

%%% Tests

call_get_test() ->
    application:ensure_all_started([hb]),
    {ok, #{<<"body">> := Body}} =
        hb_converge:resolve(
            #{
                <<"device">> => <<"relay@1.0">>,
                <<"method">> => <<"GET">>,
                <<"path">> => <<"https://www.google.com/">>
            },
            <<"call">>,
            #{ protocol => http2 }
        ),
    ?assertEqual(true, byte_size(Body) > 10_000).