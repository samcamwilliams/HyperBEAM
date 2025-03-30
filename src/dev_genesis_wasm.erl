%%% @doc A device that mimics an environment suitable for `legacynet` AO 
%%% processes, using HyperBEAM infrastructure. This allows existing `legacynet`
%%% AO process definitions to be used in HyperBEAM.
-module(dev_genesis_wasm).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% @doc Initialize the device.
init(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc All the `delegated-compute@1.0` device to execute the request. We then apply
%% the `patch@1.0` device, applying any state patches that the AO process may have
%% requested.
compute(Msg, Msg2, Opts) ->
    case hb_converge:resolve(Msg, {as, <<"delegated-compute@1.0">>, Msg2}, Opts) of
        {ok, Msg3} ->
            {ok, Msg4} = hb_converge:resolve(Msg3, {as, <<"patch@1.0">>, Msg2}, Opts),
            {ok, Msg4};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Normalize the device.
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc Snapshot the device.
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.