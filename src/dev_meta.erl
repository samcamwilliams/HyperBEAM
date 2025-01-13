%%% @doc The hyperbeam meta device, which is the default entry point
%%% for all messages processed by the machine. This device executes a
%%% Converge singleton request, after first applying the node's 
%%% pre-processor, if set.
-module(dev_meta).
-export([handle/2]).
-include("include/hb.hrl").

%% @doc Normalize and route messages downstream based on their path. Messages
%% with a `Meta` key are routed to the `handle_meta/2` function, while all
%% other messages are routed to the `handle_converge/2` function.
handle(NodeMsg, RawRequest) ->
    ?event(debug, {request, RawRequest}),
    NormRequest = hb_singleton:from(RawRequest),
    ?event(debug, {norm_request, NormRequest}),
    case is_meta_request(NormRequest) of
        true ->
            handle_meta(NormRequest, NodeMsg);
        false ->
            handle_converge(NormRequest, NodeMsg)
    end.

%% @doc Handle a potential list of messages, checking if the first message
%% has a path of `Meta`.
is_meta_request([PrimaryMsg | _]) -> hb_path:hd(PrimaryMsg, #{}) == <<"Meta">>;
is_meta_request(_) -> false.

handle_meta([Request|_], NodeMsg) ->
    case hb_converge:get(<<"method">>, Request, NodeMsg) of
        <<"GET">> ->
            {ok, hb_private:reset(Request, NodeMsg)};
        <<"POST">> ->
            ReqSigners = hb_message:signers(Request, NodeMsg),
            Owner = hb_opts:get(owner, no_owner_set, NodeMsg),
            case lists:member(Owner, ReqSigners) of
                false ->
                    {error, {unauthorized, Request}};
                true ->
                    hb_http_server:setops(NodeMsg),
                    {ok, <<"OK">>}
            end;
        _ ->
            {error, {unsupported_method, Request}}
    end.

handle_converge(Request, NodeMsg) ->
    hb_converge:resolve(Request, NodeMsg, NodeMsg).
