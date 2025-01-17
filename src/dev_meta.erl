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
    ?event(debug, #{ request => RawRequest }),
    NormRequest = hb_singleton:from(RawRequest),
    ?event(debug, {processing_messages, NormRequest}),
    case is_meta_request(NormRequest) of
        true -> handle_meta(NormRequest, NodeMsg);
        false -> handle_converge(NormRequest, NodeMsg)
    end.

%% @doc Handle a potential list of messages, checking if the first message
%% has a path of `Meta`.
is_meta_request([PrimaryMsg | _]) -> hb_path:hd(PrimaryMsg, #{}) == <<"meta">>;
is_meta_request(_) -> false.

%% @doc Get/set the node message based on the request method. If the request
%% is a `POST`, we check that the request is signed by the owner of the node.
%% If not, we return the node message as-is, aside all keys that are 
%% private (according to `hb_private`).
handle_meta([Request|_], NodeMsg) ->
    case hb_converge:get(<<"method">>, Request, NodeMsg) of
        <<"GET">> ->
            embed_status({ok, hb_private:reset(Request)});
        <<"POST">> ->
            ReqSigners = hb_message:signers(Request),
            Owner = hb_opts:get(owner, no_owner_set, NodeMsg),
            case lists:member(Owner, ReqSigners) of
                false ->
                    embed_status({error, <<"Unauthorized">>});
                true ->
                    hb_http_server:set_opts(NodeMsg),
                    embed_status({ok, <<"OK">>})
            end;
        _ -> embed_status({error, <<"Unsupported Method">>})
    end.

%% @doc Handle a Converge request, which is a list of messages. We apply
%% the node's pre-processor to the request first, and then resolve the request
%% using the node's Converge implementation if its response was `ok`.
%% After execution, we run the node's `postprocessor` message on the result of
%% the request before returning the result it grants back to the user.
handle_converge(Request, NodeMsg) ->
    % Apply the pre-processor to the request.
    case resolve_processor(preprocessor, Request, NodeMsg) of
        {ok, PreProcMsg} ->
            % Resolve the request message.
            {ok, Res} =
                embed_status(
                    hb_converge:resolve(
                        PreProcMsg,
                        NodeMsg#{ force_message => true }
                    )
                ),
            ?event(debug, {res, Res}),
            % Apply the post-processor to the result.
            embed_status(
                resolve_processor(
                    postprocessor,
                    Res,
                    NodeMsg
                )
            );
        Res -> embed_status(Res)
    end.

%% @doc execute a message from the node message upon the user's request.
resolve_processor(Processor, Request, NodeMsg) ->
    case hb_opts:get(Processor, undefined, NodeMsg) of
        undefined -> {ok, Request};
        ProcessorMsg ->
            hb_converge:resolve(
                ProcessorMsg,
                Request,
                NodeMsg#{ force_message => true }
            )
    end.

%% @doc Wrap the result of a device call in a status.
embed_status({Status, Res}) ->
    {ok, Res#{ <<"status-code">> => hb_http:status_code(Status) }}.