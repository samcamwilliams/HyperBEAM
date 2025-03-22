%%% @doc This module is used by dev_scheduler in order to produce outputs that
%%% are compatible with various forms of AO clients. It features two main formats:
%%%
%%% - `application/json'
%%% - `application/http'
%%%
%%% The `application/json' format is a legacy format that is not recommended for
%%% new integrations of the AO protocol.
-module(dev_scheduler_formats).
-export([assignments_to_bundle/4, assignments_to_aos2/4]).
-export([aos2_to_assignments/3, aos2_to_assignment/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Generate a `GET /schedule' response for a process as HTTP-sig bundles.
assignments_to_bundle(ProcID, Assignments, More, Opts) ->
    TimeInfo = ar_timestamp:get(),
    assignments_to_bundle(ProcID, Assignments, More, TimeInfo, Opts).
assignments_to_bundle(ProcID, Assignments, More, TimeInfo, Opts) ->
    {Timestamp, Height, Hash} = TimeInfo,
    {ok, #{
        <<"type">> => <<"schedule">>,
        <<"process">> => hb_util:human_id(ProcID),
        <<"continues">> => atom_to_binary(More, utf8),
        <<"timestamp">> => hb_util:int(Timestamp),
        <<"block-height">> => hb_util:int(Height),
        <<"block-hash">> => hb_util:human_id(Hash),
        <<"assignments">> =>
            maps:from_list(
                lists:map(
                    fun(Assignment) ->
                        {
                            hb_converge:get(
                                <<"slot">>,
                                Assignment,
                                Opts#{ hashpath => ignore }
                            ),
                            Assignment
                        }
                    end,
                    Assignments
                )
            )
    }}.

%%% Return legacy net-SU compatible results.
assignments_to_aos2(ProcID, Assignments, More, Opts) when is_map(Assignments) ->
    SortedKeys =
        lists:sort(
            lists:map(
                fun hb_util:int/1,
                maps:keys(
                    maps:without(
                        [<<"priv">>, <<"attestations">>],
                        Assignments
                    )
                )
            )
        ),
    ListAssignments =
        lists:map(
            fun(Key) ->
                hb_converge:get(Key, Assignments, Opts)
            end,
            SortedKeys
        ),
    assignments_to_aos2(ProcID, ListAssignments, More, Opts);
assignments_to_aos2(ProcID, Assignments, More, Opts) ->
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    BodyStruct = 
        {[
            {<<"page_info">>,
                {[
                    {<<"process">>, hb_util:human_id(ProcID)},
                    {<<"has_next_page">>, More},
                    {<<"timestamp">>, list_to_binary(integer_to_list(Timestamp))},
                    {<<"block-height">>, list_to_binary(integer_to_list(Height))},
                    {<<"block-hash">>, hb_util:human_id(Hash)}
                ]}
            },
            {<<"edges">>, [
                {[
                    {<<"cursor">>, cursor(Assignment, Opts)},
                    {<<"node">>, assignment_to_aos2(Assignment, Opts)}
                ]}
                || Assignment <- Assignments
            ]}
        ]},
    Encoded = iolist_to_binary(lists:flatten([jiffy:encode(BodyStruct)])),
    ?event({body_struct, BodyStruct}),
    ?event({encoded, {explicit, Encoded}}),
    {ok, 
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => Encoded
        }
    }.

%% @doc Generate a cursor for an assignment. This should be the slot number, at
%% least in the case of mainnet `ao.N.1' assignments. In the case of legacynet
%% (`ao.TN.1') assignments, we may want to use the assignment ID.
cursor(Assignment, Opts) ->
    hb_converge:get(<<"slot">>, Assignment, Opts#{ hashpath => ignore }).

%% @doc Convert an assignment to an AOS2-compatible JSON structure.
assignment_to_aos2(Assignment, Opts) ->
    Message = hb_converge:get(<<"body">>, Assignment, Opts),
    AssignmentWithoutBody = maps:without([<<"body">>], Assignment),
    {[
        {<<"message">>,
            dev_json_iface:message_to_json_struct(Message)},
        {<<"assignment">>,
            dev_json_iface:message_to_json_struct(AssignmentWithoutBody)}
    ]}.

%% @doc Convert an AOS2-style JSON structure to a normalized HyperBEAM
%% assignments response.
aos2_to_assignments(ProcID, Body, Opts) ->
    Assignments = maps:get(<<"edges">>, Body, Opts),
    ?event({raw_assignments, Assignments}),
    ParsedAssignments =
        lists:map(
            fun(A) -> aos2_to_assignment(A, Opts) end,
            Assignments
        ),
    ?event({parsed_assignments, ParsedAssignments}),
    TimeInfo =
        case ParsedAssignments of
            [] -> {0, 0, hb_util:encode(<<0:256>>)};
            _ ->
                Last = lists:last(ParsedAssignments),
                {
                    hb_converge:get(<<"timestamp">>, Last, Opts),
                    hb_converge:get(<<"block-height">>, Last, Opts),
                    hb_converge:get(<<"block-hash">>, Last, Opts)
                }
        end,
    assignments_to_bundle(ProcID, ParsedAssignments, false, TimeInfo, Opts).

%% @doc Create and normalize an assignment from an AOS2-style JSON structure.
%% NOTE: This method is destructive to the verifiability of the assignment.
aos2_to_assignment(A, Opts) ->
    % Unwrap the node if it is provided
    Node = maps:get(<<"node">>, A, A),
    {ok, Message} =
        hb_gateway_client:result_to_message(
            aos2_normalize_data(maps:get(<<"message">>, Node)),
            Opts
        ),
    NormalizedMessage = aos2_normalize_types(Message),
    ?event({message, Message}),
    {ok, Assignment} =
        hb_gateway_client:result_to_message(
            aos2_normalize_data(maps:get(<<"assignment">>, Node)),
            Opts
        ),
    NormalizedAssignment = aos2_normalize_types(Assignment),
    Res = NormalizedAssignment#{ <<"body">> => NormalizedMessage },
    ?event({final_assignment, Res}),
    Res.

%% @doc The `hb_gateway_client' module expects all JSON structures to at least
%% have a `data' field. This function ensures that.
aos2_normalize_data(JSONStruct) ->
    case JSONStruct of
        #{<<"data">> := _} -> JSONStruct;
        _ -> JSONStruct#{ <<"data">> => <<>> }
    end.

%% @doc Normalize an AOS2 formatted message to ensure that all field NAMES and
%% types are correct. This involves converting field names to integers and
%% specific field names to their canonical form.
%% NOTE: This will result in a message that is not verifiable! It is, however,
%% necessary for gaining compatibility with the AOS2-style scheduling API.
aos2_normalize_types(Msg = #{ <<"timestamp">> := TS }) when is_binary(TS) ->
    aos2_normalize_types(Msg#{ <<"timestamp">> => hb_util:int(TS) });
aos2_normalize_types(Msg = #{ <<"nonce">> := Nonce })
        when is_binary(Nonce) and not is_map_key(<<"slot">>, Msg) ->
    aos2_normalize_types(
        Msg#{ <<"slot">> => hb_util:int(Nonce) }
    );
aos2_normalize_types(Msg = #{ <<"epoch">> := DS }) when is_binary(DS) ->
    aos2_normalize_types(Msg#{ <<"epoch">> => hb_util:int(DS) });
aos2_normalize_types(Msg = #{ <<"slot">> := Slot }) when is_binary(Slot) ->
    aos2_normalize_types(Msg#{ <<"slot">> => hb_util:int(Slot) });
aos2_normalize_types(Msg) when not is_map_key(<<"block-hash">>, Msg) ->
    ?event({missing_block_hash, Msg}),
    aos2_normalize_types(Msg#{ <<"block-hash">> => hb_util:encode(<<0:256>>) });
aos2_normalize_types(Msg) ->
    ?event(
        {
            aos2_normalized_types,
            {msg, Msg},
            {anchor, hb_converge:get(<<"anchor">>, Msg, #{})}
        }
    ),
    Msg.