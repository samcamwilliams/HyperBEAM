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
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Generate a `GET /schedule' response for a process as HTTP-sig bundles.
assignments_to_bundle(ProcID, Assignments, More, Opts) ->
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    {ok, #{
        <<"type">> => <<"schedule">>,
        <<"process">> => hb_util:human_id(ProcID),
        <<"continues">> => atom_to_binary(More, utf8),
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"block-height">> => list_to_binary(integer_to_list(Height)),
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
                    {<<"node">>, assignment_to_json(Assignment, Opts)}
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

cursor(Assignment, Opts) ->
    hb_converge:get(<<"slot">>, Assignment, Opts#{ hashpath => ignore }).

assignment_to_json(Assignment, Opts) ->
    Message = hb_converge:get(<<"body">>, Assignment, Opts),
    AssignmentWithoutBody = maps:without([<<"body">>], Assignment),
    {[
        {<<"message">>,
            dev_json_iface:message_to_json_struct(Message)},
        {<<"assignment">>,
            dev_json_iface:message_to_json_struct(AssignmentWithoutBody)}
    ]}.