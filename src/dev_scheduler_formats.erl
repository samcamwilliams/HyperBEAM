%%% @doc This module is used by dev_scheduler in order to produce outputs that
%%% are compatible with various forms of AO clients. It features two main formats:
%%%
%%% - `application/json'
%%% - `application/http'
%%%
%%% The `application/json' format is a legacy format that is not recommended for
%%% new integrations of the AO protocol.
-module(dev_scheduler_formats).
-export([assignments_to_bundle/4, assignments_to_json/4]).

%% @doc Generate a `GET /schedule' response for a process as HTTP-sig bundles.
assignments_to_bundle(ProcID, Assignments, More, Opts) ->
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    {ok, #{
        <<"type">> => <<"schedule">>,
        <<"process">> => ProcID,
        <<"continues">> => atom_to_binary(More, utf8),
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"block-height">> => list_to_binary(integer_to_list(Height)),
        <<"block-hash">> => Hash,
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
assignments_to_json(_ProcID, Assignments, More, Opts) ->
    {ok, 
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => jiffy:encode(
                {[
                    {<<"page_info">>,
                        {[
                            {<<"has_next_page">>, More}
                        ]}
                    },
                    {<<"edges">>, [
                        {[
                            {<<"cursor">>, cursor(Assignment, Opts)},
                            {<<"node">>, assignment_to_json(Assignment, Opts)}
                        ]}
                        || Assignment <- Assignments
                    ]}
                ]}
            )
        }
    }.

cursor(Assignment, Opts) ->
    hb_converge:get(<<"slot">>, Assignment, Opts#{ hashpath => ignore }).

assignment_to_json(Assignment, Opts) ->
    Message = hb_converge:get(<<"body">>, Assignment, Opts),
    {[
        {<<"message">>, dev_json_iface:message_to_json_struct(Message)},
        {<<"assignment">>, dev_json_iface:message_to_json_struct(Assignment)}
    ]}.