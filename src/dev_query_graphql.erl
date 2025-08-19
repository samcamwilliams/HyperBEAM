%%% @doc A GraphQL interface for querying a node's cache. Accessible through the
%%% `~query@1.0/graphql' device key.
-module(dev_query_graphql).
%%% AO-Core API:
-export([handle/3]).
%%% GraphQL Callbacks:
-export([start/1, execute/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_TIMEOUT, 3000).

%% @doc Returns the complete GraphQL schema.
schema() ->
    <<"""
    input KeyInput {
        name: String!
        value: String!
    }

    type Message {
        id: ID!
        keys: [Key]
    }

    type Key {
        name: String
        value: String
    }

    type Query {
        message(keys: [KeyInput]): Message
    }
    """>>.

%% @doc Create the GraphQL schema and context.
start(_Opts) ->
    Map =
        #{
            scalars => #{ default => dev_query_graphql },
            interfaces => #{ default => dev_query_graphql },
            unions => #{ default => dev_query_graphql },
            objects => #{
                'Message' => dev_query_graphql,
                'Key' => dev_query_graphql,
                'Value' => dev_query_graphql,
                'Query' => dev_query_graphql
            }
        },
    ok = graphql:load_schema(Map, schema()),
    Root =
        {root,
            #{
                query => 'Query',
                interfaces => []
            }
        },
    ok = graphql:insert_schema_definition(Root),
    ok = graphql:validate_schema().

run(QueryReq, OpName, Vars, Opts) ->
    ?event(
        {graphql_run_called,
            {query, QueryReq},
            {operation, OpName},
            {variables, Vars}
        }
    ),
    application:ensure_all_started(graphql),
    start(Opts),
    ?event(graphql_started),
    case graphql:parse(QueryReq) of
        {ok, AST} ->
            ?event({graphql_parsed, {explicit, AST}}),
            try
                {ok, #{fun_env := FunEnv, ast := AST2 }} = graphql:type_check(AST),
                ?event({graphql_type_checked_successfully, AST2}),
                ok = graphql:validate(AST2),
                ?event({graphql_validated, {explicit, AST2},{explicit, FunEnv}}),
                Coerced = graphql:type_check_params(FunEnv, OpName, Vars),
                ?event({graphql_type_checked_params, FunEnv, OpName, Vars, {explicit, Coerced}}),
                Ctx =
                    #{
                        params => Coerced,
                        operation_name => OpName,
                        default_timeout => ?DEFAULT_TIMEOUT,
                        opts => Opts
                    },
                ?event({graphql_context_created, Ctx}),
                Response = graphql:execute(Ctx, AST2),
                {ok, Response}
            catch
                throw:Error:Stacktrace ->
                    ?event({graphql_error, {error, Error}, {trace, Stacktrace}}),
                    {error, Error}
            end
    end.

execute(#{opts := Opts}, Obj, <<"message">>, #{<<"keys">> := Keys}) ->
    Template = keys_to_template(Keys),
    ?event(
        {graphql_execute_called,
            {object, Obj},
            {field, <<"message">>},
            {raw_keys, Keys},
            {template, Template}
        }
    ),
    case hb_cache:match(Template, Opts) of
        {ok, [ID | _IDs]} ->
            ?event({graphql_cache_match_found, ID}),
            {ok, Msg} = hb_cache:read(ID, Opts),
            Loaded = hb_cache:ensure_all_loaded(Msg, Opts),
            ?event({graphql_cache_read, Loaded}),
            {ok, #{<<"id">> => ID, <<"keys">> => Loaded}};
        not_found ->
            ?event(graphql_cache_match_not_found),
            {ok, #{<<"id">> => <<"not-found">>, <<"keys">> => #{}}}
    end;
execute(_, Obj = #{<<"keys">> := Keys}, <<"keys">>, Args) ->
    ?event(
        {graphql_execute_called,
            {object, Obj},
            {field, <<"keys">>},
            {args, Args}
        }
    ),
    {
        ok,
        [
            {ok, #{ <<"name">> => Name, <<"value">> => Value }}
        ||
            {Name, Value} <- maps:to_list(Keys)
        ]
    };
execute(_, Obj, Field, Args) when Field =:= <<"name">> orelse Field =:= <<"value">> ->
    ?event(
        {graphql_execute_called,
            {object, Obj},
            {field, Field},
            {args, Args}
        }
    ),
    {ok, maps:get(Field, Obj, null)};
execute(_, Obj, <<"id">>, Args) ->
    ?event(
        {graphql_execute_called,
            {object, Obj},
            {field, <<"id">>},
            {args, Args}
        }
    ),
    {ok, maps:get(<<"id">>, Obj, null)};
execute(_, Obj, Field, Args) ->
    ?event(
        {graphql_unhandled_field,
            {object, Obj},
            {field, Field},
            {args, Args}
        }
    ),
    {ok, <<"Not found.">>}.

keys_to_template(Keys) ->
    maps:from_list(lists:foldl(
        fun(#{<<"name">> := Name, <<"value">> := Value}, Acc) ->
            [{Name, Value} | Acc]
        end,
        [],
        Keys
    )).

message_to_keys(Msg) ->
    maps:fold(
        fun(Name, Value, Acc) ->
            [{Name, Value} | Acc]
        end,
        [],
        maps:to_list(Msg)
    ).

handle(Base, Req, Opts) ->
    QueryDoc =
        hb_ao:get_first(
            [
                {Req, <<"query">>},
                {Base, <<"query">>},
                {Req, <<"body">>},
                {Base, <<"body">>}
            ],
            Opts
        ),
    % TODO: Should we throw an error if QueryDoc/Query is not found?
    % As there is no default query, but rest can be defaulted.
    Query = hb_maps:get(<<"query">>, QueryDoc),
    OpName = hb_maps:get(<<"operationName">>, QueryDoc, undefined),
    Vars = hb_maps:get(<<"variables">>, QueryDoc, #{}),
    case run(Query, OpName, Vars, Opts) of
        {ok, Response} ->
            {ok, Response};
        {error, Error} ->
            {error, Error}
    end.

%%% Tests
lookup_test() ->
    {ok, Opts, _} = dev_query:test_setup(),
    Node = hb_http_server:start_node(Opts),
    {ok, Res} =
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"~query@1.0/graphql">>,
                <<"body">> =>
                    #{
                        <<"query">> => 
                            <<""" 
                                query GetMessage { 
                                    message(
                                        keys: [{ name: "basic", value: "binary-value" }]
                                    ) {
                                        id
                                        keys {
                                        name
                                        value
                                        }
                                    }
                                }
                            """>>,
                        <<"operationName">> => <<"GetMessage">>
                    }
            },
            Opts
        ),
    ?event({test_response, Res}),
    ?assertMatch(
        #{ <<"data">> := 
            #{ 
                <<"message">> :=
                    #{ 
                        <<"id">> := _,
                        <<"keys">> := 
                            [
                                #{ 
                                    <<"name">> := <<"basic">>,
                                    <<"value">> := <<"binary-value">>
                                },
                                #{ 
                                    <<"name">> := <<"basic-2">>,
                                    <<"value">> := <<"binary-value-2">> 
                                }
                            ] 
                    } 
            } 
        },
        Res
    ).

%%% Tests for the GraphQL interface of the dev_query module.
%%% This test checks if the GraphQL query can be executed with variables.
%%% NEED_TO_BE_FIXED: due to `application:ensure_all_started(graphql)` in `run/4`, only one
%%% test can be run at a time, as it will load the schema and context.
lookup_with_vars_test() ->
    {ok, Opts, _} = dev_query:test_setup(),
    Node = hb_http_server:start_node(Opts),
    {ok, Res} =
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"~query@1.0/graphql">>,
                <<"body">> =>
                    #{
                        <<"query">> => 
                            <<""" 
                                query GetMessage($keys: [KeyInput]) { 
                                    message(
                                        keys: $keys
                                    ) {
                                        id
                                        keys {
                                        name
                                        value
                                        }
                                    }
                                }
                            """>>,
                        <<"operationName">> => <<"GetMessage">>,
                        <<"variables">> => #{
                            <<"keys">> => 
                                [
                                    #{
                                        <<"name">> => <<"basic">>,
                                        <<"value">> => <<"binary-value">>
                                    }
                                ]
                        }
                    }
            },
            Opts
        ),
    ?event({test_response, Res}),
    ?assertMatch(
        #{ <<"data">> := 
            #{ 
                <<"message">> :=
                    #{ 
                        <<"id">> := _,
                        <<"keys">> := 
                            [
                                #{ 
                                    <<"name">> := <<"basic">>,
                                    <<"value">> := <<"binary-value">>
                                },
                                #{ 
                                    <<"name">> := <<"basic-2">>,
                                    <<"value">> := <<"binary-value-2">> 
                                }
                            ] 
                    } 
            } 
        },
        Res
    ).