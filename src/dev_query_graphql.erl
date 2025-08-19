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

run(QueryBin, Operation, Req, Opts) ->
    ?event(
        {graphql_run_called,
            {query, QueryBin},
            {operation, Operation},
            {request, Req}
        }
    ),
    application:ensure_all_started(graphql),
    start(Opts),
    ?event(graphql_started),
    case graphql:parse(QueryBin) of
        {ok, AST} ->
            ?event({graphql_parsed, {explicit, AST}}),
            try
                {ok, #{fun_env := FunEnv, ast := AST2 }} = graphql:type_check(AST),
                ?event({graphql_type_checked_successfully, AST2}),
                ok = graphql:validate(AST2),
                ?event({graphql_validated, {explicit, AST2}}),
                % Coerced = graphql:type_check_params(FunEnv, Operation, Req),
                % ?event({graphql_type_checked_params, FunEnv, Operation, Req}),
                % Ctx = #{ params => Coerced, operation_name => Operation },
                % ?event({graphql_context_created, Ctx}),
                Ctx =
                    #{
                        params => #{},
                        default_timeout => ?DEFAULT_TIMEOUT,
                        opts => Opts
                    },
                Response = graphql:execute(Ctx, AST2),
                ?event({graphql_response, Response}),
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
    Query =
        hb_ao:get_first(
            [
                {Req, <<"query">>},
                {Base, <<"query">>},
                {Req, <<"body">>},
                {Base, <<"body">>}
            ],
            Opts
        ),
    case run(Query, <<"query">>, Req, Opts) of
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
                    <<"""
                    query Q { message(keys: [{ name: "basic", value: "binary-value" }])
                        { id keys { name value } }
                    }
                    """>>
            },
            Opts
        ),
    ?assertMatch(
        {
            ok,
            [
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
            ]
        },
        Res
    ),
    ok.