-module(dev_json_iface).
-export([init/1, execute/2, uses/0, stdlib/6, lib/6]).
-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

uses() -> all.

init(S) ->
    {ok, S#{ wasm_stdlib => fun stdlib/6, json_iface => #{}}}.

execute(M, S = #{pass := 1, json_iface := IfaceS}) ->
    {ok, S#{
        call => prep_call(M, S),
        json_iface => IfaceS#{stdout => []}
    }};
execute(_M, S = #{pass := 2}) ->
    {ok, results(S)};
execute(_M, S) ->
    {ok, S}.

prep_call(
    #tx{
        data = #{
            <<"Assignment">> := Assignment,
            <<"Message">> := Message
        }
    },
    #{ wasm := Port, process := Process }
) ->
    {_, Module} = lists:keyfind(<<"Module">>, 1, Process#tx.tags),
    % TODO: Get block height from the assignment message
    {_, BlockHeight} = lists:keyfind(<<"Block-Height">>, 1, Assignment#tx.tags),
    RawMsgJson = ao_message:serialize(Message, json),
    {Props} = jiffy:decode(RawMsgJson),
    MsgJson = jiffy:encode({
        Props ++
            [
                {<<"Module">>, Module},
                {<<"Block-Height">>, BlockHeight}
            ]
    }),
    {ok, MsgJsonPtr} = ao_beamr_io:write_string(Port, MsgJson),
    EnvJson = jiffy:encode({[{<<"Process">>, ar_bundles:item_to_json_struct(Process)}]}),
    {ok, EnvJsonPtr} = ao_beamr_io:write_string(Port, EnvJson),
    {"handle", [MsgJsonPtr, EnvJsonPtr]}.

%% NOTE: After the process returns messages from an evaluation, the signing unit needs to add
%% some tags to each message, and spawn to help the target process know these messages are created
%% by a process.
postProcessResultMessages(Msg = #{<<"Tags">> := Tags}, Proc) ->
    % Remove "From-Process" and "From-Image" from Tags
    FilteredTags = lists:filter(
        fun(Item) ->
            maps:get(<<"name">>, Item) =/= <<"From-Process">> andalso
                maps:get(<<"name">>, Item) =/= <<"From-Image">>
        end,
        Tags
    ),
    UpdatedMsg =
        Msg#{
            <<"Tags">> =>
                FilteredTags ++
                    [
                        #{
                            <<"name">> => <<"From-Process">>,
                            <<"value">> => ao_message:id(Proc, signed)
                        },
                        #{
                            <<"name">> => <<"From-Image">>,
                            <<"value">> =>
                                element(2, lists:keyfind(<<"Image">>, 1, Proc#tx.tags))
                        }
                    ]
        },
    maps:remove(<<"Anchor">>, UpdatedMsg).

results(S = #{wasm := Port, results := Res, process := Proc}) ->
    case Res of
        {error, Res} ->
            S#{
                outbox := [],
                results := #tx{tags = [{<<"Result">>, <<"Error">>}], data = Res}
            };
        {ok, [Ptr]} ->
            {ok, Str} = ao_beamr_io:read_string(Port, Ptr),
            Wallet = ao:wallet(),
            try jiffy:decode(Str, [return_maps]) of
                % TODO: Handle all JSON interface outputs
                #{<<"ok">> := true, <<"response">> := Resp} ->
                    #{<<"Output">> := #{<<"data">> := Data}, <<"Messages">> := Messages} = Resp,
                    S#{
                        results =>
                            #{
                                <<"/Outbox">> =>
                                    maps:from_list([
                                        {
                                            list_to_binary(integer_to_list(MessageNum)),
                                            ar_bundles:sign_item(
                                                ar_bundles:json_struct_to_item(
                                                    postProcessResultMessages(Msg, Proc)
                                                ),
                                                Wallet
                                            )
                                        }
                                    ||
                                        {MessageNum, Msg} <-
                                            lists:zip(lists:seq(1, length(Messages)), Messages)
                                    ]),
                                <<"/Data">> => ar_bundles:normalize(#tx{data = Data})
                            }
                    };
                #{<<"ok">> := false} ->
                    S#{
                        outbox => [],
                        results =>
                            #{
                                <<"Error">> => <<"JSON Parse Error">>,
                                <<"Data">> => Str
                            }
                    }
            catch
                _:_ ->
                    S#{
                        outbox => [],
                        results =>
                            #{
                                <<"Error">> => <<"JSON Parse Error">>,
                                <<"Data">> => Str
                            }
                    }
            end;
        {_, Res} ->
            ok
    end.

stdlib(S, Port, ModName, FuncName, Args, Sig) ->
    Library = maps:get(library, S, #{}),
    case maps:get({ModName, FuncName}, Library, undefined) of
        undefined ->
            lib(S, Port, Args, ModName, FuncName, Sig);
        Func ->
            {arity, Arity} = erlang:fun_info(Func, arity),
            ApplicationTerms =
                lists:sublist(
                    [S, Port, Args, ModName, FuncName, Sig],
                    Arity
                ),
            erlang:apply(Func, ApplicationTerms)
    end;
stdlib(S, Port, ModName, FuncName, Args, Sig) ->
    ?event(stdlib_called_2),
    lib(S, Port, ModName, FuncName, Args, Sig).

lib(
    S = #{stdout := Stdout},
    Port,
    [_Fd, Ptr, Vecs, RetPtr],
    "wasi_snapshot_preview1",
    "fd_write",
    _Signature
) ->
    %?event({fd_write, Fd, Ptr, Vecs, RetPtr}),
    {ok, VecData} = ao_beamr_io:read_iovecs(Port, Ptr, Vecs),
    BytesWritten = byte_size(VecData),
    %?event({fd_write_data, VecData}),
    NewStdio =
        case Stdout of
            undefined ->
                [VecData];
            Existing ->
                Existing ++ [VecData]
        end,
    % Set return pointer to number of bytes written
    ao_beamr_io:write(Port, RetPtr, <<BytesWritten:64/little-unsigned-integer>>),
    {S#{stdout := NewStdio}, [0]};
lib(S, _Port, _Args, _Module, "clock_time_get", _Signature) ->
    %?event({called, wasi_clock_time_get, 1}),
    {S, [1]};
lib(S, _Port, Args, Module, Func, Signature) ->
    ?event({unimplemented_stub_called, Module, Func, Args, Signature}),
    {S, [0]}.

result_test() ->
    Test = postProcessResultMessages(
        #{
            <<"Target">> => <<"123345676">>,
            <<"Anchor">> => <<"00000000001">>,
            <<"Tags">> => [
                #{<<"name">> => <<"From-Module">>, <<"value">> => <<"5467">>}
            ]
        },
        #tx{id = <<"1234">>}
    ),
    % Test = result(#{result => {foo, <<"Error">>}}),
    ok.
