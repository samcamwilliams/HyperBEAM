-module(dev_poda).
-export([init/2, execute/3]).
-export([is_user_signed/1]).
-export([push/2]).
-include("include/ao.hrl").
-ao_debug(print).

%%% A simple exemplar decentralized proof of authority consensus algorithm
%%% for AO processes.

init(S, Params) ->
    {ok, S, extract_opts(Params)}.

extract_opts(Params) ->
    Authorities =
        lists:filtermap(
            fun({<<"Authority">>, Addr}) -> {true, Addr};
               (_) -> false end,
               Params
        ),
    {_, RawQuorum} = lists:keyfind(<<"Quorum">>, 1, Params),
    Quorum = binary_to_integer(RawQuorum),
    ?no_prod(use_real_authority_addresses),
    Addr = ar_wallet:to_address(ao:wallet()),
    #{
        authorities =>
            Authorities ++ [ar_util:encode(Addr)],
        quorum => Quorum
    }.

execute(Outer = #tx { data = #{ <<"Message">> := Msg } }, S = #{ pass := 1 }, Opts) ->
    case is_user_signed(Msg) of
        true ->
            {ok, S};
        false ->
            % For now, the message itself will be at `/Message/Message`.
            case validate_stage(1, Msg, Opts) of
                true ->
                    ?c({poda_validated, ok}),
                    % Add the validations to the VFS.
                    Atts =
                        maps:to_list(
                            case Msg of 
                                #tx { data = #{ <<"Attestations">> := #tx { data = X } }} -> X;
                                #tx { data = #{ <<"Attestations">> := X }} -> X;
                                #{ <<"Attestations">> := X } -> X
                            end
                        ),
                    VFS1 =
                        lists:foldl(
                            fun({_, Attestation}, Acc) ->
                                Id = ar_bundles:signer(Attestation),
                                Encoded = ar_util:encode(Id),
                                maps:put(
                                    <<"/Attestations/", Encoded/binary>>,
                                    Attestation#tx.data,
                                    Acc
                                )
                            end,
                            maps:get(vfs, S, #{}),
                            Atts
                        ),
                    % Update the arg prefix to include the unwrapped message.
                    {ok, S#{ vfs => VFS1, arg_prefix =>
                        [
                            % Traverse two layers of `/Message/Message` to get
                            % the actual message, then replace `/Message` with it.
                            Outer#tx{
                                data = (Outer#tx.data)#{
                                    <<"Message">> => maps:get(<<"Message">>, Msg#tx.data)
                                }
                            }
                        ]
                    }};
                {false, Reason} -> return_error(S, Reason)
            end
    end;
execute(_M, S = #{ pass := 3, results := _Results }, _Opts) ->
    {ok, S};
execute(_M, S, _Opts) ->
    {ok, S}.

validate_stage(1, Msg, Opts) when is_record(Msg, tx) ->
    validate_stage(1, Msg#tx.data, Opts);
validate_stage(1, #{ <<"Attestations">> := Attestations, <<"Message">> := Content }, Opts) ->
    validate_stage(2, Attestations, Content, Opts);
validate_stage(1, _M, _Opts) -> {false, <<"Required PoDA messages missing">>}.

validate_stage(2, #tx { data = Attestations }, Content, Opts) ->
    validate_stage(2, Attestations, Content, Opts);
validate_stage(2, Attestations, Content, Opts) ->
    ?c({poda_stage, 2}),
    % Ensure that all attestations are valid and signed by a
    % trusted authority.
    case lists:all(fun({_, Att}) -> ar_bundles:verify_item(Att) end, maps:to_list(Attestations)) of
        true -> validate_stage(3, Content, Attestations, Opts);
        false -> {false, <<"Invalid attestations">>}
    end;

validate_stage(3, Content, Attestations, Opts = #{ quorum := Quorum }) ->
    ?c({poda_stage, 3}),
    Validations =
        lists:filter(
            fun({_, Att}) -> validate_attestation(Content, Att, Opts) end,
            maps:to_list(Attestations)
        ),
    ?c({poda_validations, length(Validations)}),
    case length(Validations) >= Quorum of
        true ->
            ?c({poda_quorum_reached, length(Validations)}),
            true;
        false -> {false, <<"Not enough validations">>}
    end.

validate_attestation(Msg, Att, Opts) ->
    MsgID = ar_util:encode(ar_bundles:id(Msg, unsigned)),
    AttSigner = ar_util:encode(ar_bundles:signer(Att)),
    ?no_prod(use_real_authority_validation),
    % ValidSigner = lists:member(
    %     ar_bundles:signer(Att),
    %     maps:get(authorities, Opts)
    % ),
    ValidSigner = true,
    ValidSignature = ar_bundles:verify_item(Att),
    RelevantMsg = ar_bundles:id(Att, unsigned) == MsgID orelse
        lists:keyfind(<<"Attestation-For">>, 1, Att#tx.tags)
            == {<<"Attestation-For">>, MsgID},
    ValidSigner and ValidSignature and RelevantMsg.

return_error(S = #{ wallet := Wallet }, Reason) ->
    ?c({poda_return_error, Reason}),
    {skip, S#{
        results => #{
            <<"/Outbox">> =>
                ar_bundles:sign_item(
                    #tx{
                        data = Reason,
                        tags = [{<<"Error">>, <<"PoDA">>}]
                    },
                    Wallet
                )
        }
    }}.

is_user_signed(#tx { data = #{ <<"Message">> := Msg } }) ->
    ?no_prod(use_real_attestation_detection),
    lists:keyfind(<<"From-Process">>, 1, Msg#tx.tags) == false;
is_user_signed(_) -> true.

push(_Item, S = #{ results := Results }) ->
    %?c({poda_push, Results}),
    NewRes = attest_to_results(Results, S),
    {ok, S#{ results => NewRes }}.

attest_to_results(Msg, S = #{ wallet := Wallet }) ->
    case is_map(Msg#tx.data) of
        true ->
            % Add attestations to the outbox and spawn items.
            maps:map(
                fun(Key, IndexMsg) ->
                    case lists:member(Key, [<<"/Outbox">>, <<"/Spawn">>]) of
                        true ->
                            ?c({poda_attest_to_results, Key}),
                            maps:map(
                                fun(_, DeepMsg) -> add_attestations(DeepMsg, S) end,
                                IndexMsg#tx.data
                            );
                        false -> IndexMsg
                    end
                end,
                Msg#tx.data
            );
        false -> Msg
    end.

add_attestations(NewMsg, S = #{ store := _Store, logger := _Logger, wallet := Wallet }) ->
    ?no_prod("PoDA currently waits for 1 second before getting attestations!"),
    Process = find_process(NewMsg, S),
    receive after 1000 -> ok end,
    case is_record(Process, tx) andalso lists:member({<<"Device">>, <<"PODA">>}, Process#tx.tags) of
        true ->
            #{ authorities := InitAuthorities, quorum := Quorum } =
                extract_opts(Process#tx.tags),
            ?c({poda_push, InitAuthorities, Quorum}),
            % Aggregate validations from other nodes.
            % TODO: Filter out attestations from the current node.
            Attestations = pfiltermap(
                fun(Address) ->
                    case ao_router:find(compute, Process#tx.id, Address) of
                        {ok, ComputeNode} ->
                            ?c({poda_asking_peer_for_attestation, ComputeNode}),
                            % TODO: Use the slot number.
                            ?no_prod("Get attestation on correct slot."),
                            {ok, ComputeNode} =
                                ao_router:find(compute, Process#tx.id, Address),
                            case ao_client:compute(ComputeNode, Process#tx.id, 0) of
                                {ok, Att} -> {true, Att};
                                _ -> false
                            end;
                        _ -> false
                    end
                end,
                InitAuthorities
            ),
            MsgID = ar_util:encode(ar_bundles:id(NewMsg, unsigned)),
            LocalAttestation = ar_bundles:sign_item(
                #tx{ tags = [{<<"Attestation-For">>, MsgID}], data = <<>> },
                Wallet
            ),
            CompleteAttestations =
                ar_bundles:sign_item(
                    ar_bundles:normalize(
                        #tx {
                            data = 
                                maps:from_list(
                                    lists:zipwith(
                                        fun(Index, Att) -> {integer_to_binary(Index), Att} end,
                                        lists:seq(1, length([LocalAttestation | Attestations])),
                                        [LocalAttestation | Attestations]
                                    )
                                )
                        }
                    ),
                    Wallet
                ),
            AttestationBundle = ar_bundles:sign_item(
                ar_bundles:normalize(
                    #tx{
                        target = NewMsg#tx.target,
                        data = #{
                            <<"Attestations">> => CompleteAttestations,
                            <<"Message">> => NewMsg
                        }
                    }
                ),
                Wallet
            ),
            AttestationBundle;
        false -> NewMsg
    end.

%% @doc Execute a predicate in parallel and filter the results.
pfiltermap(Pred, List) ->
    Parent = self(),
    Pids = lists:map(fun(X) -> 
        spawn_monitor(fun() -> 
            Result = {X, Pred(X)},
            Parent ! {self(), Result}
        end)
    end, List),
    lists:filtermap(fun({Ref, Pid}) ->
        receive
            {Pid, Result} -> Result;
            % Handle crashes as filterable events
            {'DOWN', Ref, process, Pid, _Reason} -> false
        end
    end, Pids).

find_process(Item, #{ logger := _Logger, store := Store }) ->
    case Item#tx.target of
        X when X =/= <<>> ->
            ?c({poda_find_process, ar_util:id(Item#tx.target)}),
            ao_cache:read(Store, ar_util:id(Item#tx.target));
        _ ->
            case lists:keyfind(<<"Type">>, 1, Item#tx.tags) of
                {<<"Type">>, <<"Process">>} -> Item;
                _ -> process_not_specified
            end
    end.

retry(Fun, Delay, Attempts) -> retry(Fun, Delay, Attempts, no_error).
retry(_Fun, _Delay, 0, LastError) -> LastError;
retry(Fun, Delay, Attempts, _LastError) ->
    try Fun() of
        Result -> Result
    catch A:B:C ->
        ?c({"Error: ", A, ":", B, ":", C, ". Retrying in ", Delay, "ms"}),
        receive after Delay -> ok end,
        retry(Fun, Delay, Attempts - 1, {A, B, C})
    end.