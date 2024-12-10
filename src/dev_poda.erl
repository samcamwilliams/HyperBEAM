-module(dev_poda).
-export([init/2, execute/3]).
-export([is_user_signed/1]).
-export([push/2]).
-include("include/hb.hrl").
-hb_debug(print).

%%% A simple exemplar decentralized proof of authority consensus algorithm
%%% for AO processes. This device is split into two flows, spanning three
%%% actions.
%%% 
%%% Execution flow:
%%% 1. Initialization.
%%% 2. Validation of incoming messages before execution.
%%% Attestation flow:
%%% 1. Adding attestations to results, either on a CU or MU.

%%% Execution flow: Initialization.

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
    ?event({poda_authorities, Authorities}),
    #{
        authorities => Authorities,
        quorum => Quorum
    }.

%%% Execution flow: Pre-execution validation.

execute(Outer = #tx { data = #{ <<"Message">> := Msg } }, S = #{ pass := 1 }, Opts) ->
    case is_user_signed(Msg) of
        true ->
            {ok, S};
        false ->
            % For now, the message itself will be at `/Message/Message`.
            case validate(Msg, Opts) of
                true ->
                    ?event({poda_validated, ok}),
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
                                Encoded = hb_util:encode(Id),
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

validate(Msg, Opts) ->
    validate_stage(1, Msg, Opts).

validate_stage(1, Msg, Opts) when is_record(Msg, tx) ->
    validate_stage(1, Msg#tx.data, Opts);
validate_stage(1, #{ <<"Attestations">> := Attestations, <<"Message">> := Content }, Opts) ->
    validate_stage(2, Attestations, Content, Opts);
validate_stage(1, _M, _Opts) -> {false, <<"Required PoDA messages missing">>}.

validate_stage(2, #tx { data = Attestations }, Content, Opts) ->
    validate_stage(2, Attestations, Content, Opts);
validate_stage(2, Attestations, Content, Opts) ->
    % Ensure that all attestations are valid and signed by a
    % trusted authority.
    case
        lists:all(
            fun({_, Att}) ->
                ar_bundles:verify_item(Att)
            end,
            maps:to_list(Attestations)
        ) of
        true -> validate_stage(3, Content, Attestations, Opts);
        false -> {false, <<"Invalid attestations">>}
    end;

validate_stage(3, Content, Attestations, Opts = #{ quorum := Quorum }) ->
    Validations =
        lists:filter(
            fun({_, Att}) -> validate_attestation(Content, Att, Opts) end,
            maps:to_list(Attestations)
        ),
    ?event({poda_validations, length(Validations)}),
    case length(Validations) >= Quorum of
        true ->
            ?event({poda_quorum_reached, length(Validations)}),
            true;
        false -> {false, <<"Not enough validations">>}
    end.

validate_attestation(Msg, Att, Opts) ->
    MsgID = hb_util:encode(ar_bundles:id(Msg, unsigned)),
    AttSigner = hb_util:encode(ar_bundles:signer(Att)),
    ?event({poda_attestation, {signer, AttSigner, maps:get(authorities, Opts)}, {msg_id, MsgID}}),
    ValidSigner = lists:member(AttSigner, maps:get(authorities, Opts)),
    ?no_prod(use_real_signature_verification),
    ValidSignature = ar_bundles:verify_item(Att),
    RelevantMsg = ar_bundles:id(Att, unsigned) == MsgID orelse
        (lists:keyfind(<<"Attestation-For">>, 1, Att#tx.tags)
            == {<<"Attestation-For">>, MsgID}) orelse
        ar_bundles:member(ar_bundles:id(Msg, unsigned), Att),
    case ValidSigner and ValidSignature and RelevantMsg of
        false ->
            ?event({poda_attestation_invalid,
                    {attestation, ar_bundles:id(Att, signed)},
                    {signer, AttSigner},
                    {valid_signer, ValidSigner},
                    {valid_signature, ValidSignature},
                    {relevant_msg, RelevantMsg}}
            ),
            false;
        true -> true
    end.

%%% Execution flow: Error handling.
%%% Skip execution of this message, instead returning an error message.
return_error(S = #{ wallet := Wallet }, Reason) ->
    ?event({poda_return_error, Reason}),
    ?debug_wait(10000),
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

%%% Attestation flow: Adding attestations to results.

%% @doc Hook used by the MU pathway (currently) to add attestations to an
%% outbound message if the computation requests it.
push(_Item, S = #{ results := ResultsMsg }) ->
    NewRes = attest_to_results(ResultsMsg, S),
    {ok, S#{ results => NewRes }}.

attest_to_results(Msg, S) ->
    case is_map(Msg#tx.data) of
        true ->
            % Add attestations to the outbox and spawn items.
            maps:map(
                fun(Key, IndexMsg) ->
                    ?no_prod("Currently we only attest to the outbox and spawn items."
                        "Make it general?"),
                    case lists:member(Key, [<<"/Outbox">>, <<"/Spawn">>]) of
                        true ->
                            ?event({poda_starting_to_attest_to_result, Key}),
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

add_attestations(NewMsg, S = #{ assignment := Assignment, store := _Store, logger := _Logger, wallet := Wallet }) ->
    Process = find_process(NewMsg, S),
    case is_record(Process, tx) andalso lists:member({<<"Device">>, <<"PODA">>}, Process#tx.tags) of
        true ->
            #{ authorities := InitAuthorities, quorum := Quorum } =
                extract_opts(Process#tx.tags),
            ?event({poda_push, InitAuthorities, Quorum}),
            % Aggregate validations from other nodes.
            % TODO: Filter out attestations from the current node.
            MsgID = hb_util:encode(ar_bundles:id(NewMsg, unsigned)),
            ?event({poda_add_attestations_from, InitAuthorities, {self,hb:address()}}),
            Attestations = pfiltermap(
                fun(Address) ->
                    case hb_router:find(compute, ar_bundles:id(Process, unsigned), Address) of
                        {ok, ComputeNode} ->
                            ?event({poda_asking_peer_for_attestation, ComputeNode, <<"Attest-To">>, MsgID}),
                            Res = hb_client:compute(
                                ComputeNode,
                                ar_bundles:id(Process, signed),
                                ar_bundles:id(Assignment, signed),
                                #{ <<"Attest-To">> => MsgID }
                            ),
                            case Res of
                                {ok, Att} ->
                                    ?event({poda_got_attestation_from_peer, ComputeNode}),
                                    {true, Att};
                                _ -> false
                            end;
                        _ -> false
                    end
                end,
                ?event(InitAuthorities -- [hb:address()])
            ),
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
                                        AttList = [LocalAttestation | Attestations]
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
            ?event({poda_attestation_bundle_signed, {attestations, length(AttList)}}),
            AttestationBundle;
        false -> NewMsg
    end.

%% @doc Helper function for parallel execution of attestation
%% gathering.
pfiltermap(Pred, List) ->
    Parent = self(),
    Pids = lists:map(fun(X) -> 
        spawn_monitor(fun() -> 
            Result = {X, Pred(X)},
            ?event({pfiltermap, sending_result, self()}),
            Parent ! {self(), Result}
        end)
    end, List),
    ?event({pfiltermap, waiting_for_results, Pids}),
    [
        Res
    ||
        {true, Res} <-
            lists:map(fun({Pid, Ref}) ->
                receive
                    {Pid, {_Item, Result}} ->
                        ?event({pfiltermap, received_result, Pid}),
                        Result;
                    % Handle crashes as filterable events
                    {'DOWN', Ref, process, Pid, _Reason} ->
                        ?event({pfiltermap, crashed, Pid}),
                        false;
                    Other ->
                        ?event({pfiltermap, unexpected_message, Other}),
                        false
                end
            end, Pids)
    ].

%% @doc Find the process that this message is targeting, in order to
%% determine which attestations to add.
find_process(Item, #{ logger := _Logger, store := Store }) ->
    case Item#tx.target of
        X when X =/= <<>> ->
            ?event({poda_find_process, hb_util:id(Item#tx.target)}),
            {ok, Proc} = hb_cache:read_message(Store, hb_util:id(Item#tx.target)),
            Proc;
        _ ->
            case lists:keyfind(<<"Type">>, 1, Item#tx.tags) of
                {<<"Type">>, <<"Process">>} -> Item;
                _ -> process_not_specified
            end
    end.