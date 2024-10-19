-module(dev_poda).
-export([init/2, execute/3]).
-include("include/ao.hrl").
-ao_debug(print).

%%% A simple exemplar decentralized proof of authority consensus algorithm
%%% for AO processes.

init(S, Params) ->
    ?c({poda_init, Params}),
    Authorities =
        lists:filtermap(
            fun({<<"Authority">>, Addr}) -> {true, Addr};
               (_) -> false end,
               Params
        ),
    {_, RawQuorum} = lists:keyfind(<<"Quorum">>, 1, Params),
    Quorum = binary_to_integer(RawQuorum),
    ?c({poda_authorities, Authorities, quorum, Quorum}),
    {ok, S, #{ authorities => Authorities, quorum => Quorum }}.

%% TODO: Rather than throwing, we will probably want to skip the message completely.
execute(M, S = #{ pass := 1 }, Opts) ->
    case isUserSigned(M) of
        true ->
            ?c({poda_skipped, user_signed_msg}),
            {ok, S};
        false ->
            ?c({poda_execute, Opts}),
            % For now, the message itself will be at `/Message/Message`. It is the message
            % body of the message that is attached to the assignment we are evaluating.
            Msg = maps:get(<<"Message">>, M#tx.data),
            case maps:get(<<"Message">>, Msg#tx.data, undefined) of
                undefined -> throw(content_missing);
                _ ->
                    case maps:get(<<"Attestations">>, Msg) of
                        undefined -> throw(attestations_missing);
                        Attestations ->
                            case validate_message(Msg, Opts) of
                                true ->
                                    ?c({poda_validated, Opts}),
                                    % Add the validations to the VFS.
                                    VFS1 = lists:foldl(
                                        fun(Attestation, Acc) ->
                                            Id = ar_bundles:signer(Attestation),
                                            Encoded = ar_util:encode(Id),
                                            maps:put(
                                                <<"/Attestations/", Encoded/binary>>,
                                                Attestation,
                                                Acc
                                            )
                                        end,
                                        maps:get(vfs, S, #{}),
                                        Attestations
                                    ),
                                    {ok, S#{ vfs => VFS1 }};
                            {false, Reason} -> throw({poda_validation_failed, Reason})
                            end
                    end
            end
    end;
execute(_M, S = #{ pass := 3, results := Results }, _Opts) ->
    ?c({poda_called_on_results, Results}),
    {ok, S};
execute(_M, S, _Opts) ->
    {ok, S}.

validate_message(Msg, #{ authorities := Authorities, quorum := Quorum }) ->
    case maps:get(<<"Message">>, Msg#tx.data, undefined) of
        undefined -> {false, <<"Content missing">>};
        Content ->
            case maps:get(<<"Attestations">>, Msg) of
                undefined -> {false, <<"Attestations missing">>};
                Attestations ->
                    ?c({poda_execute, Content, Attestations}),
                    % Ensure that all attestations are valid and signed by a
                    % trusted authority.
                    true = lists:all(fun ar_bundles:verify_item/1, Attestations),
                        Validations =
                            lists:filter(
                                fun(Att) ->
                                    validate_attestation(Msg, Att, Opts)
                                end,
                                Attestations
                            ),
                    case length(Validations) >= Quorum of
                        true -> true;
                        false -> {false, <<"Not enough validations">>}
                    end
            end
    end.

validate_attestation(MsgId, Att, Opts) ->
    lists:member(
        ar_bundles:signer(Att),
        maps:get(authorities, Opts)
    ) andalso
    ar_bundles:verify_item(Att).

isUserSigned(M) ->
    lists:keyfind(<<"From-Process">>, 1, M#tx.tags) == false.