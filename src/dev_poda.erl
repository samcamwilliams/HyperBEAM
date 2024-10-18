-module(dev_poda).
-export([init/2, execute/2]).
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
    {ok, S#{ authorities => Authorities, quorum => Quorum }}.

%% TODO: Rather than throwing, we will probably want to skip the message completely.
execute(M, S = #{ authorities := Authorities, quorum := Quorum, pass := 1 }) ->
    ?c({poda_execute, Authorities, Quorum}),
    Msg = maps:get(<<"Message">>, M#tx.data),
    case maps:get(<<"Content">>, Msg#tx.data, undefined) of
        undefined -> throw(content_missing);
        Content ->
            case maps:get(<<"Attestations">>, Msg) of
                undefined -> throw(attestations_missing);
                Attestations ->
                    ?c({poda_execute, Content, Attestations}),
                    % Ensure that all attestations are valid and signed by a
                    % trusted authority.
                    true = lists:all(fun ar_bundles:verify_item/1, Attestations),
                    Validations =
                        lists:filter(
                            fun(Attestation) ->
                                From = crypto:hash(sha256, Attestation#tx.owner),
                                lists:member(ar_util:id(From), Authorities)
                        end,
                        Attestations
                    ),
                    case length(Validations) >= Quorum of
                        true ->
                            % Add the validations to the VFS.
                            VFS0 = maps:get(vfs, S, #{}),
                            VFS1 = lists:foldl(
                                fun(Attestation, Acc) ->
                                    maps:put(
                                        ar_util:id(Attestation#tx.owner),
                                        Attestation,
                                        Acc
                                    )
                                end,
                                VFS0,
                                Validations
                            ),
                            {ok, S#{ results => Validations, vfs => VFS1 }};
                        false ->
                            throw(not_enough_validations)
                    end
            end
    end,
    {ok, S};
execute(M, S = #{ authorities := Authorities, quorum := Quorum, results := Results, pass := 2 }) ->
    ?c({poda_called_on_results, Results}),
    {ok, S}.
