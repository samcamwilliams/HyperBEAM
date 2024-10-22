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
    #{ authorities => Authorities, quorum => Quorum }.

execute(M, S = #{ pass := 1 }, Opts) ->
    case is_user_signed(M) of
        true ->
            {ok, S};
        false ->
            ?c({poda_execute, Opts}),
            % For now, the message itself will be at `/Message/Message`. It is the message
            % body of the message that is attached to the assignment we are evaluating.
            Msg = maps:get(<<"Message">>, M#tx.data),
            case maps:get(<<"Message">>, Msg#tx.data, undefined) of
                undefined -> return_error(S, <<"Content missing">>);
                _ ->
                    case maps:get(<<"Attestations">>, Msg) of
                        undefined ->
                            return_error(S, <<"Attestations missing">>);
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
                            {false, Reason} -> return_error(S, Reason)
                            end
                    end
            end
    end;
execute(_M, S = #{ pass := 3, results := Results }, _Opts) ->
    {ok, S};
execute(_M, S, _Opts) ->
    {ok, S}.

validate_message(Msg, Opts = #{ quorum := Quorum }) ->
    case maps:get(<<"Message">>, Msg#tx.data, undefined) of
        undefined -> {false, <<"Content missing">>};
        Content ->
            case maps:get(<<"Attestations">>, Msg) of
                undefined -> {false, <<"Attestations missing">>};
                Attestations ->
                    ?c({poda_execute, Content, Attestations}),
                    % Ensure that all attestations are valid and signed by a
                    % trusted authority.
                    true =
                        lists:all(
                            fun ar_bundles:verify_item/1,
                            Attestations
                        ),
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

validate_attestation(Msg, Att, Opts) ->
    MsgID = ar_bundles:id(Msg, unsigned),
    ValidSigner = lists:member(
        ar_bundles:signer(Att),
        maps:get(authorities, Opts)
    ),
    ValidSignature = ar_bundles:verify_item(Att),
    RelevantMsg = ar_bundles:id(Att, unsigned) == MsgID orelse
        lists:keyfind(<<"Attestation-For">>, 1, Att#tx.tags)
            == {<<"Attestation-For">>, MsgID},
    ValidSigner and ValidSignature and RelevantMsg.

return_error(S = #{ wallet := Wallet }, Reason) ->
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

is_user_signed(M) when is_map(M#tx.data) ->
    case lists:keyfind(<<"From-Process">>, 1, M#tx.tags) of
        false -> true;
        _ -> false
    end;
is_user_signed(_) -> false.

push(_Item, S = #{ results := Results }) ->
    {ok, S#{
        results =>
            maps:map(
                fun(_, NewMsg) -> add_attestations(NewMsg, S) end,
                Results#tx.data
            )
    }}.

add_attestations(NewMsg, S = #{ store := _Store, logger := _Logger, wallet := Wallet }) ->
    Process = find_process(NewMsg, S),
    case is_record(Process, tx) andalso lists:member({<<"Device">>, <<"PODA">>}, Process#tx.tags) of
        true ->
            #{ authorities := InitAuthorities, quorum := Quorum } =
                extract_opts(Process#tx.tags),
            ?c({poda_push, InitAuthorities, Quorum}),
            % Aggregate validations from other nodes.
            % TODO: Filter out attestations from the current node.
            Attestations = lists:filtermap(
                fun(Address) ->
                    case ao_router:find(compute, Process#tx.id, Address) of
                        {ok, Att} -> Att;
                        _ -> false
                    end
                end,
                InitAuthorities
            ),
            ar_bundles:sign_item(
                #tx{
                    data = #{
                        <<"Attestations">> => Attestations,
                        <<"Message">> => NewMsg
                    }
                },
                Wallet
            );
        false -> NewMsg
    end.

find_process(Item, #{ logger := _Logger, store := Store }) ->
    case Item#tx.target of
        X when X =/= <<>> ->
            ao_store:read(Store, Item#tx.target);
        _ ->
            case lists:keyfind(<<"Type">>, 1, Item#tx.tags) of
                {<<"Type">>, <<"Process">>} -> Item;
                _ -> process_not_specified
            end
    end.