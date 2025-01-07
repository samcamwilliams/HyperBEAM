%%% @doc A device that relays messages to their `Target` destination,
%%% optionally waiting for a resulting message upon which further
%%% computation can be performed.
%%% 
%%% This device is typically employed in two modes:
%%% 
%%% 1. Invoked in order to force calculation of a hashpath, and to
%%% `push` sub-messages from the result to a further hashpath (for 
%%% another computation), or to any traditional HTTP/1.1-3 URL.
%%% 2. Added to a stack of devices, resulting in a 'real-time' `push`
%%% of messages as a result of a computation.
%%% 
%%% The `Messenger` device honors the following paramaters:
%%% ```
%%%     Recursive:  Determines whether the device should seek to evaluate
%%%                 pushed messages, and push messages that result from 
%%%                 those. This mode allows interactions that require many
%%%                 interactions with paralell computations to be
%%%                 orchestrated on behalf of a user. Default: True.
%%% 
%%%     Monitor:    The device should periodically execute a hashpath, every
%%%                 `Value` time period, pushing the resulting messages as
%%%                 in other modes. Default: Not set.
%%% 
%%%    Allow-URLs:  Whether messages for which the `Target` is a centralized
%%%                 web location (a fully-qualified IP/FDQN address) should be
%%%                 honored (`True`) or ignored (`False`) by the messenger. 
%%%                 If set to `True`, the device will relay messages as given
%%%                 in the `Base` to the URL specified by `Target`, adding the
%%%                 response message received from the remote HTTP server to
%%%                 its own output message.
%%% Messenger-Keys: A list of the keys for which the messenger device should
%%%                 be active. Default: [<<"Push">>].
%%%         Report: Determines whether the results of the message pushing 
%%%                 should be reported in the resulting output, and if set,
%%%                 to which subpath.
%%% '''
%%% Each parameter is expected to be located in the `BaseMessage`.
-module(dev_messenger).
-export([info/1]).
-include("include/hb.hrl").

info(Msg) ->
    maps:merge(
        #{
            handler => fun relay/4,
            excludes => [set, keys]
        },
        case maps:get(<<"Messenger-Keys">>, Msg, not_found) of
            not_found -> #{};
            StackKeys -> #{ exports => StackKeys }
        end
    ).

%% @doc Search for messages to `push` in the base message.
relay(keys, M1, _, _) -> dev_message:keys(M1);
relay(_, M1, M2, Opts) ->
    Prefix =
        hb_converge:get(
            <<"Input-Prefix">>,
            {as, dev_message, M1},
            <<>>,
            Opts
        ),
    ShouldReport =
        hb_converge:get(<< Prefix/binary, "/Report">>, M1, false, Opts),
    Outbox =
        hb_converge:get(<< Prefix/binary, "/Outbox">>, M1, [], Opts),
    case ShouldReport of
        false ->
            % We do not need to report our progress, so we can spawn a new
            % Erlang process to handle pushing for us.
            spawn(fun() -> dispatch(M1, Outbox, Opts) end),
            {ok, M1};
        Subpath ->
            % Synchronously push messages, taking the returned value and
            % placing it into the BaseMsg at `Subpath`.
            hb_converge:set(
                M1,
                #{ Subpath => dispatch(M1, Outbox, Opts) },
                Opts
            )
    end.

dispatch(M1, Outbox, Opts) ->
    Hashpath = hb_path:from_message(hashpath, M1),
    maps:map(
        fun(Key, MsgToPush) ->
            case hb_converge:get(<<"Target">>, MsgToPush, Opts) of
                not_found ->
                    ?event({skip_no_target, {key, Key}, MsgToPush}),
                    {ok, <<"No Target. Did not push.">>};
                Target ->
                    ?event(
                        {pushing_child,
                            {originates_from, Hashpath},
                            {outbox_key, Key}
                        }
                    ),
                    {ok, NextSlotOnProc} = hb_converge:resolve(
                        M1,
                        #{
                            method => <<"POST">>,
                            path => <<"Schedule/Slot">>,
                            <<"Message">> =>
                                PushedMsg = hb_message:sign(
                                    MsgToPush,
                                    hb_opts:get(wallet, Opts)
                                )
                        },
                        Opts
                    ),
                    PushedMsgID = hb_converge:get(<<"id">>, PushedMsg, Opts),
                    ?event(
                        {push_scheduled,
                            {assigned_slot, NextSlotOnProc},
                            {target, Target}
                        }),
                    {ok, Downstream} = hb_converge:resolve(
                        M1,
                        #{ path => <<"Push">>, <<"Slot">> => NextSlotOnProc },
                        Opts
                    ),
                    #{
                        <<"id">> => PushedMsgID,
                        <<"Target">> => Target,
                        <<"Slot">> => NextSlotOnProc,
                        <<"Resulted-In">> => Downstream
                    }
            end
        end,
        hb_converge:ensure_message(Outbox)
    ).