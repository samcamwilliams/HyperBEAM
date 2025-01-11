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
    case ShouldReport of
        false ->
            % We do not need to report our progress, so we can spawn a new
            % Erlang process to handle pushing for us.
            spawn(fun() -> dispatch(M1, Opts) end),
            {ok, M1};
        Subpath ->
            % Synchronously push messages, taking the returned value and
            % placing it into the BaseMsg at `Subpath`.
            hb_converge:set(
                M1,
                #{ Subpath => dispatch(M1, Opts) },
                Opts
            )
    end.

dispatch(Msg1, Msg2, Opts) ->
    case hb_converge:get(<<"Target">>, Msg, Opts) of
        not_found ->
            ?event({uploading_to_arweave_as_no_target_set, Msg}),
            hb_client:upload(Msg);
        Target = << "http", _/binary >> ->
            ?event({posting_to_http, Target, Msg}),
            M1Allowed = hb_converge:get(<<"Allow-URLs">>, Msg1, Opts),
            OptsAllowed = hb_opts:get(allow_urls, false, Opts),
            case {M1Allowed, OptsAllowed} of
                {A1, A2} when (not A1) or (A2 == false) ->
                    throw(settings_disallow_http_post);
                {_, }
                
            hb_http:post(Target, Msg);
        Target ->
            ?event(
                {posting_to_converge,
                    {originates_from, hb_path:from_message(hashpath, Msg)},
                    {outbox_key, hb_util:id(Msg, unsigned)}
                }
            ),
            {ok, Downstream} = hb_converge:resolve(
                Msg,
                #{ path => <<"Push">> },
                Opts
            ),
            #{
                <<"Target">> => Target,
                <<"Resulted-In">> => Downstream
            }
    end.