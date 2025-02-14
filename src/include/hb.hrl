-include("include/ar.hrl").

%% @doc Macro for checking if a message is empty, ignoring its hashpath.
-define(IS_EMPTY_MESSAGE(Msg), (map_size(Msg) == 0) orelse (map_size(Msg) == 1 andalso (is_map_key(priv, Msg) orelse is_map_key(<<"priv">>, Msg)))).
%% @doc Macro usable in guards that validates whether a term is a
%% human-readable ID encoding.
-define(IS_ID(X), (is_binary(X) andalso (byte_size(X) == 43 orelse byte_size(X) == 32))).
%% @doc List of special keys that are used in the Converge Protocol.
-define(CONVERGE_KEYS, [<<"path">>, <<"hashpath">>, <<"priv">>]).
%% @doc Keys that can be regenerated losslessly.
-define(REGEN_KEYS, [<<"unsigned_id">>, <<"content-digest">>]).

%% @doc Record used for parsing relevant components of a cursor-browsable
%% response.
-record(result, {
    messages = [],
    assignments = [],
    spawns = [],
    output = [],
    cursor = undefined
}).

%%% Functional macros that pass the current module and line number to the
%%% underlying function.
-define(event(X), hb:event(?MODULE, X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(event(Topic, X), hb:event(Topic, X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(event(Topic, X, Opts), hb:event(maps:get(topic, Opts, Topic), X, ?MODULE, ?FUNCTION_NAME, ?LINE), Opts).
-define(debug_wait(T), hb:debug_wait(T, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(no_prod(X), hb:no_prod(X, ?MODULE, ?LINE)).

%%% Macro shortcuts for debugging.
%% @doc A macro for marking that you got 'here'.
-define(h(), hb:event("[Debug point reached.]", ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Quickly print a value in the logs. Currently uses the event
%% function, but should be moved to a debug-specific function once we
%% build out better logging infrastructure.
-define(p(X), hb:event(X, ?MODULE, ?FUNCTION_NAME, ?LINE)).
%% @doc Print the trace of the current stack, up to the first non-hyperbeam
%% module.
-define(trace(), hb_util:trace_macro_helper(fun hb_util:print_trace/4, catch error(test), ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(trace_short(), hb_util:trace_macro_helper(fun hb_util:print_trace_short/4, catch error(test), ?MODULE, ?FUNCTION_NAME, ?LINE)).