%%% @doc An OTP-compatible `logger_formatter' implementation that proxies Erlang's
%%% requests for console logging to `hb_format' functions, utilizing HyperBEAM's
%%% custom formatting.
-module(hb_format_logger).
-behaviour(logger_formatter).
-export([format/2, check_config/1]).

-spec format(logger:format_spec(), logger:log_event()) -> iodata().
format(LogEvent, _Config) ->
    hb_format:message(LogEvent).

check_config(Config) ->
    {ok, Config}.