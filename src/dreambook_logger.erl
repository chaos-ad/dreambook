-module(dreambook_logger).

-include("logger.hrl").

-export([format/5, start/0, start/1, stop/0]).

format(Module, Lvl, Process, Pattern, Args) ->
	Message = lists:flatten(io_lib:format(Pattern, Args)),
    %ok = ensure_started(),
	log4erl:log(Lvl, "[~p][~p]~s", [Process, Module, Message]).

start() ->
    start(?DEFAULT_LOGGER_CONFIG).

start(Filename) ->
    case application:start(log4erl) of
        ok ->
            try
                ok = log4erl:conf(Filename),
                ok = log4erl:error_logger_handler()
            catch
                _:{badmatch, {error, enoent}} ->
                    erlang:error(config_not_found)
            end;
        {error, {already_started, log4erl}} ->
            ok
    end.

stop() ->
    application:stop(log4erl).