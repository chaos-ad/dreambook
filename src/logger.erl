%% Author: chaos
%% Created: 14.03.2011
%% Description: TODO: Add description to logger
-module(logger).

%%
%% Include files
%%

-include("logger.hrl").

%%
%% Exported Functions
%%
-export([format/5, start/0, start/1, stop/0]).

%%
%% API Functions
%%

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
                ok = log4erl_error_logger:add_handler()
            catch
                _:{badmatch, {error, enoent}} ->
                    erlang:error(config_not_found)
            end;
        {error, {already_started, log4erl}} ->
            ok
    end.

stop() ->
    application:stop(log4erl).