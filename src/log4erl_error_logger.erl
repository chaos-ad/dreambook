-module(log4erl_error_logger).
-behaviour(gen_event).


%% gen_event callbacks
-export([
		 init/1,
		 handle_event/2,
		 handle_call/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		]).

-export([
		 add_handler/0
		]).

%%======================================
%% gen_event callback functions
%%======================================
init([])->
	{ok, state}.

add_handler() ->
    error_logger:add_report_handler(?MODULE,[]).

handle_event({Type, _GLeader, {Pid, Msg, Data}}, State) ->
	case get_level(Type) of
		{ok, Lvl} ->
			log4erl:log(Lvl,  "[~p] ~p ~p: ~p", [Pid, Type, Msg, Data]);
		{error} ->
			ok
	end,
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_level(error) -> {ok, error};
get_level(error_report) -> {ok, error};
get_level(warning_msg) -> {ok, warn};
get_level(warning_report) -> {ok, warn};
get_level(info_msg) -> {ok, info};
get_level(info_report) -> {ok, info};
get_level(_) -> {error}.
	
