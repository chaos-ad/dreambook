
-module(dreambook_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

get_env(Name) ->
    case application:get_env(Name) of
        {ok, Options} -> Options;
        undefined     -> []
    end.

init([]) ->
    {ok, { {one_for_one, 5, 10},
        [
            {dreambook_db_server,
                {dreambook_db_server, start_link, [get_env(db_server)]},
                permanent, 5000, worker, [dreambook_db_server]
            },
            {dreambook_json_server,
                {dreambook_json_server, start_link, [get_env(json_server)]},
                permanent, 5000, worker, [dreambook_json_server]
            }
        ]
    } }.
