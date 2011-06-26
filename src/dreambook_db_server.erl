-module(dreambook_db_server).
-behavior(gen_server).
-compile(export_all).

-include("logger.hrl").
-include_lib("emysql/include/emysql.hrl").

-export([start/0, start/1, stop/1, stop/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% -export([find_meaning/1, find_keywords/1, get_balance/1, set_balance/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start()             -> start([]).
start(Options)      -> gen_server:start( {local, ?MODULE}, ?MODULE, Options, [] ).
stop(Pid)           -> stop(Pid, shutdown).
stop(Pid, Reason)   -> gen_server:call(Pid, {shutdown, Reason}, infinity).

find_books(Keyword)          -> gen_server:call(?MODULE, {find_books, Keyword}).
find_meaning(Keyword)        -> gen_server:call(?MODULE, {find_meaning, Keyword}).
find_keywords(Keyword)       -> gen_server:call(?MODULE, {find_keywords, Keyword}).

add_user(UserID, Balance)    -> gen_server:call(?MODULE, {add_user, UserID, Balance}).

in_history(UserID, Keyword)  -> gen_server:call(?MODULE, {in_history, UserID, Keyword}).
add_history(UserID, Keyword) -> gen_server:call(?MODULE, {add_history, UserID, Keyword}).
get_history(UserID)          -> gen_server:call(?MODULE, {get_history, UserID}).

get_balance(UserID)          -> gen_server:call(?MODULE, {get_balance, UserID}).
add_balance(UserID, Delta)   -> gen_server:call(?MODULE, {add_balance, UserID, Delta}).
del_balance(UserID, Delta)   -> gen_server:call(?MODULE, {del_balance, UserID, Delta}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Options) ->
    process_flag(trap_exit, true),

    Port = proplists:get_value(port, Options, 3306),
    Host = proplists:get_value(host, Options, "localhost"),
    User = proplists:get_value(user, Options, "sonnik"),
    Pass = proplists:get_value(pass, Options, "sonnik"),
    Name = proplists:get_value(name, Options, "sonnik"),

    LoggerConfig = proplists:get_value(logconf, Options, "logger.conf"),
    application:start(log4erl),
    ok = log4erl:conf(LoggerConfig),
    ok = log4erl:error_logger_handler(),

    application:start(crypto),
    application:start(emysql),
    ok = emysql:add_pool(database, 1, User, Pass, Host, Port, Name, utf8),

    {ok, nil}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({find_books, Keyword}, _, State) ->
    Key = emysql_util:quote(Keyword),
    Query = "SELECT Book FROM sonnik WHERE Keyword = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Key])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) -> {reply, lists:flatten(R#result_packet.rows), State}
    end;

handle_call({find_meaning, Keyword}, _, State) ->
    Key = emysql_util:quote(Keyword),
    Query = "SELECT Book, Data FROM sonnik WHERE Keyword = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Key])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) -> {reply, lists:map(fun([X,Y]) -> {X, Y} end, R#result_packet.rows), State}
    end;

handle_call({find_keywords, Keyword}, _, State) ->
    Key = emysql_util:quote(Keyword),
    Query = "SELECT Keyword FROM sonnik WHERE Keyword LIKE ~s GROUP BY Keyword",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Key])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) -> {reply, lists:flatten(R#result_packet.rows), State}
    end;


handle_call({add_user, UserID, Balance}, _, State) ->
    Uid = emysql_util:quote(UserID),
    Query = "INSERT INTO users VALUES (~s, ~B)",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Uid, Balance])),
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> {reply, ok, State};
        R when is_record(R, error_packet) -> {reply, make_error(R), State}
    end;

handle_call({in_history, UserID, Keyword}, _, State) ->
    Uid = emysql_util:quote(UserID),
    Key = emysql_util:quote(Keyword),
    Query = "SELECT COUNT(*) FROM history WHERE UserID = ~s AND Keyword = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Uid, Key])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) ->
            case R#result_packet.rows of
                [[1]] -> {reply, true, State};
                _     -> {reply, false, State}
            end
    end;

handle_call({add_history, UserID, Keyword}, _, State) ->
    Uid = emysql_util:quote(UserID),
    Key = emysql_util:quote(Keyword),
    Query = "INSERT INTO history VALUES (~s, ~s)",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Uid, Key])),
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> {reply, ok, State};
        R when is_record(R, error_packet) -> {reply, make_error(R), State}
    end;

handle_call({get_history, UserID}, _, State) ->
    Uid = emysql_util:quote(UserID),
    Query = "SELECT Keyword FROM history WHERE UserID = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Uid])),
    case execute(QueryBin) of
        R when is_record(R, error_packet) -> {reply, make_error(R), State};
        R when is_record(R, result_packet) -> {reply, lists:append(R#result_packet.rows), State}
    end;


handle_call({get_balance, UserID}, _, State) ->
    Uid = emysql_util:quote(UserID),
    Query = "SELECT Balance from users WHERE UserID = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Uid])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) ->
            case R#result_packet.rows of
                [[N]] when is_integer(N) -> {reply, N, State};
                _                        -> {reply, {error, invalid_uid}, State}
            end
    end;

handle_call({add_balance, UserID, Delta}, _, State) when Delta > 0 ->
    Uid = emysql_util:quote(UserID),
    Query = "UPDATE users SET Balance = Balance + ~B WHERE UserID = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Delta, Uid])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, ok_packet) ->
            case R#ok_packet.affected_rows of
                1 -> {reply, ok, State};
                _ -> {reply, {error, invalid_uid}, State}
            end
    end;

handle_call({del_balance, UserID, Delta}, _, State) when Delta > 0 ->
    Uid = emysql_util:quote(UserID),
    Query = "UPDATE users SET Balance = Balance - ~B WHERE Balance >= ~B AND UserID = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [Delta, Delta, Uid])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, ok_packet) ->
            case R#ok_packet.affected_rows of
                1 -> {reply, ok, State};
                _ -> {reply, {error, invalid_request}, State}
            end
    end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({shutdown, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(Msg, _, State) ->
    error_logger:info_msg("unexpected call received: ~p", [Msg]),
%     ?LOG_ERROR(": unexpected call received: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?LOG_ERROR(": unexpected cast received: ~p", [Msg]),
    {noreply, State}.

handle_info( {'EXIT', Pid, Reason}, State ) ->
    ?LOG_INFO(": unexpected exit signal received from ~p: ~p", [Pid, Reason]),
    {noreply, State};

handle_info( Msg, State ) ->
    ?LOG_ERROR(": unexpected info received: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) ->
    emysql:remove_pool(database),
    ?LOG_DEBUG(": terminated with reason ~p", [Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions:

execute(Query) ->
    try emysql:execute(database, Query)
    catch
        exit:connection_lock_timeout ->
            ?LOG_DEBUG(": connection lock timed out, retrying query", []),
            execute(Query);
        exit:Err -> {error, Err}
    end.


make_error(R) when is_record(R, error_packet) ->
    {error, {R#error_packet.code, R#error_packet.msg}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%