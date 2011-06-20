-module(server).
-behavior(gen_server).

-include("logger.hrl").
-include_lib("emysql/include/emysql.hrl").

-export([start/0, start/1, stop/1, stop/2]).
-export([init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, handle_http_request/1]).
-export([find_meaning/1, find_keywords/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start()             -> start([]).
start(Options)      -> gen_server:start( {local, ?MODULE}, ?MODULE, Options, [] ).
stop(Pid)           -> stop(Pid, shutdown).
stop(Pid, Reason)   -> gen_server:call(Pid, {shutdown, Reason}, infinity).

find_meaning(Text)  -> gen_server:call(?MODULE, {find_meaning, Text}).
find_keywords(Text) -> gen_server:call(?MODULE, {find_keywords, Text}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Options) ->
    process_flag(trap_exit, true),

    DBPort = proplists:get_value(db_port, Options, 3306),
    DBHost = proplists:get_value(db_host, Options, "localhost"),
    DBUser = proplists:get_value(db_user, Options, "sonnik"),
    DBPass = proplists:get_value(db_pass, Options, "sonnik"),
    DBName = proplists:get_value(db_name, Options, "sonnik"),

    WebIP   = proplists:get_value(web_ip, Options, "0.0.0.0"),
    WebPort = proplists:get_value(web_port, Options, 8080),

    LoggerConfig = proplists:get_value(logconf, Options, "logger.conf"),
    ok = logger:start(LoggerConfig),

    application:start(crypto),
    application:start(emysql),
    ok = emysql:add_pool(database, 10, DBUser, DBPass, DBHost, DBPort, DBName, utf8),

    mochiweb_http:start([{ip, WebIP}, {port, WebPort}, {loop, {?MODULE, handle_http_request, []}}]),

    {ok, nil}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peername(Socket) ->
    {ok, {{A, B, C, D}, Port}} = inet:peername(Socket),
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]).

handle_http_request(Request) ->
    ?LOG_TRACE(": request from ~s: ~s", [peername(Request:get(socket)), Request:get(raw_path)]),
    case "/api/" =:= Request:get(path) of
        false -> Request:respond({400, [], []});
        true  ->
            QS = Request:parse_qs(),
            case lists:keyfind("method", 1, QS) of
                false -> Request:respond({400, [], []});
                {"method", "find_keywords"} ->
                    case lists:keyfind("keyword", 1, QS) of
                        false -> Request:respond({400, [], []});
                        {"keyword", Keyword} ->
                            Request:ok({"application/json", find_keywords(json, Keyword)})
                    end;
                {"method", "find_meaning"} ->
                    case lists:keyfind("keyword", 1, QS) of
                        false -> Request:respond({400, [], []});
                        {"keyword", Keyword} ->
                            Request:ok({"application/json", find_meaning(json, Keyword)})
                    end;
                _ -> Request:respond({400, [], []})
            end
    end.

find_keywords(json, Text) ->
    json(find_keywords(Text)).

find_meaning(json, Text) ->
    Meanings = lists:map(fun([B,M]) -> {struct, [{<<"Сонник">>, B}, {<<"Значение">>, M}]} end, find_meaning(Text)),
    json({struct, [{<<"Толкования">>, Meanings}]}).

json(Text) ->
    Encode = mochijson2:encoder([{utf8, true}]),
    Result = Encode(Text),
    Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({find_keywords, Text}, _, State) ->
    Keyword = to_binary(emysql_util:quote(Text)),
    Query = <<"SELECT Keyword FROM sonnik WHERE Keyword LIKE ", Keyword/binary, " GROUP BY Keyword">>,
    case execute(Query) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) -> {reply, lists:flatten(R#result_packet.rows), State}
    end;

handle_call({find_meaning, Text}, _, State) ->
    Keyword = to_binary(emysql_util:quote(Text)),
    Query = <<"SELECT Book, Data FROM sonnik WHERE Keyword = ", Keyword/binary>>,
    case execute(Query) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) -> {reply, R#result_packet.rows, State}
    end;

handle_call({shutdown, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(Msg, _, State) ->
    ?LOG_ERROR(": unexpected call received: ~p", [Msg]),
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
    ?LOG_DEBUG(": terminated with reason ~p", [Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

to_binary(X) when is_binary(X)  -> X;
to_binary(X) -> list_to_binary(to_list(X)).

to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> float_to_list(A);
to_list(A) when is_binary(A)    -> binary_to_list(A).