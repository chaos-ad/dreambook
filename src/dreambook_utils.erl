-module(dreambook_utils).

-include("logger.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_option_strict(Key, Values) ->
    case proplists:get_value(Key, Values) of
        undefined -> erlang:error(option_absent);
        Value     -> Value
    end.

extract_option(Key, Values) ->
    {proplists:get_value(Key, Values), proplists:delete(Key, Values)}.

extract_option(Key, Values, Def) ->
    {proplists:get_value(Key, Values, Def), proplists:delete(Key, Values)}.

del_options(Keys, Values) ->
    lists:foldl(fun(Key, Rest) -> proplists:delete(Key, Rest) end, Values, Keys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_options_strict(Keys, Values) ->
    get_options_strict(Keys, Values, []).

get_options_strict([], Values, Results) ->
    {lists:reverse(Results), Values};

get_options_strict([Key|Tail], Values, Results) ->
    get_options_strict(Tail, Values, [get_option_strict(Key, Values)|Results]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

peername({{A, B, C, D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]);

% Will fail on versions, prior to R14B02
peername({IP, Port}) ->
    inet_parse:ntoa(IP) ++ ":" ++ integer_to_list(Port);

peername(Socket) ->
    {ok, Peername} = inet:peername(Socket),
    peername(Peername).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

json_pretty_print(Json) when is_list(Json) ->
    {PrettyPrintedJson, 0} =
        lists:foldl(fun(Char, {Out, Indent}) ->
                            case Char of
                                $: ->
                                    {Out ++ [Char] ++ " ", Indent};
                                ${ ->
                                    NewIndent = Indent + 1,
                                    {Out ++ [Char] ++ "\n" ++ [$\t || _N <- lists:seq(1, NewIndent)], NewIndent};
                                $[ ->
                                    NewIndent = Indent + 1,
                                    {Out ++ [Char] ++ "\n" ++ [$\t || _N <- lists:seq(1, NewIndent)], NewIndent};
                                $} ->
                                    NewIndent = Indent - 1,
                                    {Out ++ "\n" ++ [$\t || _N <- lists:seq(1, NewIndent)] ++ [Char] , NewIndent};
                                $] ->
                                    NewIndent = Indent - 1,
                                    {Out ++ "\n" ++ [$\t || _N <- lists:seq(1, NewIndent)] ++ [Char] , NewIndent};
                                $, ->
                                    {Out ++ [Char] ++ "\n" ++ [$\t || _N <- lists:seq(1, Indent)], Indent};
                                Char1 ->
                                    {Out ++ [Char1], Indent}
                            end
                    end, {"", 0}, lists:flatten(Json)),
    PrettyPrintedJson.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_binary(X) when is_binary(X)  -> X;
to_binary(X) -> list_to_binary(to_list(X)).

to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> float_to_list(A);
to_list(A) when is_binary(A)    -> binary_to_list(A).

md5_hex(Data) ->
    utils:bin_to_hex(erlang:md5(Data)).

bin_to_hex(B) when is_binary(B) ->
    list_to_binary(list_to_hex(binary_to_list(B))).

list_to_hex(L) when is_list(L) ->
    lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, L)).

int_to_hex(N) when N < 256 -> [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a + (N-10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
