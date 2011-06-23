-module(dreambook_crawler).
-compile(export_all).

-include_lib("emysql/include/emysql.hrl").

init() ->
    ok = application:start(inets),
    ok = application:start(crypto),
    ok = application:start(emysql),
    ok = emysql:add_pool(sonnik, 10, "sonnik", "sonnik", "localhost", 3306, "sonnik", utf8).

start() ->
    init(),
    crawl_books("http://www.sunhome.ru/dreams").

crawl_books(URL) ->
    {ok, {_, _, Body}} = httpc:request(URL),
    {match, [Table]} = re:run(Body, "<A href=\\\"/dreams\\\" title=\\\".*\\\">(.*)<div class=\\\"marg14\\\"></div>", [dotall, ungreedy, {capture, [1], list}]),
    {match, Links} = re:run(Table, "<a href=\\\"(.*)\\\" title=\\\"(.*)\\\"><U>.*</U></A>", [dotall, ungreedy, global, {capture, [1, 2], list}]),
    lists:foreach( fun([Link, Book]) -> crawl_book(fix(Book), fix_link(Link)) end, Links ).

crawl_book(Book, URL) ->
    io:format("Start crawling book '~ts' [~ts]...~n", [Book, URL]),
    {ok, {_, _, Body}} = httpc:request(URL),
    case re:run(Body, "<table class=\\\"box98\\\">.*</table>(.*)<a href=\\\"/add/dreamer\\\"", [dotall, ungreedy, {capture, [1], list}]) of
        {match, [Table]} ->
            Links =
            case re:run(Table, "<a href=\\\"(.*)\\\"", [dotall, ungreedy, global, {capture, [1], list}]) of
                {match, L} -> [[URL]|L];
                nomatch    -> [[URL]]
            end,
            lists:foreach( fun([Link]) -> crawl_page(Book, fix_link(Link)) end, Links );
        nomatch -> io:format("Start crawling book '~ts' from '~ts': book skipped~n", [Book, URL])
    end.

crawl_page(Book, URL) ->
    io:format("Crawling page '~ts'~n", [URL]),
    {ok, {_, _, Body}} = httpc:request(URL),
    {match, [Table]} = re:run(Body, "<table class=\\\"box98\\\">(.*)</table>", [dotall, ungreedy, {capture, [1], list}]),
    {match, Links}   = re:run(Table, "<a href=\\\"(.*)\\\"><b>(.*)</b></a>", [dotall, ungreedy, global, {capture, [1, 2], list}]),
    lists:foreach( fun([Link, Word]) -> crawl_text(Book, fix(Word), fix_link(Link)) end, Links ).

crawl_text(Book, Word, Link) ->
    io:format("Crawling word '~ts' [~ts]...", [Word, Link]),
    case is_present(Book, Word) of
        true -> io:format(" already present in database~n");
        false ->
            io:format("~n"),
            {ok, {_, _, Body}} = httpc:request(Link),
            {match, [Text]} = re:run(Body, "<div id=\\\"hypercontext\\\"><index>.*<br>(.*)<br><br>.*<div", [dotall, ungreedy, {capture, [1], list}]),
            ok = save(Book, Word, fix(Text))
    end.

fix(Text0) ->
    Text1 = re:replace(Text0, "(^\\s{1,})|(\\s{1,}$)", "", [global]),
    Text2 = re:replace(Text1, "(\\s{2,})|(/r/n)", " ", [global]),
    {ok, Text3} = iconverl:conv("utf-8", "cp1251", list_to_binary(Text2)),
    Text3.

save(Book, Word, Text) ->
    [B, W, T] = lists:map(fun emysql_util:quote/1, [Book, Word, Text]),
    Query = <<"INSERT INTO `sonnik` VALUES (", B/binary, ", ", W/binary, ", ", T/binary, ")">>,
    case emysql:execute(sonnik, Query) of
        R when is_record(R, ok_packet)     -> ok;
        R when is_record(R, result_packet) -> ok;
        R when is_record(R, error_packet)  -> erlang:error(R)
    end.

is_present(Book, Word) ->
    [B, W] = lists:map(fun emysql_util:quote/1, [Book, Word]),
    Query = <<"SELECT COUNT(*) FROM `sonnik` WHERE Book = ", B/binary, " AND Keyword = ", W/binary>>,
    case emysql:execute(sonnik, Query) of
        R when is_record(R, result_packet) ->
            case R#result_packet.rows of
                [[0]] -> false;
                _     -> true
            end;
        _ -> false
    end.


fix_link("http://www.sunhome.ru" ++ Link) -> "http://www.sunhome.ru" ++ Link;
fix_link(Link) -> "http://www.sunhome.ru" ++ Link.