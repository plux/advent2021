-module(day4).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    Parsed = parse(Input),
    {part1(Parsed), part2(Parsed)}.

parse(Input) ->
    [Nums|Lines] = ?lines(Input),
    {?ints(Nums), parse_boards(Lines)}.

parse_boards([]) ->
    [];
parse_boards([A,B,C,D,E|Rest]) ->
    Rows = [?ints(L) || L <- [A,B,C,D,E]],
    [{Rows, aoc:transpose(Rows)}|parse_boards(Rest)].

part1({Nums, Boards}) ->
    find_bingo_board(Nums, Boards, 0, [], part1).

part2({Nums, Boards}) ->
    find_bingo_board(Nums, Boards, 0, [], part2).

find_bingo_board([], _Boards, Last, _SeenNums, _Part) ->
    Last;
find_bingo_board([Num|RestNums], Boards, Last, SeenNums0, Part) ->
    SeenNums = [Num|SeenNums0],
    case [B || B <- Boards, is_bingo(SeenNums, B)] of
        [] ->
            find_bingo_board(RestNums, Boards, Last, SeenNums, Part);
        [{Rows, _}] when Part == part1 ->
            Count = lists:sum(lists:flatten(Rows) -- SeenNums),
            Count * Num;
        [{Rows, _}|_] = Matches when Part == part2 ->
            Count = lists:sum(lists:flatten(Rows) -- SeenNums),
            Result = Count * Num,
            find_bingo_board(RestNums, Boards -- Matches, Result, SeenNums, Part)
    end.

is_bingo(Nums, {Rows, Cols}) ->
    lists:any(fun(Row) -> Row -- Nums == [] end, Rows ++ Cols).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7",
    [ ?_assertEqual(4512, part1(parse(Input)))
    , ?_assertEqual(1924, part2(parse(Input)))
    , ?_assertEqual({71708, 34726}, ?solve())
    ].
