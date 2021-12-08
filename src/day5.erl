-module(day5).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?ints_lines(Input)), part2(?ints_lines(Input))}.

part1(Lines) ->
    maps:size(counter:dups(draw_lines(Lines, part1))).

part2(Lines) ->
    maps:size(counter:dups(draw_lines(Lines, part2))).

draw_lines(Lines, Part) ->
    lists:foldl(fun(Line, Acc) ->
                        draw_line(Line, Acc, Part)
                end, #{}, Lines).

draw_line([X, Y1, X, Y2], Grid, _) ->
    draw(Grid, [{X, Y} || Y <- seq(Y1, Y2)]);
draw_line([X1, Y, X2, Y], Grid, _) ->
    draw(Grid, [{X, Y} || X <- seq(X1, X2)]);
draw_line(_, Grid, part1) ->
    %% Only draw horizontal and vertical lines for part 1
    Grid;
draw_line([X1, Y1, X2, Y2], Grid, part2) ->
    L = lists:zip(seq(X1, X2), seq(Y1, Y2)),
    draw(Grid, L).

draw(Grid, Points) ->
    lists:foldl(fun({X, Y}, Acc) ->
                        counter:incr(Acc, {X, Y})
                end, Grid, Points).

seq(A, B) when A < B ->
    lists:seq(A, B, 1);
seq(A, B) ->
    lists:seq(A, B, -1).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    In = "0,9 -> 5,9
          8,0 -> 0,8
          9,4 -> 3,4
          2,2 -> 2,1
          7,0 -> 7,4
          6,4 -> 2,0
          0,9 -> 2,9
          3,4 -> 1,4
          0,0 -> 8,8
          5,5 -> 8,2",
    [ ?_assertEqual(5, part1(?ints_lines(In)))
    , ?_assertEqual(12, part2(?ints_lines(In)))
    , ?_assertEqual({5835, 17013}, ?solve())
    ].
