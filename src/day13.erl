-module(day13).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({Points, Folds}) ->
    length(fold(hd(Folds), Points)).

part2({Points, Folds}) ->
    Grid = lists:foldl(fun fold/2, Points, Folds),
    aoc:draw_grid(Grid),
    aoc:ocr(Grid).

fold({Dir, Pos}, Points) ->
    lists:usort(lists:map(fun({X, Y}) when Dir == $x, X > Pos -> {2*Pos - X, Y};
                             ({X, Y}) when Dir == $y, Y > Pos -> {X, 2*Pos - Y};
                             ({X, Y})                         -> {X, Y}
                          end, Points)).

parse(Input) ->
    Lines = ?words_lines(Input),
    Points = [{?int(X), ?int(Y)} || [X, Y] <- Lines],
    Folds = [{Dir, ?int(X)} || ["fold", "along", [Dir, $= | X]] <- Lines],
    {Points, Folds}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5",
    [ ?_assertEqual(17, part1(parse(Input)))
    , ?_assertEqual({error,
                     "#####\n"
                     "#...#\n"
                     "#...#\n"
                     "#...#\n"
                     "#####\n"
                    }, {part2(parse(Input)), ?capturedOutput})
    , ?_assertEqual({790, "PGHZBFJC"}, ?solve())
    ].
