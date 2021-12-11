-module(day9).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    Grid = aoc:grid(?lines(Input)),
    {part1(Grid), part2(Grid)}.

part1(Grid) ->
    lists:sum([maps:get(Pos, Grid) - $0 + 1 || Pos <- lowpoints(Grid)]).

part2(Grid) ->
    Basins = [basin([Pos], Grid, gb_sets:new()) || Pos <- lowpoints(Grid)],
    [A, B, C | _] = lists:reverse(lists:sort(Basins)),
    A * B * C.

lowpoints(Grid) ->
    lists:filter(
      fun(Pos) ->
              Neighbors = [maps:get(N, Grid, $9) || N <- aoc:neighbors_4(Pos)],
              maps:get(Pos, Grid) < lists:min(Neighbors)
      end, maps:keys(Grid)).

basin([], _Grid, Acc) ->
    gb_sets:size(Acc);
basin([Pos | Rest], Grid, Acc) ->
    Neighbors = [N || N <- aoc:neighbors_4(Pos),
                      not gb_sets:is_member(N, Acc),
                      maps:get(N, Grid, $9) =/= $9],
    basin(Neighbors ++ Rest, Grid, gb_sets:add(Pos, Acc)).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "2199943210
3987894921
9856789892
8767896789
9899965678",
    Grid = aoc:grid(?lines(Input)),
    [ ?_assertEqual(15, part1(Grid))
    , ?_assertEqual(1134, part2(Grid))
    , ?_assertEqual({562,1076922}, ?solve())
    ].
