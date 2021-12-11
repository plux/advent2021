-module(day11).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Grid0 = aoc:grid(Lines),
    total_flashes(100, Grid0, 0).

part2(Lines) ->
    Grid0 = aoc:grid(Lines),
    find_sync(1, Grid0).

total_flashes(0, _Grid, Flashes) ->
    Flashes;
total_flashes(N, Grid0, Flashes0) ->
    Grid1 = increase_energy_levels(Grid0),
    Grid = update_flashes(Grid1),
    Flashes = count_flashes(Grid),
    total_flashes(N-1, Grid, Flashes0 + Flashes).

find_sync(N, Grid0) ->
    Grid1 = increase_energy_levels(Grid0),
    Grid = update_flashes(Grid1),
    case count_flashes(Grid) == maps:size(Grid0) of
        true  -> N;
        false -> find_sync(N+1, Grid)
    end.

increase_energy_levels(Grid) ->
    maps:map(fun(_, V) -> V+1 end, Grid).

update_flashes(Grid0) ->
    Coords = maps:keys(Grid0),
    Grid = lists:foldl(
             fun(Pos, Acc0) ->
                     case maps:get(Pos, Acc0) of
                         V when V > $9 ->
                             %% FLASHING
                             Acc1 = update_neighbors(Pos, Acc0),
                             maps:put(Pos, $0, Acc1);
                         _ ->
                             Acc0
                     end
             end, Grid0, Coords),
    case Grid of
        Grid0 -> Grid;
        _Else -> update_flashes(Grid)
    end.

update_neighbors(Pos, Grid) ->
    lists:foldl(
      fun(N, Acc) ->
              case maps:get(N, Acc, $0) of
                  $0 -> Acc;
                  _ -> counter:incr(Acc, N)
              end
      end, Grid, aoc:neighbors_8(Pos)).

count_flashes(Grid) ->
    length([N || N <- maps:values(Grid), N == $0]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526",
    [ ?_assertEqual(1656, part1(?lines(Input)))
    , ?_assertEqual(195, part2(?lines(Input)))
    , ?_assertEqual({1717,476}, ?solve())
    ].
