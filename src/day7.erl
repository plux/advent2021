-module(day7).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?ints(Input)), part2(?ints(Input))}.

part1(Crabs) ->
    min_fuel(Crabs, fun(Crab, Pos) -> abs(Crab - Pos) end).

part2(Crabs) ->
    min_fuel(Crabs, fun(Crab, Pos) ->
                      N = abs(Crab - Pos),
                      %% Arithmetic series
                      (N * (N + 1)) div 2
                    end).

min_fuel(Crabs, FuelFun) ->
    Positions = lists:seq(lists:min(Crabs), lists:max(Crabs)),
    lists:min(
      [lists:sum([FuelFun(Crab, Pos) || Crab <- Crabs]) || Pos <- Positions]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "16,1,2,0,4,2,7,1,2,14",
    [ ?_assertEqual(37, part1(?ints(Input)))
    , ?_assertEqual(168, part2(?ints(Input)))
    , ?_assertEqual({336120,96864235}, ?solve())
    ].
