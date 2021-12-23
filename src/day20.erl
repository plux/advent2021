-module(day20).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({Algo, Image}) ->
    solve(Algo, Image, 2).

part2({Algo, Image}) ->
    solve(Algo, Image, 50).

solve([First|_] = Algo, Image, Times) ->
    Last = lists:last(Algo),
    Enhanced = lists:foldl(fun(N, Acc) ->
                                   enhance(Algo, Acc, default(N, First, Last))
                           end, Image, lists:seq(1, Times)),
    length([1 || $# <- maps:values(Enhanced)]).

default(N, $#, _) when N rem 2 =:= 0 -> $#;
default(N, $#, Last) when N =/= 1    -> Last;
default(_, _, _)                     -> $..

enhance(Algo, Image, Default) ->
    {Xs, Ys} = lists:unzip(maps:keys(Image)),
    maps:from_list(
      [{{X, Y}, enhance_px({X, Y}, Algo, Image, Default)} ||
          X <- lists:seq(lists:min(Xs) - 1, lists:max(Xs) + 1),
          Y <- lists:seq(lists:min(Ys) - 1, lists:max(Ys) + 1)]).

enhance_px(Pos, Algo, Image, Default) ->
    lists:nth(pixel_to_index(Pos, Image, Default), Algo).

pixel_to_index({X, Y}, Image, Default) ->
    Coords = [?nw, ?north, ?ne, ?west, {0, 0}, ?east, ?sw, ?south, ?se],
    Bits = [bit(maps:get({X0+X, Y0+Y}, Image, Default)) || {X0, Y0} <- Coords],
    list_to_integer(Bits, 2) + 1.

bit($#) -> $1;
bit($.) -> $0.

parse(Input) ->
    [Algo|Image] = ?lines(Input),
    {Algo, aoc:grid(Image)}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###",
    [ ?_assertEqual(35, part1(parse(Input)))
    , ?_assertEqual(3351, part2(parse(Input)))
    , ?_assertEqual({5326, 17096}, ?solve())
    ].
