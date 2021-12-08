-module(day3).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Init = lists:duplicate(length(hd(Lines)), 0),
    Counts = lists:foldl(
               fun(Bits, Acc) ->
                       lists:map(fun({Count, $1}) -> Count + 1;
                                    ({Count, $0}) -> Count - 1
                                 end, lists:zip(Acc, Bits))
               end, Init, Lines),
    counts_to_int(Counts) * counts_to_int_invert(Counts).

counts_to_int(Counts) ->
    list_to_integer(lists:map(fun(Count) when Count > 0 -> $1;
                                 (_)                    -> $0
                              end, Counts), 2).

counts_to_int_invert(Counts) ->
    list_to_integer(lists:map(fun(Count) when Count > 0 -> $0;
                                 (_)                    -> $1
                              end, Counts), 2).

part2(Lines) ->
    rating(Lines, 1, oxygen) * rating(Lines, 1, co2).

rating([Line], _I, _Type) ->
    list_to_integer(Line, 2);
rating(Lines, I, Type) ->
    Counter = lists:foldl(fun(Line, Acc) ->
                                  B = lists:nth(I, Line),
                                  counter:incr(Acc, B)
                          end, #{}, Lines),
    Bit = case {Type, counter:max(Counter), counter:min(Counter)} of
              {oxygen, {_, Same}, {_, Same}} -> $1;
              {co2, {_, Same}, {_, Same}}    -> $0;
              {oxygen, {Max, _}, _}          -> Max;
              {co2, _, {Min, _}}             -> Min
          end,
    Filtered = [Line || Line <- Lines, lists:nth(I, Line) == Bit],
    rating(Filtered, I+1, Type).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010",
    [ ?_assertEqual(198, part1(?lines(L)))
    , ?_assertEqual(230, part2(?lines(L)))
    , ?_assertEqual({749376,2372923}, ?solve())
    ].
