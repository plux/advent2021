-module(day1).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?ints(Input)), part2(?ints(Input))}.

part1([]) ->
    0;
part1([X,Y|Rest]) when Y > X ->
    1 + part1([Y|Rest]);
part1([_|Rest]) ->
    part1(Rest).

part2(L) ->
    part1(window(L)).

window([A,B,C|Rest]) ->
    [A+B+C|window([B,C|Rest])];
window(_) ->
    [].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = [ 199
        , 200
        , 208
        , 210
        , 200
        , 207
        , 240
        , 269
        , 260
        , 263],
    [ ?_assertEqual(7, part1(L))
    , ?_assertEqual(5, part2(L))
    , ?_assertEqual({1655, 1683}, ?solve())
    ].
