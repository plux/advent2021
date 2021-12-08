-module(day6).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?ints(Input)), part2(?ints(Input))}.

part1(Fishes) ->
    counter:sum(update(counter:count(Fishes), 80)).

part2(Fishes) ->
    counter:sum(update(counter:count(Fishes), 256)).

update(Fishes, 0) ->
    Fishes;
update(Fishes, Days) ->
    Updated = maps:fold(
                fun(0, V, Acc) -> counter:incr(counter:incr(Acc, 8, V), 6, V);
                   (K, V, Acc) -> counter:incr(Acc, K-1, V)
                end, #{}, Fishes),
    update(Updated, Days-1).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = "3,4,3,1,2",
    [ ?_assertEqual(5934, part1(?ints(L)))
    , ?_assertEqual(26984457539, part2(?ints(L)))
    , ?_assertEqual({380612, 1710166656900}, ?solve())
    ].
