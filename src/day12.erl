-module(day12).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1(Map) ->
    travel([['start']], Map, 0, part1).

part2(Map) ->
    travel([['start']], Map, 0, part2).

travel([], _Map, PathCount, _Part) ->
    PathCount;
travel([['end'|_]|Rest], Map, PathCount, Part) ->
    travel(Rest, Map, PathCount+1, Part);
travel([[Cave|_] = CurrPath|Rest], Map, PathCount, Part) ->
    NextCaves = [[C|CurrPath] || C <- maps:get(Cave, Map),
                                 C =/= 'start',
                                 (is_big(C) orelse
                                  check_small(Part, [C|CurrPath]))],
    travel(NextCaves ++ Rest, Map, PathCount, Part).

check_small(part1, [C|Rest]) ->
    not lists:member(C, Rest);
check_small(part2, Path) ->
    SmallCaves = [C || C <- Path, not is_big(C)],
    length(lists:usort(SmallCaves)) + 1 >= length(SmallCaves).

is_big({big, _}) -> true;
is_big(_)        -> false.

parse(Input) ->
    lists:foldl(
      fun([A0, B0], Acc0) ->
              A = to_node(A0),
              B = to_node(B0),
              Acc1 = maps:update_with(A, fun(L) -> [B|L] end, [B], Acc0),
              maps:update_with(B, fun(L) -> [A|L] end, [A], Acc1)
      end, #{}, [?split(Line, "-") || Line <- ?lines(Input)]).

to_node(Str) ->
    case string:to_upper(Str) == Str of
        true -> {big, list_to_atom(Str)};
        false -> list_to_atom(Str)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "start-A
start-b
A-c
A-b
b-d
A-end
b-end",
    [ ?_assertEqual(10, part1(parse(Input)))
    , ?_assertEqual(36, part2(parse(Input)))
    , ?_assertEqual({4011,108035}, ?solve())
    ].
