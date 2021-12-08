-module(day2).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?words_lines(Input)), part2(?words_lines(Input))}.

part1(Ins) ->
    part1(Ins, {0,0}).

part1([], {X,Y}) ->
    X*Y;
part1([["forward", Num]|Rest], {X,Y}) ->
    part1(Rest, {X+?int(Num), Y});
part1([["down", Num]|Rest], {X,Y}) ->
    part1(Rest, {X, Y+?int(Num)});
part1([["up", Num]|Rest], {X,Y}) ->
    part1(Rest, {X, Y-?int(Num)}).

part2(Ins) ->
    part2(Ins, {0,0,0}).

part2([], {X,Y, _}) ->
    X*Y;
part2([["forward", Num]|Rest], {X,Y,Aim}) ->
    part2(Rest, {X+?int(Num), Y + (Aim*?int(Num)), Aim});
part2([["down", Num]|Rest], {X,Y,Aim}) ->
    part2(Rest, {X, Y, Aim+?int(Num)});
part2([["up", Num]|Rest], {X,Y,Aim}) ->
    part2(Rest, {X, Y, Aim-?int(Num)}).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "forward 5
down 5
forward 8
up 3
down 8
forward 2",
    [ ?_assertEqual(150, part1(?words_lines(Input)))
    , ?_assertEqual(900, part2(?words_lines(Input)))
    , ?_assertEqual({1714950,1281977850}, ?solve())
    ].
