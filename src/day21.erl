-module(day21).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({P1, P2}) ->
    play({P1, 0}, {P2, 0}, 0).

part2({P1, P2}) ->
    erase(),
    Rolls = [X + Y + Z || X <- [1, 2, 3], Y <- [1, 2, 3], Z <- [1, 2, 3]],
    {Wins1, Wins2} = wins({p1, P1, 0}, {p2, P2, 0}, Rolls),
    max(Wins1, Wins2).

play({_, LoserScore}, {_, WinnerScore}, Dice) when WinnerScore >= 1000 ->
    Dice * LoserScore;
play({Pos, Score}, OtherPlayer, Dice) ->
    NewPos = move(Pos, Dice),
    play(OtherPlayer, {NewPos, Score + NewPos + 1}, Dice + 3).

move(Pos, Dice) ->
    (Pos + roll(Dice, 1) + roll(Dice, 2) + roll(Dice, 3)) rem 10.

roll(Dice, N) ->
    (Dice + N) rem 100.

wins(_, {p1, _, Score}, _Rolls) when Score >= 21 ->
    {1, 0};
wins(_, {_, _, Score}, _Rolls) when Score >= 21 ->
    {0, 1};
wins({Who, Pos, Score} = Curr, Other, Rolls) ->
    ?memo(
       {Curr, Other},
       fun() ->
               lists:foldl(
                 fun(Roll, {Sum1, Sum2}) ->
                         NewPos = (Pos + Roll) rem 10,
                         NewScore = Score + NewPos + 1,
                         Player = {Who, NewPos, NewScore},
                         {Wins1, Wins2} = wins(Other, Player, Rolls),
                         {Sum1 + Wins1, Sum2 + Wins2}
                 end, {0, 0}, Rolls)
       end).

parse(Input) ->
    [1, P1, 2, P2] = ?ints(Input),
    {P1-1, P2-1}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "Player 1 starting position: 4
Player 2 starting position: 8",
    [ ?_assertEqual(739785, part1(parse(Input)))
    , ?_assertEqual(444356092776315, part2(parse(Input)))
    , ?_assertEqual({604998, 157253621231420}, ?solve())
    ].
