-module(day10).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Parsed = [parse(Line, []) || Line <- Lines],
    lists:sum([points(C) || {unexpected, C} <- Parsed]).

part2(Lines) ->
    Parsed = [parse(Line, []) || Line <- Lines],
    L = lists:sort([score(Stack, 0) || {incomplete, Stack} <- Parsed]),
    lists:nth(length(L) div 2 + 1, L).

parse([], Stack) ->
    {incomplete, Stack};
parse([C|Rest], []) ->
    parse(Rest, [closing(C)]);
parse([C|Rest], [Expected|_] = Stack) ->
    case {closing(C), C} of
        {not_open, Expected} -> parse(Rest, tl(Stack));
        {not_open, _}        -> {unexpected, C};
        {Closing, _}         -> parse(Rest, [Closing|Stack])
    end.

score([], Score) ->
    Score;
score([C|Rest], Score) ->
    score(Rest, (Score * 5) + points_complete(C)).

closing($() -> $);
closing($[) -> $];
closing(${) -> $};
closing($<) -> $>;
closing(_)  -> not_open.

points($)) -> 3;
points($]) -> 57;
points($}) -> 1197;
points($>) -> 25137.

points_complete($)) -> 1;
points_complete($]) -> 2;
points_complete($}) -> 3;
points_complete($>) -> 4.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]",
    I2 = [ "[({(<(())[]>[[{[]{<()<>>",
           "[(()[<>])]({[<{<<[]>>(",
           "(((({<>}<{<{<>}{[]{[]{}",
           "{<[[]]>}<{[{[{[]{()[[[]",
           "<{([{{}}[<[[[<>{}]]]>[]]"
         ],
    [ ?_assertEqual(26397, part1(?lines(Input)))
    , ?_assertEqual(288957, part2(I2))
    , ?_assertEqual({364389,2870201088}, ?solve())
    ].
