-module(day8).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

parse(Input) ->
    lists:map(fun(Line) ->
                      [Left, Right] = ?split(Line, "|"),
                      {?words(Left), ?words(Right)}
              end, ?lines(Input)).

part1(Lines) ->
    Unique = [2, 3, 4, 7],
    lists:sum(
      lists:map(
        fun({_, Output}) ->
                length([W || W <- Output, lists:member(length(W), Unique)])
        end, Lines)).

part2(Lines) ->
    lists:sum(
      lists:map(fun({Input, Output}) ->
                        Mapping = analyze(Input),
                        ?int([maps:get(lists:sort(W), Mapping) || W <- Output])
                end, Lines)).
analyze(L) ->
    Normalized = [lists:sort(X) || X <- L],
    Sorted = lists:sort(fun(A, B) -> length(A) =< length(B) end, Normalized),
    Solved = lists:foldl(fun analyze/2, #{}, Sorted),
    maps:from_list([{V, K} || {K, V} <- maps:to_list(Solved)]).

analyze(W, Acc) when length(W) == 2 -> Acc#{$1 => W};
analyze(W, Acc) when length(W) == 3 -> Acc#{$7 => W};
analyze(W, Acc) when length(W) == 4 -> Acc#{$4 => W};
analyze(W, Acc) when length(W) == 7 -> Acc#{$8 => W};
analyze(W, #{$1 := One, $4 := Four} = Acc) when length(W) == 5 ->
    case {contains(W, One), contains(W, Four -- One)} of
        {true, false}  -> Acc#{$3 => W};
        {false, true}  -> Acc#{$5 => W};
        {false, false} -> Acc#{$2 => W}
    end;
analyze(W, #{$1 := One, $4 := Four} = Acc) when length(W) == 6 ->
    case {contains(W, One), contains(W, Four)} of
        {true, true}   -> Acc#{$9 => W};
        {true, false}  -> Acc#{$0 => W};
        {false, false} -> Acc#{$6 => W}
    end.

contains(Haystack, Needle) ->
    Needle -- Haystack == [].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
    [ ?_assertEqual(26, part1(parse(L)))
    , ?_assertEqual(61229, part2(parse(L)))
    , ?_assertEqual({548,1074888}, ?solve())
    ].
