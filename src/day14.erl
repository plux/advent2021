-module(day14).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({Template, Rules}) ->
    solve(10, init_pairs(Template, #{}), Rules).

part2({Template, Rules}) ->
    solve(40, init_pairs(Template, #{}), Rules).

init_pairs([X], Acc) ->
    counter:incr(Acc, [X]);
init_pairs([A,B|Rest], Acc) ->
    init_pairs([B|Rest], counter:incr(Acc, [A,B])).

solve(0, Pairs, _Rules) ->
    Counts = counter:from_list([{A, N} || {[A|_], N} <- maps:to_list(Pairs)]),
    {_, Max} = counter:max(Counts),
    {_, Min} = counter:min(Counts),
    Max - Min;
solve(N, Pairs0, Rules) ->
    Pairs = maps:fold(fun([A], Count, Acc)->
                              counter:incr(Acc, [A], Count);
                         ([A,B], Count, Acc0) ->
                              X = maps:get([A,B], Rules),
                              Acc = counter:incr(Acc0, [A,X], Count),
                              counter:incr(Acc, [X,B], Count)
                      end, #{}, Pairs0),
    solve(N-1, Pairs, Rules).

parse(Input) ->
    [Template | Lines] = ?lines(Input),
    Splitted = [?split(Line, " -> ") || Line <- Lines],
    Rules = maps:from_list([{From, To} || [From, [To]] <- Splitted]),
    {Template, Rules}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C",
    [ ?_assertEqual(1588, part1(parse(Input)))
    , ?_assertEqual(2188189693529, part2(parse(Input)))
    , ?_assertEqual({4244, 4807056953866}, ?solve())
    ].
