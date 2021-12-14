-module(day14).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({Template, Rules}) ->
    solve(10, pairs(Template, []), Rules).

part2({Template, Rules}) ->
    solve(40, pairs(Template, []), Rules).

pairs([X], Acc) ->
    counter:from_list([{[X], 1}|Acc]);
pairs([A,B|Rest], Acc) ->
    pairs([B|Rest], [{[A,B], 1}|Acc]).

solve(0, Pairs, _Rules) ->
    Counts = split_pairs(maps:to_list(Pairs), []),
    {_, Max0} = counter:max(Counts),
    {_, Min0} = counter:min(Counts),
    (Max0 - Min0) div 2;
solve(N, Pairs0, Rules) ->
    Pairs = counter:from_list(apply_rules(maps:to_list(Pairs0), Rules)),
    solve(N-1, Pairs, Rules).

split_pairs([], Acc) ->
    counter:from_list(Acc);
split_pairs([{[A], Count} | Rest], Acc) ->
    split_pairs(Rest, [{[A], Count} | Acc]);
split_pairs([{[A,B], Count} | Rest], Acc) ->
    split_pairs(Rest, [{[A], Count}, {[B], Count} | Acc]).

apply_rules([], _Rules) ->
    [];
apply_rules([{[A], Count} | Rest], Rules) ->
    [{[A], Count} | apply_rules(Rest, Rules)];
apply_rules([{[A,B], Count} | Rest], Rules) ->
    X = maps:get([A,B], Rules),
    [{[A,X], Count}, {[X,B], Count} | apply_rules(Rest, Rules)].

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
