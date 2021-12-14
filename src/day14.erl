-module(day14).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({Template, Rules}) ->
    Result = calc(10, Template, Rules),
    Counts = counter:count(Result),
    {_, Max} = counter:max(Counts),
    {_, Min} = counter:min(Counts),
    Max - Min.

calc(0, Str, _Rules) ->
    Str;
calc(N, Str, Rules) ->
    %% io:format("~p: ~s\n", [10-N, Str]),
    calc(N-1, apply_rules(Str, Rules), Rules).

apply_rules([], _) ->
    [];
apply_rules([X], _) ->
    [X];
apply_rules([A,B|Rest], Rules) ->
    X = maps:get([A,B], Rules),
    [A,X|apply_rules([B|Rest], Rules)].

part2({Points, Folds}) ->
    ok.

chunks([]) ->
    [];
chunks([A,B|Rest]) ->
    [[A,B]|chunks(Rest)].

parse(Input) ->
    [Template|Lines] = ?lines(Input),
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
    %% , ?_assertEqual({error,
    %%                  "#####\n"
    %%                  "#...#\n"
    %%                  "#...#\n"
    %%                  "#...#\n"
    %%                  "#####\n"
    %%                 }, {part2(parse(Input)), ?capturedOutput})
    , ?_assertEqual({790, "PGHZBFJC"}, ?solve())
    ].
