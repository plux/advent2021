-module(aoc).

-compile([export_all]).
-include("aoc.hrl").

-type str() :: list().

%% Integers ------------------------------------------------------------
-spec int(str()) -> integer().
int(N) when is_integer(N) -> N;
int(L)                    -> list_to_integer(string:trim(L)).

-spec ints(str()) -> [integer()].
ints(L) ->
    [int(X) || X <- words(L), is_int(X)].

-spec is_int(_) -> boolean().
is_int(X) ->
    is_integer(catch aoc:int(X)).

%% Floats --------------------------------------------------------------
float(L) ->
    list_to_float(string:trim(L)).

-spec floats(str()) -> [float()].
floats(L) ->
    [aoc:float(X) || X <- words(L), aoc:is_float(X)].

-spec is_float(_) -> boolean().
is_float(X) ->
    erlang:is_float(catch aoc:float(X)).

%% Splitting -----------------------------------------------------------
-spec words(str()) -> [str()].
words(L) ->
    string:lexemes(L, ", \t\n").

-spec lines(str()) -> [str()].
lines(L) ->
    string:lexemes(L, "\n").

-spec split(str(), str()) -> [str()].
split(Input, Sep) ->
    case string:split(Input, Sep) of
        [Input]       -> [Input];
        [Match, Rest] -> [Match|split(Rest, Sep)]
    end.

%% Grid
-spec grid([str()]) -> map().
grid(L) ->
    maps:from_list([{{X, Y}, C} || {Y, Line} <- enumerate(L),
                                   {X, C} <- enumerate(Line)]).

draw_grid(L) when is_list(L) ->
    draw_grid(maps:from_list([{{X, Y}, "#"} || {X, Y} <- L]));
draw_grid(Grid) ->
    Xs = [X || {X, _} <- maps:keys(Grid)],
    Ys = [Y || {_, Y} <- maps:keys(Grid)],
    draw_grid(Grid,
              {lists:min(Xs), lists:min(Ys)},
              {lists:max(Xs), lists:max(Ys)}).

draw_grid(Grid, From, To) ->
    io:format("~s", [grid_to_string(Grid, From, To)]).

-spec enumerate([X]) -> [{integer(), X}].
enumerate(L) ->
    lists:zip(lists:seq(0, length(L)-1), L).


cartesian([H|T]) -> [[A|B] || A <- H, B <- cartesian(T)];
cartesian([])    -> [[]].


neighbors_4({PosX, PosY}) ->
    Offsets = [?north, ?east, ?south, ?west],
    [{X0 + PosX,  Y0 + PosY} || {X0, Y0} <- Offsets].

neighbors_8({PosX, PosY}) ->
    Offsets = [?north, ?ne, ?east, ?se, ?south, ?sw, ?west, ?nw],
    [{X0 + PosX,  Y0 + PosY} || {X0, Y0} <- Offsets].

-spec transpose([list()]) -> [list()].
transpose([])     -> [];
transpose([[]|_]) -> [];
transpose(L)      -> [[H || [H|_] <- L]|transpose([T || [_|T] <- L])].

steps(0, _, Acc) -> Acc;
steps(N, F, Acc) -> steps(N-1, F, F(Acc)).

ocr(L) when is_list(L) ->
    ocr(maps:from_list([{{X, Y}, "#"} || {X, Y} <- L]));
ocr(Grid) ->
    try
        {MaxX, _} = lists:max(maps:keys(Grid)),
        [to_char(grid_to_string(Grid, {X, 0}, {X+3, 5})) ||
            X <- lists:seq(0, MaxX, 5)]
    catch _:_ ->
            error
    end.

grid_to_string(Grid, {X1, Y1}, {X2,Y2}) ->
    lists:flatten(
      lists:map(
        fun(Y) ->
                [ lists:map(
                    fun(X) ->
                            case maps:get({X, Y}, Grid, ".") of
                                S when is_list(S) -> S;
                                C when $0 =< C, C =< $9 -> [C];
                                C when 1 =< C, C =< 9 -> [$0+C];
                                C -> io_lib:format("~s", [[C]])
                            end
                    end, lists:seq(X1, X2))
                , "\n"]
        end, lists:seq(Y1,Y2))).


to_char(".##.\n#..#\n#..#\n####\n#..#\n#..#\n") -> $A;
to_char("###.\n#..#\n###.\n#..#\n#..#\n###.\n") -> $B;
to_char(".##.\n#..#\n#...\n#...\n#..#\n.##.\n") -> $C;
to_char("####\n#...\n###.\n#...\n#...\n####\n") -> $E;
to_char("####\n#...\n###.\n#...\n#...\n#...\n") -> $F;
to_char(".##.\n#..#\n#...\n#.##\n#..#\n.###\n") -> $G;
to_char("#..#\n#..#\n####\n#..#\n#..#\n#..#\n") -> $H;
to_char(".###\n..#.\n..#.\n..#.\n..#.\n.###\n") -> $I;
to_char("..##\n...#\n...#\n...#\n#..#\n.##.\n") -> $J;
to_char("#..#\n#.#.\n##..\n#.#.\n#.#.\n#..#\n") -> $K;
to_char("#...\n#...\n#...\n#...\n#...\n####\n") -> $L;
to_char(".##.\n#..#\n#..#\n#..#\n#..#\n.##.\n") -> $O;
to_char("###.\n#..#\n#..#\n###.\n#...\n#...\n") -> $P;
to_char("###.\n#..#\n#..#\n###.\n#.#.\n#..#\n") -> $R;
to_char(".###\n#...\n#...\n.##.\n...#\n###.\n") -> $S;
to_char("#..#\n#..#\n#..#\n#..#\n#..#\n.##.\n") -> $U;
to_char("#...\n#...\n.#.#\n..#.\n..#.\n..#.\n") -> $Y;
to_char("####\n...#\n..#.\n.#..\n#...\n####\n") -> $Z.

%% Solve ---------------------------------------------------------------
solve(Mod) when is_atom(Mod) ->
    {ok, Input} = file:read_file("input/" ++ atom_to_list(Mod) ++ ".txt"),
    {T, Answer} = timer:tc(fun() -> Mod:solve(string:chomp(binary_to_list(Input))) end),
    io:format("~s: ~p (~p ms)\n",
              [Mod, Answer, trunc(math:ceil(T / 1000))]),
    Answer;
solve(Files) ->
    lists:map(
      fun(F) ->
              solve(list_to_atom(hd(string:tokens(F, "."))))
      end, lists:sort(Files)).

solve(Mod, File) ->
    {ok, Input} = file:read_file(File),
    Mod:solve(string:chomp(binary_to_list(Input))).


%% Tests ---------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
aoc_test_() ->
    [ ?_assertEqual(123,            aoc:int("123"))
    , ?_assertEqual([123, 456],     aoc:ints("123 bapa \n456 apa"))
    , ?_assertEqual(false,          aoc:is_int("1.0"))
    , ?_assertEqual(true,           aoc:is_int("1"))
    , ?_assertEqual(["foo", "bar"], aoc:words("foo \n    bar"))
    , ?_assertEqual(["foo", "bar"], aoc:lines("foo\nbar"))
    , ?_assertEqual(1.0,            aoc:float("1.0"))
    , ?_assertEqual([1.5, 8.9],     aoc:floats("1.5 \n6 8.9 apa"))
    , ?_assertEqual(true,           aoc:is_float("1.0"))
    , ?_assertEqual(false,          aoc:is_float("1"))
    ].
