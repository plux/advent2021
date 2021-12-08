-module(advent2021).

-export([main/1]).
-export([solve/1]).
-export([solve/2]).

%% escript Entry point
main([]) ->
    {ok, Files} = file:list_dir("input"),
    solve(Files),
    erlang:halt(0);
main([Arg]) ->
    solve([Arg ++ ".txt"]),
    erlang:halt(0).

solve(Files) ->
    aoc:solve(Files).

solve(Mod, File) ->
    aoc:solve(Mod, File).
