-module(day15).
-compile([export_all]).
-include("aoc.hrl").

-define(infinity, 999999). % close enough

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1(Grid) ->
    shortest_path(Grid).

part2(Grid) ->
    shortest_path(expand(Grid)).

shortest_path(Grid) ->
    Start = {0,0},
    End = lists:max(maps:keys(Grid)), % bottom right
    Heap = gb_trees:from_orddict([{{0, Start, root}, 0}]),
    Result = dijkstra(Grid, graph(Grid), Heap, #{}, #{}),
    {Length, _} = maps:get(End, Result),
    Length.

dijkstra(Grid, Graph, Heap, Visited, Parents) ->
    case gb_trees:is_empty(Heap) of
        true ->
            Parents;
        false ->
            {{Cost, Node, Prev}, _, NewHeap} = gb_trees:take_smallest(Heap),
            case maps:is_key(Node, Visited) of
                true ->
                    dijkstra(Grid, Graph, NewHeap, Visited, Parents);
                false ->
                    NewParents = maps:put(Node, {Cost, Prev}, Parents),
                    NewVisited = maps:put(Node, 'true', Visited),
                    AdjList = maps:get(Node, Graph),
                    NextHeap =
                        lists:foldl(
                          fun({Neighbor, _Weight}, H)
                                when is_map_key(Neighbor, NewVisited) ->
                                  H;
                             ({Neighbor, Weight}, H) ->
                                  gb_trees:insert({Cost + Weight, Neighbor, Node}, 0, H)
                          end, NewHeap, AdjList),
                    dijkstra(Grid, Graph, NextHeap, NewVisited, NewParents)
            end
    end.

expand(Grid) ->
    {Width, Height} = lists:max(maps:keys(Grid)),
    Steps = [{X, Y} || X <- lists:seq(0,4), Y <- lists:seq(0,4)],
    lists:foldl(
      fun({X,Y}, Acc0) ->
              maps:fold(
                fun({X0, Y0}, V, Acc1) ->
                        Amount0 = V + X + Y,
                        Amount = case Amount0 > 9 of
                                     true  -> Amount0 rem 9;
                                     false -> Amount0
                                 end,
                        Acc1#{{X0 + X * (Width + 1),
                               Y0 + Y * (Height + 1)} => Amount}
                end, Acc0, Grid)
      end, #{}, Steps).

graph(Grid) ->
    maps:fold(
      fun(K, _V, Acc) ->
              Neighs = [{N, maps:get(N, Grid)} || N <- aoc:neighbors_4(K),
                                                  maps:is_key(N, Grid)],
              Acc#{K => Neighs}
      end, #{}, Grid).

parse(Input) ->
    aoc:grid([[C-$0 || C <- Line] || Line <- ?lines(Input)]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581",
    [ ?_assertEqual(40, part1(parse(Input)))
    , ?_assertEqual(315, part2(parse(Input)))
    , ?_assertEqual({487, 2821}, ?solve())
    ].
