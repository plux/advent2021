-define(lines(X), aoc:lines(X)).
-define(words(X), aoc:words(X)).
-define(int(X), aoc:int(X)).
-define(ints(X), aoc:ints(X)).
-define(solve(), aoc:solve(?MODULE)).
-define(grid(X), aoc:grid(X)).
-define(words_lines(X), [?words(Line) || Line <- ?lines(X)]).
-define(ints_lines(X), [?ints(Line) || Line <- ?lines(X)]).
-define(int_lines(X), [?int(Line) || Line <- ?lines(X)]).
-define(split(X, Sep), aoc:split(X, Sep)).

%%      N              {0,-1}
%%   NW | NE      {-1,-1}|{1,-1}
%% W ---+--- E {-1,0}----+----{1,0}
%%   SW | SE       {-1,1}|{1,1}
%%      S              {0,1}

-define(west,  {-1,  0}).
-define(east,  { 1,  0}).
-define(north, { 0, -1}).
-define(south, { 0,  1}).

-define(nw,  {-1, -1}).
-define(ne,  { 1, -1}).
-define(sw,  {-1,  1}).
-define(se,  { 1,  1}).

-define(manhattan(X, Y), abs(X) + abs(Y)).
