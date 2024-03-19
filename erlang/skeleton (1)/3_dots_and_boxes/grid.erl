-module(grid).
-export([choose_random_wall/1, get_open_spots/1, show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2, add_wall/2, get_cell_walls/2, get_all_walls/2]).

new(Width, Height) -> {Width, Height, []}.

get_wall(X,Y,north) -> {{X,Y-1},{X,Y}};
get_wall(X,Y,south) -> {{X,Y},{X,Y+1}};
get_wall(X,Y,east) -> {{X,Y},{X+1,Y}};
get_wall(X,Y,west) -> {{X-1,Y},{X,Y}}.

add_wall(Wall,{Width, Height, List}) ->
    Bool = has_wall(Wall, {Width, Height, List}),
    case Bool of
    true -> {Width, Height, List};
    false -> {Width, Height, List ++ [Wall]}
    end.

has_wall(Wall,{_, _, List}) -> lists:any(fun (X) -> X == Wall end, List).

string_hlines(List, Row, Col) ->
    Bool = lists:any(fun (A) -> A == {{Col, Row-1},{Col,Row}} end, List),
    case Bool of
        true -> "--+";
        false -> "  +"
    end.
show_hlines(Row, {Width, _, List}) -> "+" ++ lists:concat(lists:map(fun (Col) -> string_hlines(List, Row, Col) end, (lists:seq(0, Width-1)))) ++ "~n".
    % List = lists:map(fun (Col) -> string_hlines(List, Row, Col) end, (lists:seq(0, Width-1))),
    % "+" ++ lists:concat(List) ++ "~n".

string_vlines(List, Row, Col, End) ->
    Bool = lists:any(fun (A) -> A == {{Col, Row},{Col+1,Row}} end, List),
    case Bool of
        true -> if Col == End -> "|"; true -> "|  " end;
        false -> if Col == End -> " "; true -> "   " end
    end.
show_vlines(Row, {_, Length, List}) -> lists:concat(lists:map(fun (Col) -> string_vlines(List, Row, Col, Length-1) end, (lists:seq(-1, Length-1)))) ++ "~n".

% Prints this grid in a structured format
% using the show_Xlines functions.
print(Grid) ->
    {_, H, _} = Grid,
    lists:map(fun(Row) ->
        io:fwrite(show_hlines(Row, Grid)),

        case Row < H of
            true ->
                io:fwrite(show_vlines(Row, Grid));
            false ->
                ok
        end
    end, lists:seq(0, H)),
    io:fwrite("~n"),
    ok.

get_cell_walls(X,Y) ->
    [get_wall(X,Y,north), get_wall(X,Y,south), get_wall(X,Y,east), get_wall(X,Y,west)].

get_all_walls(W,H) ->
    Set = sets:from_list(lists:concat([get_cell_walls(X, Y) || X <- lists:seq(0, W-1), Y <- lists:seq(0, H-1)])),
    sets:to_list(Set).

get_open_spots({W,H, List}) -> get_all_walls(W, H) -- List.

choose_random_wall(Grid) ->
    OpenSpots = get_open_spots(Grid),
    Index = rand:uniform(length(OpenSpots)),
    lists:nth(Index, OpenSpots).

