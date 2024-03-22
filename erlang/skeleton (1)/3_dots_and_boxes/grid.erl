% Name: Tobias van den Bosch
% UvAnettID: 15172635
% Short discription: This module describes the helper functions used by the game_server and client
% module. All the functions in this module are made for implementing a online version of the game
% room rental.

-module(grid).
-export([boxes_at_point/2, amount_boxes_wall/2, choose_random_wall/1, get_open_spots/1,
            show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2, add_wall/2,
            get_cell_walls/2, get_all_walls/2]).

% This function creates a new grid initialised with the given width and height
new(Width, Height) -> {Width, Height, []}.

% This function will give back the wall between the block on the X, Y coordinate on the grid
% (Left above is the 0,0 coordinate) and the block, north, south, east or west of that block,
% depending on the given atom.
get_wall(X,Y,north) -> {{X,Y-1},{X,Y}};
get_wall(X,Y,south) -> {{X,Y},{X,Y+1}};
get_wall(X,Y,east) -> {{X,Y},{X+1,Y}};
get_wall(X,Y,west) -> {{X-1,Y},{X,Y}}.

% This function will add a given wall to the grid if the grid doesn´t contain that wall yet
add_wall(Wall,{Width, Height, List}) ->
    Bool = has_wall(Wall, {Width, Height, List}),
    case Bool of
    true -> {Width, Height, List};
    false -> {Width, Height, List ++ [Wall]}
    end.

% This function returns a boolean of true if the given wall is in the given grid, false otherwise
has_wall(Wall,{_, _, List}) -> lists:any(fun (X) -> X == Wall end, List).

% This function is a helperfunction of show_hlines.
string_hlines(List, Row, Col) ->
    Bool = lists:any(fun (A) -> A == {{Col, Row-1},{Col,Row}} end, List),
    case Bool of
        true -> "--+";
        false -> "  +"
    end.

% This function is a helperfunction of the print function, it returns the horizontal lines of the
% maze drawn with the walls.
show_hlines(Row, {Width, _, List}) ->
    "+" ++ lists:concat(lists:map(fun (Col) ->
        string_hlines(List, Row, Col) end, (lists:seq(0, Width-1)))) ++ "~n".

% This function is a helperfunction of show_vlines
string_vlines(List, Row, Col, End) ->
    Bool = lists:any(fun (A) -> A == {{Col, Row},{Col+1,Row}} end, List),
    case Bool of
        true -> if Col == End -> "|"; true -> "|  " end;
        false -> if Col == End -> " "; true -> "   " end
    end.

% This function is a helperfunction of the print function, it returns the vertical lines of the
% maze drawn with the walls.
show_vlines(Row, {_, Length, List}) ->
    lists:concat(lists:map(fun (Col) ->
        string_vlines(List, Row, Col, Length-1) end, (lists:seq(-1, Length-1)))) ++ "~n".

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

% This function returns the walls of a given block on a given coordinate
get_cell_walls(X,Y) ->
    [get_wall(X,Y,north), get_wall(X,Y,south), get_wall(X,Y,east), get_wall(X,Y,west)].

% This function returns all possible walls of a given grid
get_all_walls(W,H) ->
    Set = sets:from_list(lists:concat([get_cell_walls(X, Y) ||
        X <- lists:seq(0, W-1), Y <- lists:seq(0, H-1)])),
    sets:to_list(Set).

% This function returns all walls in the grid that are not yet drawn
get_open_spots({W,H, List}) -> get_all_walls(W, H) -- List.

% This function gives back a random wall in the grid that isn´t drawn yet
choose_random_wall(Grid) ->
    OpenSpots = get_open_spots(Grid),
    case length(OpenSpots) == 0 of
        true -> [];
        false -> lists:nth(rand:uniform(length(OpenSpots)), OpenSpots)
    end.

% This function is a not working trash function that should have been a helperfunction of
% amount_boxes_wall.
boxes_at_point({A,B}, List) ->
    Bool = lists:member({A,B-1}, List) and lists:member({A+1,B}, List) and
    lists:member({A-1,B}, List) and lists:member({A,B+1}, List),
    file:write_file("/tmp/Debug8.txt", io_lib:fwrite("~w", [Bool])),
    case Bool of true -> 1; false -> 0 end.

% This function is a not working trash function that should have returned the amount of boxes this
% wall is part of (0, 1, or 2)
amount_boxes_wall({{A,B},{C,D}}, {_, _, List}) ->
    boxes_at_point({A,B}, List) + boxes_at_point({C,D}, List).