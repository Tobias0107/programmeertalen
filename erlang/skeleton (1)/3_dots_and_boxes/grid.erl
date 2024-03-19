-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2, add_wall/2]).

new(Width, Height) -> {Width, Height, []}.

get_wall(X,Y,north) -> {{X,Y-1},{X,Y}};
get_wall(X,Y,south) -> {{X,Y},{X,Y+1}};
get_wall(X,Y,east) -> {{X,Y},{X+1,Y}};
get_wall(X,Y,west) -> {{X-1,Y},{X,Y}}.

add_wall(Wall,{Width, Height, List}) -> {Width, Height, List ++ [Wall]}.

has_wall(Wall,{_, _, List}) -> lists:any(fun (X) -> X == Wall end, List).

string_hlines(List, Row, Col) ->
    Bool = lists:any(fun (A) -> A == {{Col, Row-1},{Col,Row}} end, List),
    case Bool of
        true -> "--+";
        false -> "  +"
    end.
show_hlines(Row, {Width, _, List}) -> "+" ++ lists:map(fun (Col) -> string_hlines(List, Row, Col) end, (lists:seq(0, Width-1))) ++ "\n".

string_vlines(List, Col, X) ->
    Bool = lists:any(fun (A) -> A == {{X, Col},{X+1,Col}} end, List),
    case Bool of
        true -> "|  ";
        false -> "   "
    end.
show_vlines(Col, {_, Length, List}) -> lists:map(fun (X) -> string_vlines(List, Col, X) end, (lists:seq(-1, Length+1))) ++ "\n".

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
