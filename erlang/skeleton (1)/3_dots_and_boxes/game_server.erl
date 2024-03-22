-module(game_server).

-behaviour(gen_server).

-export([start_link/1, handle_call/3, handle_cast/2]).
-export([init/1, move/2]).
-import(grid, [get_open_spots/1, add_wall/2, amount_boxes_wall/2]).

start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).


% TODO: You need to inform the first player to move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    [FirstPlayer|_] = Players,
    FirstPlayer ! {move, self(), Grid},
    {ok, {Grid, Players}}.

% TODO: add handle_call for move.
handle_call({move, MoveVanSpeler}, _From, {Grid, [CurrentPlayer | RestOfPlayers]}) ->
    case length(RestOfPlayers) > 1 of true -> [NextPlayer | _ ] = RestOfPlayers;
                                      false -> NextPlayer = CurrentPlayer end,
    case lists:member(MoveVanSpeler, get_open_spots(Grid)) of
        false -> Score = 0,
                 NewGrid = Grid;
        true -> NewGrid = add_wall(MoveVanSpeler, Grid),
                Score = amount_boxes_wall(MoveVanSpeler, NewGrid) - amount_boxes_wall(MoveVanSpeler, Grid),
                case get_open_spots(NewGrid) == [] of
                    true -> [Pid ! finished || Pid <- [CurrentPlayer | RestOfPlayers]],
                            gen_server:stop(self())
                end
    end,
    case Score == 0 of
        true -> spawn_link(fun () -> NextPlayer ! {move, self(), Grid} end),
                {reply, {ok, Score}, {NewGrid, RestOfPlayers ++ [CurrentPlayer]}};
        false -> spawn_link(fun () -> CurrentPlayer ! {move, self(), NewGrid} end),
                 {reply, {ok, Score}, {NewGrid, [CurrentPlayer | RestOfPlayers]}}
    end;

% Used for testing.
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From, {{W, H, _}, Players}) ->
    {reply, ok, {{W, H, Walls}, Players}}.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
