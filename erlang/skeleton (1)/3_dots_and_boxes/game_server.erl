% Name: Tobias van den Bosch
% UvAnettID: 15172635
% Short discription: This module is the implementation of the gen_server of a game called room
% rental.

-module(game_server).

-behaviour(gen_server).

-export([start_link/1, handle_call/3, handle_cast/2, check_program_finished/2]).
-export([init/1, move/2]).
-import(grid, [get_open_spots/1, add_wall/2, amount_boxes_wall/2]).

start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).

% Initialise the genserver state with a grid of the given width and height and a list of the given
% PID's of the players of the game.
% The first player is asked to make a move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    [FirstPlayer|_] = Players,
    FirstPlayer ! {move, self(), Grid},
    {ok, {Grid, Players}}.

check_program_finished(NewGrid, ListOfPlayers) ->
    case get_open_spots(NewGrid) == [] of
                    true -> [Pid ! finished || Pid <- ListOfPlayers],
                        timer:sleep(timer:seconds(5)),
                        gen_server:stop(self())
    end.

% If a move of a player is made this function starts working, it doesnt work quite yet. This is
% as far as I know for two reasons, one, the score is not calculated correctly by the functions
% two, line 48 that terminates the server if the game is finished terminates the server.
% This function handles the move of the player by the rules of the game (search google).
% The function doesn´t keep track of the overall score
handle_call({move, MoveVanSpeler}, _From, {Grid, [CurrentPlayer | RestOfPlayers]}) ->
    case length(RestOfPlayers) > 1 of true -> [NextPlayer | _ ] = RestOfPlayers;
                                      false -> NextPlayer = CurrentPlayer end,
    case lists:member(MoveVanSpeler, get_open_spots(Grid)) of
        false -> Score = 0,
                 NewGrid = Grid;
        true -> NewGrid = add_wall(MoveVanSpeler, Grid),
                Score = amount_boxes_wall(MoveVanSpeler, NewGrid) - amount_boxes_wall(MoveVanSpeler, Grid),
                spawn_link(check_program_finished(NewGrid, [CurrentPlayer | RestOfPlayers]))
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
