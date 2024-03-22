% Name: Tobias van den Bosch
% UvAnettID: 15172635
% Short discription: This module is practically the full AI that counts as a player for the room
% rental game.

-module(client).
-export([move/0, new/0]).
-import(grid, [choose_random_wall/1]).

% This function will when recieving {move,ServerPid,Grid} give back a random move to the server.
% When recieving finished the function will stop, infinite loop otherwise
move() ->
    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exs1024,{S1, S2, S3}),
    receive
        finished ->
            io:format("~p: I am done~n", [self()]);
        {move,ServerPid,Grid} ->
            Wall = choose_random_wall(Grid),
            gen_server:call(ServerPid, {move,Wall}),
            move()
    end.

new() ->
    spawn(client, move, []).
