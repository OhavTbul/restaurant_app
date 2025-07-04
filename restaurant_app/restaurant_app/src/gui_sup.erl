-module(gui_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {game_controller, {game_controller, start_link, []}, permanent, 5000, worker, [game_controller]},
        {gui_frame, {gui_frame, start_link, []}, permanent, 5000, worker, [gui_frame]}
    ]}}. 