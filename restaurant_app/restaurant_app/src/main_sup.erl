-module(main_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {restaurant_sup, {restaurant_sup, start_link, []}, permanent, 5000, supervisor, [restaurant_sup]},
        {customer_sup, {customer_sup, start_link, []}, permanent, 5000, supervisor, [customer_sup]},
        {restaurant_state, {restaurant_state, start_link, []}, permanent, 5000, worker, [restaurant_state]},
        {gui_sup, {gui_sup, start_link, []}, permanent, 5000, supervisor, [gui_sup]}
    ]}}. 