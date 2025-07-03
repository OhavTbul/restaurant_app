-module(main_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {task_queue, {task_queue, start_link, []}, permanent, 5000, worker, [task_queue]},
        {client_sup, {client_sup, start_link, []}, permanent, 5000, supervisor, [client_sup]},
        {waiter_sup, {waiter_sup, start_link, []}, permanent, 5000, supervisor, [waiter_sup]},
        {kitchen_sup, {kitchen_sup, start_link, []}, permanent, 5000, supervisor, [kitchen_sup]},
        {host, {host, start_link, []}, permanent, 5000, worker, [host]}
    ]}}. 