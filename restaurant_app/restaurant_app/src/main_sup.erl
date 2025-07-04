-module(main_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        % Safe node components
        {task_queue, {task_queue, start_link, []}, permanent, 5000, worker, [task_queue]},
        {controller, {controller, start_link, []}, permanent, 5000, worker, [controller]},
        {ledger_srv, {ledger_srv, start_link, []}, permanent, 5000, worker, [ledger_srv]},
        {order_queue, {order_queue, start_link, []}, permanent, 5000, worker, [order_queue]},
        {player, {player, start_link, []}, permanent, 5000, worker, [player]},
        {cashier, {cashier, start_link, []}, permanent, 5000, worker, [cashier]},
        
        % Node supervisors
        {customer_sup, {customer_sup, start_link, []}, permanent, 5000, supervisor, [customer_sup]},
        {waiter_sup, {waiter_sup, start_link, []}, permanent, 5000, supervisor, [waiter_sup]},
        {machine_sup, {machine_sup, start_link, []}, permanent, 5000, supervisor, [machine_sup]},
        {table_sup, {table_sup, start_link, []}, permanent, 5000, supervisor, [table_sup]},
        
        % Host process
        {host, {host, start_link, []}, permanent, 5000, worker, [host]},
        
        % GUI components
        {gui_sup, {gui_sup, start_link, []}, permanent, 5000, supervisor, [gui_sup]}
    ]}}. 