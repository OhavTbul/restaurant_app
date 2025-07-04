-module(restaurant_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_restaurant/0, stop_restaurant/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

start_restaurant() ->
    % Start table supervisor
    {ok, TableSup} = table_sup:start_link(),
    
    % Start waiter supervisor
    {ok, WaiterSup} = waiter_sup:start_link(),
    
    % Start machine supervisor
    {ok, MachineSup} = machine_sup:start_link(),
    
    % Start 5 tables
    start_tables(),
    
    % Start 4 waiters
    start_waiters(),
    
    % Start 4 cook machines
    start_machines(),
    
    io:format("Restaurant started with 5 tables, 4 waiters, and 4 cook machines~n"),
    {ok, #{table_sup => TableSup, waiter_sup => WaiterSup, machine_sup => MachineSup}}.

start_tables() ->
    lists:foreach(fun(TableId) ->
        case table_sup:start_table(TableId) of
            {ok, _Pid} ->
                io:format("Table ~p started~n", [TableId]);
            {error, Reason} ->
                io:format("Failed to start table ~p: ~p~n", [TableId, Reason])
        end
    end, [table1, table2, table3, table4, table5]).

start_waiters() ->
    lists:foreach(fun(WaiterId) ->
        case waiter_sup:start_waiter(WaiterId) of
            {ok, _Pid} ->
                io:format("Waiter ~p started~n", [WaiterId]);
            {error, Reason} ->
                io:format("Failed to start waiter ~p: ~p~n", [WaiterId, Reason])
        end
    end, [waiter1, waiter2, waiter3, waiter4]).

start_machines() ->
    lists:foreach(fun(MachineId) ->
        case machine_sup:start_cook(MachineId) of
            {ok, _Pid} ->
                io:format("Cook machine ~p started~n", [MachineId]);
            {error, Reason} ->
                io:format("Failed to start cook machine ~p: ~p~n", [MachineId, Reason])
        end
    end, [machine1, machine2, machine3, machine4]).

stop_restaurant() ->
    % Stop all components
    lists:foreach(fun(TableId) ->
        table_sup:stop_table(TableId)
    end, [table1, table2, table3, table4, table5]),
    
    io:format("Restaurant stopped~n"),
    ok. 