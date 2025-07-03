-module(machine_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_cook/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

start_cook(MachineId) ->
    supervisor:start_child(?MODULE, {MachineId, {machine_fsm, start_link, [MachineId]}, permanent, 5000, worker, [machine_fsm]}). 