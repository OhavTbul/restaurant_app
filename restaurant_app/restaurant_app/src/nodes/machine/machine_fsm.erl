-module(machine_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([machine_order/2, finish_machineing/2]).

% States
-export([idle/3, machineing/3]).

start_link(MachineId) ->
    Name = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),
    gen_statem:start_link({local, Name}, ?MODULE, MachineId, []).

init(MachineId) ->
    {ok, idle, #{machine_id => MachineId, current_order => undefined}}.

callback_mode() ->
    state_functions.

% State: idle - machine is available
idle(cast, {machine_order, Order}, State) ->
    % Simulate machineing time
    timer:send_after(2000, {machineing_done, Order}),
    NewState = State#{current_order := Order},
    restaurant_state:update_machine_state(maps:get(machine_id, State), #{
        state => cooking,
        position => {200, 300},
        current_order => Order
    }),
    {next_state, machineing, NewState};

idle(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% State: machineing - machine is preparing food
machineing(info, {machineing_done, Order}, State) ->
    % Notify that food is ready
    io:format("machine ~p finished machineing order: ~p~n", [maps:get(machine_id, State), Order]),
    NewState = State#{current_order := undefined},
    restaurant_state:update_machine_state(maps:get(machine_id, State), #{
        state => idle,
        position => {200, 300},
        current_order => undefined
    }),
    {next_state, idle, NewState};

machineing(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% Generic event handler
handle_event(EventType, EventContent, State) ->
    io:format("machine ~p received unexpected event ~p: ~p~n", [maps:get(machine_id, State), EventType, EventContent]),
    {keep_state, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

% Public API
machine_order(MachineId, Order) ->
    Name = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),
    gen_statem:cast(Name, {machine_order, Order}).

finish_machineing(MachineId, Order) ->
    Name = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),
    gen_statem:cast(Name, {finish_machineing, Order}). 