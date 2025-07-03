-module(machine_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([machine_order/2, finish_machineing/2]).

% States
-export([idle/3, machineing/3]).

start_link(machineId) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, machineId, []).

init(machineId) ->
    {ok, idle, #{machine_id => machineId, current_order => undefined}}.

callback_mode() ->
    state_functions.

% State: idle - machine is available
idle(cast, {machine_order, Order}, State) ->
    % Simulate machineing time
    timer:send_after(2000, {machineing_done, Order}),
    {next_state, machineing, State#{current_order := Order}};

idle(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% State: machineing - machine is preparing food
machineing(info, {machineing_done, Order}, State) ->
    % Notify that food is ready
    io:format("machine ~p finished machineing order: ~p~n", [maps:get(machine_id, State), Order]),
    {next_state, idle, State#{current_order := undefined}};

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
machine_order(machineId, Order) ->
    gen_statem:cast(?MODULE, {machine_order, Order}).

finish_machineing(machineId, Order) ->
    gen_statem:cast(?MODULE, {finish_machineing, Order}). 