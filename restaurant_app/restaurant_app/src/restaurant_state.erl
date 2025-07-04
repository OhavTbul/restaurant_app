-module(restaurant_state).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([update_table_state/2, update_waiter_state/2, update_machine_state/2, update_customer_state/2, get_restaurant_state/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Initialize restaurant state
    State = #{
        tables => #{
            table1 => #{occupied => false, customer => undefined, position => {100, 100}},
            table2 => #{occupied => false, customer => undefined, position => {200, 100}},
            table3 => #{occupied => false, customer => undefined, position => {300, 100}},
            table4 => #{occupied => false, customer => undefined, position => {400, 100}},
            table5 => #{occupied => false, customer => undefined, position => {500, 100}}
        },
        waiters => #{
            waiter1 => #{state => idle, position => {50, 200}, current_table => undefined},
            waiter2 => #{state => idle, position => {150, 200}, current_table => undefined},
            waiter3 => #{state => idle, position => {250, 200}, current_table => undefined},
            waiter4 => #{state => idle, position => {350, 200}, current_table => undefined}
        },
        machines => #{
            machine1 => #{state => idle, position => {100, 300}, current_order => undefined},
            machine2 => #{state => cooking, position => {200, 300}, current_order => undefined},
            machine3 => #{state => idle, position => {300, 300}, current_order => undefined},
            machine4 => #{state => idle, position => {400, 300}, current_order => undefined}
        },
        customers => #{}
    },
    {ok, State}.

handle_call(get_restaurant_state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_table_state, TableId, TableState}, State) ->
    Tables = maps:get(tables, State),
    NewTables = maps:put(TableId, TableState, Tables),
    NewState = State#{tables := NewTables},
    broadcast_update(NewState),
    {noreply, NewState};

handle_cast({update_waiter_state, WaiterId, WaiterState}, State) ->
    Waiters = maps:get(waiters, State),
    NewWaiters = maps:put(WaiterId, WaiterState, Waiters),
    NewState = State#{waiters := NewWaiters},
    broadcast_update(NewState),
    {noreply, NewState};

handle_cast({update_machine_state, MachineId, MachineState}, State) ->
    Machines = maps:get(machines, State),
    NewMachines = maps:put(MachineId, MachineState, Machines),
    NewState = State#{machines := NewMachines},
    broadcast_update(NewState),
    {noreply, NewState};

handle_cast({update_customer_state, CustomerId, CustomerState}, State) ->
    Customers = maps:get(customers, State),
    NewCustomers = maps:put(CustomerId, CustomerState, Customers),
    NewState = State#{customers := NewCustomers},
    broadcast_update(NewState),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
update_table_state(TableId, TableState) ->
    gen_server:cast(?MODULE, {update_table_state, TableId, TableState}).

update_waiter_state(WaiterId, WaiterState) ->
    gen_server:cast(?MODULE, {update_waiter_state, WaiterId, WaiterState}).

update_machine_state(MachineId, MachineState) ->
    gen_server:cast(?MODULE, {update_machine_state, MachineId, MachineState}).

update_customer_state(CustomerId, CustomerState) ->
    gen_server:cast(?MODULE, {update_customer_state, CustomerId, CustomerState}).

get_restaurant_state() ->
    gen_server:call(?MODULE, get_restaurant_state).

% Broadcast updates to WebSocket clients
broadcast_update(RestaurantState) ->
    Update = #{
        type => restaurant_update,
        tables => maps:to_list(maps:get(tables, RestaurantState)),
        waiters => maps:to_list(maps:get(waiters, RestaurantState)),
        machines => maps:to_list(maps:get(machines, RestaurantState)),
        customers => maps:to_list(maps:get(customers, RestaurantState))
    },
    % Send to WebSocket handler
    websocket_handler ! {restaurant_update, Update},
    io:format("Restaurant state updated: ~p~n", [Update]). 