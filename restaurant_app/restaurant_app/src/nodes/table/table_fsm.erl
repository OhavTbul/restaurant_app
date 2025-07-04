-module(table_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([reserve_table/2, seat_customer/2, free_table/1]).

% States
-export([available/3, occupied/3, reserved/3]).

start_link(TableId) ->
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
    gen_statem:start_link({local, Name}, ?MODULE, TableId, []).

init(TableId) ->
    io:format("Table ~p initialized as available~n", [TableId]),
    {ok, available, #{table_id => TableId, customer_id => undefined, position => {0, 0}}}.

callback_mode() ->
    state_functions.

% State: available - table is free for customers
available(cast, {reserve_table, CustomerId}, State) ->
    io:format("Table ~p reserved for customer ~p~n", [maps:get(table_id, State), CustomerId]),
    NewState = State#{customer_id := CustomerId},
    restaurant_state:update_table_state(maps:get(table_id, State), #{
        occupied => false, 
        customer => CustomerId, 
        position => maps:get(position, State)
    }),
    {next_state, reserved, NewState};

available(cast, {seat_customer, CustomerId}, State) ->
    io:format("Customer ~p seated at table ~p~n", [CustomerId, maps:get(table_id, State)]),
    NewState = State#{customer_id := CustomerId},
    restaurant_state:update_table_state(maps:get(table_id, State), #{
        occupied => true, 
        customer => CustomerId, 
        position => maps:get(position, State)
    }),
    {next_state, occupied, NewState};

available(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% State: occupied - table has a customer
occupied(cast, {free_table, CustomerId}, State) ->
    case maps:get(customer_id, State) of
        CustomerId ->
            io:format("Table ~p freed by customer ~p~n", [maps:get(table_id, State), CustomerId]),
            NewState = State#{customer_id := undefined},
            restaurant_state:update_table_state(maps:get(table_id, State), #{
                occupied => false, 
                customer => undefined, 
                position => maps:get(position, State)
            }),
            {next_state, available, NewState};
        _ ->
            io:format("Table ~p: unauthorized free attempt by ~p~n", [maps:get(table_id, State), CustomerId]),
            {keep_state, State}
    end;

occupied(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% State: reserved - table is reserved but not occupied
reserved(cast, {seat_customer, CustomerId}, State) ->
    case maps:get(customer_id, State) of
        CustomerId ->
            io:format("Reserved customer ~p seated at table ~p~n", [CustomerId, maps:get(table_id, State)]),
            {next_state, occupied, State};
        _ ->
            io:format("Table ~p: unauthorized seating attempt by ~p~n", [maps:get(table_id, State), CustomerId]),
            {keep_state, State}
    end;

reserved(cast, {cancel_reservation, CustomerId}, State) ->
    case maps:get(customer_id, State) of
        CustomerId ->
            io:format("Reservation cancelled for customer ~p at table ~p~n", [CustomerId, maps:get(table_id, State)]),
            {next_state, available, State#{customer_id := undefined}};
        _ ->
            io:format("Table ~p: unauthorized cancellation by ~p~n", [maps:get(table_id, State), CustomerId]),
            {keep_state, State}
    end;

reserved(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% Generic event handler
handle_event(EventType, EventContent, State) ->
    io:format("Table ~p received unexpected event ~p: ~p~n", [maps:get(table_id, State), EventType, EventContent]),
    {keep_state, State}.

terminate(_Reason, _StateName, State) ->
    io:format("Table ~p terminating~n", [maps:get(table_id, State)]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

% Public API
reserve_table(TableId, CustomerId) ->
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
    gen_statem:cast(Name, {reserve_table, CustomerId}).

seat_customer(TableId, CustomerId) ->
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
    gen_statem:cast(Name, {seat_customer, CustomerId}).

free_table(TableId) ->
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
    gen_statem:cast(Name, {free_table, TableId}).
