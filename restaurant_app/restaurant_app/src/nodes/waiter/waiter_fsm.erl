-module(waiter_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([take_order/2, serve_food/2, collect_payment/2]).

% States
-export([idle/3, taking_order/3, serving/3, collecting_payment/3]).

start_link(WaiterId) ->
    Name = list_to_atom("waiter_fsm_" ++ atom_to_list(WaiterId)),
    gen_statem:start_link({local, Name}, ?MODULE, WaiterId, []).

init(WaiterId) ->
    {ok, idle, #{waiter_id => WaiterId, current_table => undefined, current_order => undefined}}.

callback_mode() ->
    state_functions.

% State: idle - waiter is available
idle(cast, {take_order, Table, Order}, State) ->
    NewState = State#{current_table := Table, current_order := Order},
    restaurant_state:update_waiter_state(maps:get(waiter_id, State), #{
        state => taking_order,
        position => {150, 200},
        current_table => Table
    }),
    {next_state, taking_order, NewState};

idle(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% State: taking_order - waiter is taking an order
taking_order(cast, {order_taken, Order}, State) ->
    % Send order to kitchen
    task_queue:add_task({cook_order, Order}),
    NewState = State#{current_table := undefined, current_order := undefined},
    restaurant_state:update_waiter_state(maps:get(waiter_id, State), #{
        state => idle,
        position => {150, 200},
        current_table => undefined
    }),
    {next_state, idle, NewState};

taking_order(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% State: serving - waiter is serving food
serving(cast, {serve_food, Table, Food}, State) ->
    {next_state, idle, State#{current_table := undefined}};

serving(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% State: collecting_payment - waiter is collecting payment
collecting_payment(cast, {payment_collected, Table, Amount}, State) ->
    {next_state, idle, State#{current_table := undefined}};

collecting_payment(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

% Generic event handler
handle_event(EventType, EventContent, State) ->
    io:format("Waiter ~p received unexpected event ~p: ~p~n", [maps:get(waiter_id, State), EventType, EventContent]),
    {keep_state, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

% Public API
take_order(WaiterId, {Table, Order}) ->
    Name = list_to_atom("waiter_fsm_" ++ atom_to_list(WaiterId)),
    gen_statem:cast(Name, {take_order, Table, Order}).

serve_food(WaiterId, {Table, Food}) ->
    Name = list_to_atom("waiter_fsm_" ++ atom_to_list(WaiterId)),
    gen_statem:cast(Name, {serve_food, Table, Food}).

collect_payment(WaiterId, {Table, Amount}) ->
    Name = list_to_atom("waiter_fsm_" ++ atom_to_list(WaiterId)),
    gen_statem:cast(Name, {collect_payment, Table, Amount}). 