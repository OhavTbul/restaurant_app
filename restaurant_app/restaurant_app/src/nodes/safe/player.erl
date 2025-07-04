-module(player).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_customer_flow/1, trigger_customer_event/2, get_customer_stats/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Player service started~n"),
    {ok, #{}}.

handle_call({start_customer_flow, CustomerId}, _From, State) ->
    % Simulate customer entering restaurant
    io:format("Player: Customer ~p entering restaurant~n", [CustomerId]),
    
    % Find available table
    case table_registry:find_available_table() of
        {ok, TableId} ->
            % Assign customer to table
            controller:assign_customer_to_table(CustomerId, TableId),
            
            % Simulate customer ordering after a delay
            timer:send_after(3000, {customer_order, CustomerId}),
            
            {reply, {ok, TableId}, State};
        {error, no_available_tables} ->
            io:format("Player: No available tables for customer ~p~n", [CustomerId]),
            {reply, {error, no_available_tables}, State}
    end;

handle_call({trigger_customer_event, CustomerId, Event}, _From, State) ->
    case Event of
        order_food ->
            Order = generate_random_order(),
            controller:process_order(CustomerId, Order),
            io:format("Player: Customer ~p ordered ~p~n", [CustomerId, Order]);
        
        finish_eating ->
            controller:complete_order(CustomerId, "completed"),
            io:format("Player: Customer ~p finished eating~n", [CustomerId]);
        
        pay_bill ->
            Amount = generate_random_amount(),
            controller:process_payment(CustomerId, "table_1", Amount),
            io:format("Player: Customer ~p paid ~p~n", [CustomerId, Amount]);
        
        _ ->
            io:format("Player: Unknown event ~p for customer ~p~n", [Event, CustomerId])
    end,
    {reply, ok, State};

handle_call({get_customer_stats, CustomerId}, _From, State) ->
    % Get customer payment history
    Payments = ledger_srv:get_customer_payments(CustomerId),
    TotalSpent = lists:foldl(fun(Payment, Acc) ->
        maps:get(amount, Payment, 0) + Acc
    end, 0, Payments),
    
    Stats = #{customer_id => CustomerId, 
              total_spent => TotalSpent, 
              visit_count => length(Payments)},
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({customer_order, CustomerId}, State) ->
    % Simulate customer placing order
    Order = generate_random_order(),
    controller:process_order(CustomerId, Order),
    
    % Simulate customer finishing eating after a delay
    timer:send_after(5000, {customer_finish_eating, CustomerId}),
    {noreply, State};

handle_info({customer_finish_eating, CustomerId}, State) ->
    % Simulate customer finishing eating
    controller:complete_order(CustomerId, "completed"),
    
    % Simulate customer paying after a delay
    timer:send_after(2000, {customer_pay, CustomerId}),
    {noreply, State};

handle_info({customer_pay, CustomerId}, State) ->
    % Simulate customer paying
    Amount = generate_random_amount(),
    controller:process_payment(CustomerId, "table_1", Amount),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Player service terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
start_customer_flow(CustomerId) ->
    gen_server:call(?MODULE, {start_customer_flow, CustomerId}).

trigger_customer_event(CustomerId, Event) ->
    gen_server:call(?MODULE, {trigger_customer_event, CustomerId, Event}).

get_customer_stats(CustomerId) ->
    gen_server:call(?MODULE, {get_customer_stats, CustomerId}).

% Helper functions
generate_random_order() ->
    Orders = [burger, pizza, salad, pasta, steak, chicken],
    lists:nth(rand:uniform(length(Orders)), Orders).

generate_random_amount() ->
    rand:uniform(50) + 10. % Random amount between 11-60
