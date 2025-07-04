-module(controller).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([assign_customer_to_table/2, process_order/2, complete_order/2, process_payment/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Restaurant controller started~n"),
    {ok, #{}}.

handle_call({assign_customer_to_table, CustomerId, TableId}, _From, State) ->
    case table_registry:get_table_pid(TableId) of
        {ok, TablePid} ->
            table_fsm:seat_customer(TableId, CustomerId),
            customer_fsm:assign_table(CustomerId, TableId),
            io:format("Controller: Customer ~p assigned to table ~p~n", [CustomerId, TableId]),
            {reply, {ok, TableId}, State};
        {error, Reason} ->
            io:format("Controller: Failed to assign customer ~p to table ~p: ~p~n", [CustomerId, TableId, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({process_order, CustomerId, Order}, _From, State) ->
    % Add order to task queue for kitchen processing
    task_queue:add_task({cook_order, Order}),
    customer_fsm:place_order(CustomerId, Order),
    io:format("Controller: Order ~p processed for customer ~p~n", [Order, CustomerId]),
    {reply, ok, State};

handle_call({complete_order, CustomerId, Order}, _From, State) ->
    % Notify customer that food is ready
    customer_fsm:done_eating(CustomerId),
    io:format("Controller: Order ~p completed for customer ~p~n", [Order, CustomerId]),
    {reply, ok, State};

handle_call({process_payment, CustomerId, TableId, Amount}, _From, State) ->
    % Process payment through ledger
    case ledger_srv:record_payment(CustomerId, Amount) of
        ok ->
            table_fsm:free_table(TableId),
            customer_fsm:pay(CustomerId),
            io:format("Controller: Payment ~p processed for customer ~p~n", [Amount, CustomerId]),
            {reply, ok, State};
        {error, Reason} ->
            io:format("Controller: Payment failed for customer ~p: ~p~n", [CustomerId, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Restaurant controller terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
assign_customer_to_table(CustomerId, TableId) ->
    gen_server:call(?MODULE, {assign_customer_to_table, CustomerId, TableId}).

process_order(CustomerId, Order) ->
    gen_server:call(?MODULE, {process_order, CustomerId, Order}).

complete_order(CustomerId, Order) ->
    gen_server:call(?MODULE, {complete_order, CustomerId, Order}).

process_payment(CustomerId, TableId, Amount) ->
    gen_server:call(?MODULE, {process_payment, CustomerId, TableId, Amount}).
