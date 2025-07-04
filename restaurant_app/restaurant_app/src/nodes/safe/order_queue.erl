-module(order_queue).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_order/2, get_next_order/0, get_queue_status/0, cancel_order/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Order queue service started~n"),
    {ok, #{pending => queue:new(), processing => #{}}}.

handle_call({add_order, CustomerId, Order}, _From, State) ->
    OrderId = make_order_id(),
    OrderRecord = #{id => OrderId, customer_id => CustomerId, order => Order, 
                   timestamp => erlang:system_time(second), status => pending},
    PendingQueue = maps:get(pending, State),
    NewPendingQueue = queue:in(OrderRecord, PendingQueue),
    NewState = State#{pending := NewPendingQueue},
    io:format("Order queue: Order ~p added for customer ~p~n", [OrderId, CustomerId]),
    {reply, {ok, OrderId}, NewState};

handle_call(get_next_order, _From, State) ->
    PendingQueue = maps:get(pending, State),
    Processing = maps:get(processing, State),
    case queue:out(PendingQueue) of
        {{value, OrderRecord}, NewPendingQueue} ->
            OrderId = maps:get(id, OrderRecord),
            UpdatedOrder = OrderRecord#{status := processing},
            NewProcessing = maps:put(OrderId, UpdatedOrder, Processing),
            NewState = State#{pending := NewPendingQueue, processing := NewProcessing},
            io:format("Order queue: Order ~p moved to processing~n", [OrderId]),
            {reply, {ok, UpdatedOrder}, NewState};
        {empty, PendingQueue} ->
            {reply, {error, no_orders}, State}
    end;

handle_call(get_queue_status, _From, State) ->
    PendingQueue = maps:get(pending, State),
    Processing = maps:get(processing, State),
    Status = #{pending_count => queue:len(PendingQueue), 
               processing_count => maps:size(Processing)},
    {reply, Status, State};

handle_call({cancel_order, OrderId}, _From, State) ->
    Processing = maps:get(processing, State),
    case maps:find(OrderId, Processing) of
        {ok, OrderRecord} ->
            NewProcessing = maps:remove(OrderId, Processing),
            NewState = State#{processing := NewProcessing},
            io:format("Order queue: Order ~p cancelled~n", [OrderId]),
            {reply, ok, NewState};
        error ->
            {reply, {error, order_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({complete_order, OrderId}, State) ->
    Processing = maps:get(processing, State),
    case maps:find(OrderId, Processing) of
        {ok, OrderRecord} ->
            NewProcessing = maps:remove(OrderId, Processing),
            NewState = State#{processing := NewProcessing},
            io:format("Order queue: Order ~p completed~n", [OrderId]),
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Order queue service terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
add_order(CustomerId, Order) ->
    gen_server:call(?MODULE, {add_order, CustomerId, Order}).

get_next_order() ->
    gen_server:call(?MODULE, get_next_order).

get_queue_status() ->
    gen_server:call(?MODULE, get_queue_status).

cancel_order(OrderId) ->
    gen_server:call(?MODULE, {cancel_order, OrderId}).

complete_order(OrderId) ->
    gen_server:cast(?MODULE, {complete_order, OrderId}).

% Helper function to generate unique order IDs
make_order_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    list_to_atom("order_" ++ integer_to_list(MegaSecs * 1000000 + Secs) ++ "_" ++ integer_to_list(MicroSecs)).
