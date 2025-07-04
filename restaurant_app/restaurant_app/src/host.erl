-module(host).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([greet_customer/1, assign_table/2, get_waiting_list/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Host service started~n"),
    {ok, #{waiting_list => [], available_tables => []}}.

handle_call({greet_customer, CustomerId}, _From, State) ->
    WaitingList = maps:get(waiting_list, State),
    NewWaitingList = [CustomerId | WaitingList],
    NewState = State#{waiting_list := NewWaitingList},
    io:format("Host: Customer ~p added to waiting list~n", [CustomerId]),
    {reply, {ok, length(NewWaitingList)}, NewState};

handle_call({assign_table, CustomerId, TableId}, _From, State) ->
    WaitingList = maps:get(waiting_list, State),
    case lists:member(CustomerId, WaitingList) of
        true ->
            % Remove customer from waiting list
            NewWaitingList = lists:delete(CustomerId, WaitingList),
            NewState = State#{waiting_list := NewWaitingList},
            
            % Assign customer to table
            case table_registry:get_table_pid(TableId) of
                {ok, _TablePid} ->
                    table_fsm:seat_customer(TableId, CustomerId),
                    customer_fsm:assign_table(CustomerId, TableId),
                    io:format("Host: Customer ~p assigned to table ~p~n", [CustomerId, TableId]),
                    {reply, {ok, TableId}, NewState};
                {error, Reason} ->
                    io:format("Host: Failed to assign customer ~p to table ~p: ~p~n", [CustomerId, TableId, Reason]),
                    {reply, {error, Reason}, State}
            end;
        false ->
            io:format("Host: Customer ~p not in waiting list~n", [CustomerId]),
            {reply, {error, customer_not_waiting}, State}
    end;

handle_call(get_waiting_list, _From, State) ->
    WaitingList = maps:get(waiting_list, State),
    {reply, WaitingList, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Host service terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
greet_customer(CustomerId) ->
    gen_server:call(?MODULE, {greet_customer, CustomerId}).

assign_table(CustomerId, TableId) ->
    gen_server:call(?MODULE, {assign_table, CustomerId, TableId}).

get_waiting_list() ->
    gen_server:call(?MODULE, get_waiting_list). 