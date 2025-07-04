-module(ledger_srv).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([record_payment/2, get_total_revenue/0, get_customer_payments/1, get_daily_summary/0]).

-define(LEDGER_TABLE, restaurant_ledger).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Create ETS table for ledger if it doesn't exist
    case ets:info(?LEDGER_TABLE) of
        undefined -> ets:new(?LEDGER_TABLE, [named_table, public, set]);
        _ -> ok
    end,
    io:format("Ledger service started~n"),
    {ok, #{}}.

handle_call({record_payment, CustomerId, Amount}, _From, State) ->
    Timestamp = erlang:system_time(second),
    PaymentRecord = #{customer_id => CustomerId, amount => Amount, timestamp => Timestamp},
    ets:insert(?LEDGER_TABLE, {Timestamp, PaymentRecord}),
    io:format("Ledger: Payment ~p recorded for customer ~p~n", [Amount, CustomerId]),
    {reply, ok, State};

handle_call(get_total_revenue, _From, State) ->
    Total = ets:foldl(fun({_Key, Record}, Acc) ->
        maps:get(amount, Record, 0) + Acc
    end, 0, ?LEDGER_TABLE),
    {reply, Total, State};

handle_call({get_customer_payments, CustomerId}, _From, State) ->
    Payments = ets:foldl(fun({_Key, Record}, Acc) ->
        case maps:get(customer_id, Record) of
            CustomerId -> [Record | Acc];
            _ -> Acc
        end
    end, [], ?LEDGER_TABLE),
    {reply, Payments, State};

handle_call(get_daily_summary, _From, State) ->
    Today = erlang:system_time(second) div 86400, % Days since epoch
    DailyPayments = ets:foldl(fun({_Key, Record}, Acc) ->
        RecordDay = maps:get(timestamp, Record) div 86400,
        case RecordDay of
            Today -> [Record | Acc];
            _ -> Acc
        end
    end, [], ?LEDGER_TABLE),
    
    DailyTotal = lists:foldl(fun(Record, Acc) ->
        maps:get(amount, Record, 0) + Acc
    end, 0, DailyPayments),
    
    Summary = #{date => Today, total => DailyTotal, count => length(DailyPayments)},
    {reply, Summary, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Ledger service terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
record_payment(CustomerId, Amount) ->
    gen_server:call(?MODULE, {record_payment, CustomerId, Amount}).

get_total_revenue() ->
    gen_server:call(?MODULE, get_total_revenue).

get_customer_payments(CustomerId) ->
    gen_server:call(?MODULE, {get_customer_payments, CustomerId}).

get_daily_summary() ->
    gen_server:call(?MODULE, get_daily_summary).
