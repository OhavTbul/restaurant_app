-module(cashier).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([process_payment/3, generate_receipt/2, get_daily_transactions/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Cashier service started~n"),
    {ok, #{}}.

handle_call({process_payment, CustomerId, Amount, PaymentMethod}, _From, State) ->
    % Validate payment amount
    case Amount > 0 of
        true ->
            % Record payment in ledger
            case ledger_srv:record_payment(CustomerId, Amount) of
                ok ->
                    % Generate receipt
                    Receipt = generate_receipt_internal(CustomerId, Amount, PaymentMethod),
                    io:format("Cashier: Payment ~p processed for customer ~p~n", [Amount, CustomerId]),
                    {reply, {ok, Receipt}, State};
                {error, Reason} ->
                    io:format("Cashier: Payment failed for customer ~p: ~p~n", [CustomerId, Reason]),
                    {reply, {error, Reason}, State}
            end;
        false ->
            {reply, {error, invalid_amount}, State}
    end;

handle_call({generate_receipt, CustomerId, Amount}, _From, State) ->
    Receipt = generate_receipt_internal(CustomerId, Amount, cash),
    {reply, {ok, Receipt}, State};

handle_call(get_daily_transactions, _From, State) ->
    DailySummary = ledger_srv:get_daily_summary(),
    {reply, DailySummary, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Cashier service terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
process_payment(CustomerId, Amount, PaymentMethod) ->
    gen_server:call(?MODULE, {process_payment, CustomerId, Amount, PaymentMethod}).

generate_receipt(CustomerId, Amount) ->
    gen_server:call(?MODULE, {generate_receipt, CustomerId, Amount}).

get_daily_transactions() ->
    gen_server:call(?MODULE, get_daily_transactions).

% Helper function to generate receipt
generate_receipt_internal(CustomerId, Amount, PaymentMethod) ->
    Timestamp = erlang:system_time(second),
    ReceiptId = make_receipt_id(),
    
    Receipt = #{receipt_id => ReceiptId,
                customer_id => CustomerId,
                amount => Amount,
                payment_method => PaymentMethod,
                timestamp => Timestamp,
                restaurant_name => "Functional Programming Restaurant",
                address => "123 Erlang Street, Concurrent City"},
    
    io:format("Receipt generated: ~p~n", [Receipt]),
    Receipt.

% Helper function to generate unique receipt ID
make_receipt_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    "RCPT_" ++ integer_to_list(MegaSecs * 1000000 + Secs) ++ "_" ++ integer_to_list(MicroSecs).
