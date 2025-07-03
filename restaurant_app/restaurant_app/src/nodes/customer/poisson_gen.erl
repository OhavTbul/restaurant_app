-module(poisson_gen).
-export([start/0, stop/0, spawn_clients/2]).

start() ->
    register(poisson_gen, spawn(?MODULE, loop, [])).

stop() ->
    poisson_gen ! stop.

spawn_clients(Count, Interval) ->
    poisson_gen ! {spawn_clients, Count, Interval}.

loop() ->
    receive
        {spawn_clients, Count, Interval} ->
            spawn_clients_loop(Count, Interval),
            loop();
        stop ->
            ok;
        _ ->
            loop()
    end.

spawn_clients_loop(0, _Interval) ->
    ok;
spawn_clients_loop(Count, Interval) ->
    %% Generate unique customer ID
    CustomerId = make_customer_id(Count),
    
    %% Delegate creation to customer_sup
    ok = customer_sup:start_client(CustomerId),
    io:format("Requested creation of customer ~p~n", [CustomerId]),

    timer:sleep(Interval),
    spawn_clients_loop(Count - 1, Interval).

%% Helper function to make ID – can be replaced with any unique ID logic
make_customer_id(N) ->
    list_to_atom("customer_" ++ integer_to_list(N)).
