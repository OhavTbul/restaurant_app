-module(customer_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_client/1, update_customer_state/2, send_state_to_safe/0]).
-export([handle_info/2]).

-define(TABLE_STATE, customer_state).

-record(state, {
    pid_to_id = #{}
}).

%%%===================
%%% API
%%%===================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% create ETS to store customer state externally - if not already exists
    case ets:info(?TABLE_STATE) of
        undefined -> ets:new(?TABLE_STATE, [named_table, public, set]);
        _ -> ok
    end,

    %% Define child spec
    ChildSpec = {
        customer_fsm,                   % ID
        {customer_fsm, start_link, []}, % Start function
        temporary,                      % Restart strategy - no reset, we will make a new proccess for this fail customer
        5000,                           % Shutdown timeout
        worker,                         % Type
        [customer_fsm]                  % Modules
    },
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxTime = 10,

    {ok, {{RestartStrategy, MaxRestarts, MaxTime}, [ChildSpec]}}.

%%%===================
%%% Start & Restart Customers
%%%===================

start_client(CustomerId) ->
    %% Check if already exists
    case ets:lookup(?TABLE_STATE, CustomerId) of
        [{_, Data}] ->
            case maps:get(pid, Data, undefined) of
                undefined ->
                    %% No pid saved – treat as new
                    create_and_monitor(CustomerId);
                Pid when is_pid(Pid) ->
                    %% Check if alive
                    case is_process_alive(Pid) of
                        true ->
                            io:format("Customer ~p already exists. Skipping.~n", [CustomerId]),
                            ok;
                        false ->
                            %% Dead – restart it
                            create_and_monitor(CustomerId)
                    end
            end;
        [] ->
            %% New customer – insert to ETS and monitor
            create_and_monitor(CustomerId)
    end.

create_and_monitor(CustomerId) ->
    case supervisor:start_child(?MODULE, [CustomerId]) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            ets:insert(?TABLE_STATE, {CustomerId, #{pid => Pid}}),
            io:format("Started new customer: ~p~n", [CustomerId]),
            ok;
        {error, Reason} ->
            io:format("Failed to start customer ~p: ~p~n", [CustomerId, Reason]),
            {error, Reason}
    end.


%%%===================
%%% Customer sends updated state periodically
%%%===================

-spec update_customer_state(CustomerId :: term(), Data :: map()) -> ok.
update_customer_state(CustomerId, Data) ->
    %% Data must include pid, pos, state, table, etc.
    ets:insert(?TABLE_STATE, {CustomerId, Data}),
    ok.

%%%===================
%%% Handle customer crash and restart
%%%===================

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case find_customer_id_by_pid(Pid) of
        undefined ->
            io:format("Unknown process ~p crashed.~n", [Pid]),
            {noreply, State};
        CustomerId ->
            io:format("Customer ~p crashed. Restarting...~n", [CustomerId]),
            case ets:lookup(?TABLE_STATE, CustomerId) of
                [{_, Data}] ->
                    %% Start new process with same CustomerId
                    {ok, NewPid} = supervisor:start_child(?MODULE, [CustomerId]),
                    erlang:monitor(process, NewPid),
                    %% Update ETS with new pid, keep other state
                    NewData = maps:put(pid, NewPid, Data),
                    ets:insert(?TABLE_STATE, {CustomerId, NewData}),
                    {noreply, State};
                [] ->
                    io:format("No ETS state for customer ~p. Cannot restart.~n", [CustomerId]),
                    {noreply, State}
            end
    end;

handle_info(_, State) ->
    {noreply, State}.



%%%===================
%%% Send ETS to Safe Node
%%%===================

send_state_to_safe() ->
    AllStates = ets:tab2list(?TABLE_STATE),
    %% Replace with actual communication to safe_node or GUI
    io:format("Sending ~p customer states to SAFE NODE~n", [length(AllStates)]),
    ok.

%%%===================
%%% Helper: find CustomerId by Pid
%%%===================

find_customer_id_by_pid(Pid) ->
    try
        lists:foldl(fun({CustId, Data}, _Acc) ->
            case maps:get(pid, Data, undefined) of
                Pid -> throw({found, CustId});
                _ -> undefined
            end
        end, undefined, ets:tab2list(?TABLE_STATE))
    catch
        throw:{found, Id} -> Id;
        _:_ -> undefined
    end.
