-module(customer_fsm).
-behaviour(gen_statem).

%% API
-export([start_link/1, assign_table/2, place_order/2, done_eating/1, pay/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% States
-export([idle/3, seated/3, busy/3, paying/3, leaving/3]).

%% Macros
-define(TABLE_TIMEOUT, 5000). % 5sec
-define(ORDER_TIMEOUT, 8000). % 8sec
-define(HEARTBEAT_INTERVAL, 3000).  % 3sec

%%%===================
%%% API
%%%===================

start_link(ClientId) ->
    gen_statem:start_link({local, {customer_fsm, ClientId}}, ?MODULE, ClientId, []).

assign_table(Pid, TableId) ->
    gen_statem:cast(Pid, {assign_table, TableId}).

place_order(Pid, MenuItem) ->
    gen_statem:cast(Pid, {order, MenuItem}).

done_eating(Pid) ->
    gen_statem:cast(Pid, done_eating).

pay(Pid) ->
    gen_statem:cast(Pid, paid).

%%%===================
%%% gen_statem callbacks
%%%===================

init(ClientId) ->
    io:format("Customer ~p entered and waiting for table.~n", [ClientId]),
    %% נניח שהמיקום בגרפיקה מאותחל ל- {0, 0}
    State = #{client_id => ClientId, pos => {0, 0}},
    {ok, idle, State, {state_timeout, ?TABLE_TIMEOUT, timeout_table}}.

callback_mode() -> state_functions.

terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, StateName, Data, _Extra) -> {ok, StateName, Data}.

%%%===================
%%% Heartbeat
%%%===================

send_heartbeat(State) ->
    customer_sup:update_customer_state(maps:get(client_id, State), State),
    ok.


%%%===================
%%% States
%%%===================

idle(cast, {assign_table, TableId}, State) ->
    io:format("Customer ~p assigned to table ~p~n", [maps:get(client_id, State), TableId]),
    %% table POS knwom by table id
    %TablePos = table_position(TableId),  %% need to add function for table pos
    TablePos ={0,0},
    NewState = State#{table => TableId, pos => TablePos},
    send_heartbeat(NewState),
    {next_state, seated, NewState, {state_timeout, ?ORDER_TIMEOUT, timeout_order}};

idle(state_timeout, timeout_table, State) ->
    io:format("Customer ~p gave up waiting for table.~n", [maps:get(client_id, State)]),
    {next_state, leaving, State};

idle(state_timeout, heartbeat, State) ->
    send_heartbeat(State);

%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
idle(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- SEATED

seated(cast, {order, MenuItem}, State) ->
    io:format("Customer ~p ordered ~p~n", [maps:get(client_id, State), MenuItem]),
    NewState = State#{order => MenuItem},
    send_heartbeat(NewState),
    {next_state, busy, NewState};

seated(state_timeout, timeout_order, State) ->
    io:format("Customer ~p waited too long to order. Leaving.~n", [maps:get(client_id, State)]),
    {next_state, leaving, State};

seated(state_timeout, heartbeat, State) ->
    send_heartbeat(State);

%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
seated(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- BUSY

busy(cast, done_eating, State) ->
    io:format("Customer ~p finished eating~n", [maps:get(client_id, State)]),
    send_heartbeat(State),
    {next_state, paying, State};

busy(state_timeout, heartbeat, State) ->
    send_heartbeat(State);

%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
busy(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- PAYING

paying(cast, paid, State) ->
    io:format("Customer ~p paid. Leaving.~n", [maps:get(client_id, State)]),
    %known pos to exit
    ExitPos = {999, 999},
    NewState = State#{pos => ExitPos},
    send_heartbeat(NewState),
    {next_state, leaving, NewState};

paying(state_timeout, heartbeat, State) ->
    send_heartbeat(State);

%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
paying(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- LEAVING

leaving(_Type, _Event, State) ->
    {stop, normal, State}.
