-module(table_registry).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([register_table/1, unregister_table/1, get_table_pid/1, get_all_tables/0, find_available_table/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call({register_table, TableId}, _From, Tables) ->
    case maps:is_key(TableId, Tables) of
        true ->
            {reply, {error, already_registered}, Tables};
        false ->
            case table_fsm:start_link(TableId) of
                {ok, Pid} ->
                    NewTables = maps:put(TableId, Pid, Tables),
                    io:format("Table ~p registered with PID ~p~n", [TableId, Pid]),
                    {reply, {ok, Pid}, NewTables};
                {error, Reason} ->
                    {reply, {error, Reason}, Tables}
            end
    end;

handle_call({unregister_table, TableId}, _From, Tables) ->
    case maps:find(TableId, Tables) of
        {ok, Pid} ->
            gen_statem:stop(Pid),
            NewTables = maps:remove(TableId, Tables),
            io:format("Table ~p unregistered~n", [TableId]),
            {reply, ok, NewTables};
        error ->
            {reply, {error, not_found}, Tables}
    end;

handle_call({get_table_pid, TableId}, _From, Tables) ->
    case maps:find(TableId, Tables) of
        {ok, Pid} ->
            {reply, {ok, Pid}, Tables};
        error ->
            {reply, {error, not_found}, Tables}
    end;

handle_call(get_all_tables, _From, Tables) ->
    {reply, maps:keys(Tables), Tables};

handle_call({find_available_table}, _From, Tables) ->
    AvailableTables = find_available_tables(Tables),
    case AvailableTables of
        [] ->
            {reply, {error, no_available_tables}, Tables};
        [TableId | _] ->
            {reply, {ok, TableId}, Tables}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, Tables) ->
    % Remove crashed table from registry
    NewTables = maps:filter(fun(_Key, Value) -> Value =/= Pid end, Tables),
    io:format("Table with PID ~p crashed and was removed from registry~n", [Pid]),
    {noreply, NewTables};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Table registry terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
register_table(TableId) ->
    gen_server:call(?MODULE, {register_table, TableId}).

unregister_table(TableId) ->
    gen_server:call(?MODULE, {unregister_table, TableId}).

get_table_pid(TableId) ->
    gen_server:call(?MODULE, {get_table_pid, TableId}).

get_all_tables() ->
    gen_server:call(?MODULE, get_all_tables).

find_available_table() ->
    gen_server:call(?MODULE, {find_available_table}).

% Helper function to find available tables
find_available_tables(Tables) ->
    maps:fold(fun(TableId, Pid, Acc) ->
        case is_process_alive(Pid) of
            true -> [TableId | Acc];
            false -> Acc
        end
    end, [], Tables).
