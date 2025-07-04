-module(table_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_table/1, stop_table/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {table_registry, {table_registry, start_link, []}, permanent, 5000, worker, [table_registry]}
    ]}}.

start_table(TableId) ->
    case table_registry:register_table(TableId) of
        {ok, Pid} ->
            io:format("Table ~p started successfully~n", [TableId]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start table ~p: ~p~n", [TableId, Reason]),
            {error, Reason}
    end.

stop_table(TableId) ->
    case table_registry:unregister_table(TableId) of
        ok ->
            io:format("Table ~p stopped successfully~n", [TableId]),
            ok;
        {error, Reason} ->
            io:format("Failed to stop table ~p: ~p~n", [TableId, Reason]),
            {error, Reason}
    end.
