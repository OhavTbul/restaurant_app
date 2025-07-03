-module(waiter_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_waiter/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

start_waiter(WaiterId) ->
    supervisor:start_child(?MODULE, {WaiterId, {waiter_fsm, start_link, [WaiterId]}, permanent, 5000, worker, [waiter_fsm]}). 