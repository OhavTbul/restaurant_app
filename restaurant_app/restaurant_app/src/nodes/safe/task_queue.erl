-module(task_queue).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_task/1, get_task/0, get_queue_length/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, queue:new()}.

handle_call({add_task, Task}, _From, Queue) ->
    NewQueue = queue:in(Task, Queue),
    {reply, ok, NewQueue};

handle_call(get_task, _From, Queue) ->
    case queue:out(Queue) of
        {{value, Task}, NewQueue} ->
            {reply, {ok, Task}, NewQueue};
        {empty, Queue} ->
            {reply, {error, empty}, Queue}
    end;

handle_call(get_queue_length, _From, Queue) ->
    {reply, queue:len(Queue), Queue};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Public API
add_task(Task) ->
    gen_server:call(?MODULE, {add_task, Task}).

get_task() ->
    gen_server:call(?MODULE, get_task).

get_queue_length() ->
    gen_server:call(?MODULE, get_queue_length). 