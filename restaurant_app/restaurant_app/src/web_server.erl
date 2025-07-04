-module(web_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_web_server/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Start web server after a short delay to ensure dependencies are ready
    erlang:send_after(1000, self(), start_web_server),
    {ok, #{}}.

start_web_server() ->
    try
        Dispatch = cowboy_router:compile([
            {'_', [
                {"/", web_handler, []},
                {"/websocket", websocket_handler, []},
                {"/static/[...]", cowboy_static, {priv_dir, restaurant_app, "static"}}
            ]}
        ]),
        {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
            env => #{dispatch => Dispatch}
        }),
        io:format("Web server started at http://localhost:8080~n")
    catch
        Error:Reason ->
            io:format("Failed to start web server: ~p:~p~n", [Error, Reason]),
            % Retry after 2 seconds
            erlang:send_after(2000, self(), start_web_server)
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_web_server, State) ->
    start_web_server(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    try
        cowboy:stop_listener(http)
    catch
        _:_ -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 