-module(gui_frame).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("GUI Frame started (placeholder for wx GUI)~n"),
    io:format("Menu: Start Game, Settings, Exit~n"),
    io:format("Game Panel: Pause, Resume, Stop Game~n"),
    io:format("Settings Panel: Difficulty selection and configuration~n"),
    {ok, #{game_state => menu}}.

handle_call(start_game, _From, State) ->
    io:format("GUI: Start Game button clicked~n"),
    game_controller:start_game(),
    {reply, ok, State#{game_state := running}};

handle_call(pause_game, _From, State) ->
    io:format("GUI: Pause Game button clicked~n"),
    game_controller:pause_game(),
    {reply, ok, State};

handle_call(resume_game, _From, State) ->
    io:format("GUI: Resume Game button clicked~n"),
    game_controller:resume_game(),
    {reply, ok, State};

handle_call(stop_game, _From, State) ->
    io:format("GUI: Stop Game button clicked~n"),
    game_controller:stop_game(),
    {reply, ok, State#{game_state := menu}};

handle_call({set_difficulty, Difficulty}, _From, State) ->
    io:format("GUI: Difficulty set to ~p~n", [Difficulty]),
    game_controller:set_difficulty(Difficulty),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("GUI Frame terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 