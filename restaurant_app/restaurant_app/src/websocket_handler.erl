-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    io:format("WebSocket connection established~n"),
    {ok, State}.

websocket_handle({text, Data}, State) ->
    try
        Command = jsx:decode(Data, [return_maps]),
        handle_command(Command),
        {ok, State}
    catch
        _:Error ->
            io:format("Error handling WebSocket command: ~p~n", [Error]),
            {ok, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({restaurant_update, Update}, State) ->
    JsonData = jsx:encode(Update),
    {reply, {text, JsonData}, State};

websocket_info({game_update, Update}, State) ->
    JsonData = jsx:encode(Update),
    {reply, {text, JsonData}, State};

websocket_info({status_update, Message}, State) ->
    Update = #{type => status, message => Message},
    JsonData = jsx:encode(Update),
    {reply, {text, JsonData}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    io:format("WebSocket connection closed~n"),
    ok.

handle_command(#{<<"action">> := Action} = Command) ->
    case Action of
        <<"start">> ->
            Difficulty = maps:get(<<"difficulty">>, Command, <<"normal">>),
            io:format("Starting game with difficulty: ~p~n", [Difficulty]),
            game_controller:start_game(),
            send_status_update("Game started with difficulty: " ++ binary_to_list(Difficulty));
            
        <<"pause">> ->
            io:format("Pausing game~n"),
            game_controller:pause_game(),
            send_status_update("Game paused");
            
        <<"resume">> ->
            io:format("Resuming game~n"),
            game_controller:resume_game(),
            send_status_update("Game resumed");
            
        <<"stop">> ->
            io:format("Stopping game~n"),
            game_controller:stop_game(),
            send_status_update("Game stopped");
            
        _ ->
            io:format("Unknown command: ~p~n", [Action])
    end;

handle_command(Command) ->
    io:format("Invalid command format: ~p~n", [Command]).

send_status_update(Message) ->
    % Send status update to all connected WebSocket clients
    % This is a simplified version - in a real app you'd track connections
    io:format("Status: ~s~n", [Message]). 