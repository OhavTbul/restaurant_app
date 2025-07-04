-module(game_controller).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_game/0, pause_game/0, resume_game/0, stop_game/0, set_difficulty/1, get_state/0]).

-define(GAME_STATES, [menu, running, paused, game_over]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Game controller started~n"),
    {ok, #{game_state => menu, difficulty => normal, score => 0}}.

handle_call(start_game, _From, State) ->
    % Load configuration based on difficulty
    %load_configuration(maps:get(difficulty, State)),
    
    % Start restaurant components using restaurant supervisor
    case restaurant_sup:start_restaurant() of
        {ok, RestaurantState} ->
            % Start customer generation
            poisson_gen:start(),
            poisson_gen:spawn_clients(3, 2000),
            
            io:format("Game started with difficulty: ~p~n", [maps:get(difficulty, State)]),
            {reply, ok, State#{game_state := running, restaurant_state => RestaurantState}};
        {error, Reason} ->
            io:format("Failed to start restaurant: ~p~n", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(pause_game, _From, State) ->
    case maps:get(game_state, State) of
        running ->
            io:format("Game paused~n"),
            {reply, ok, State#{game_state := paused}};
        _ ->
            {reply, {error, invalid_state}, State}
    end;

handle_call(resume_game, _From, State) ->
    case maps:get(game_state, State) of
        paused ->
            io:format("Game resumed~n"),
            {reply, ok, State#{game_state := running}};
        _ ->
            {reply, {error, invalid_state}, State}
    end;

handle_call(stop_game, _From, State) ->
    % Stop customer generation
    poisson_gen:stop(),
    
    % Stop restaurant components
    restaurant_sup:stop_restaurant(),
    
    % Reset game state
    io:format("Game stopped~n"),
    {reply, ok, State#{game_state := menu, score := 0}};

handle_call({set_difficulty, Difficulty}, _From, State) ->
    case Difficulty of
        easy -> 
            io:format("Difficulty set to: easy~n"),
            {reply, ok, State#{difficulty := easy}};
        normal -> 
            io:format("Difficulty set to: normal~n"),
            {reply, ok, State#{difficulty := normal}};
        hard -> 
            io:format("Difficulty set to: hard~n"),
            {reply, ok, State#{difficulty := hard}};
        _ ->
            {reply, {error, invalid_difficulty}, State}
    end;

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Game controller terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% % Load configuration based on difficulty
% load_configuration(easy) ->
%     application:set_env(customer, table_timeout, 10000),
%     application:set_env(customer, order_timeout, 15000),
%     application:set_env(machine, cooking_time, 3000),
%     application:set_env(waiter, service_timeout, 15000),
%     application:set_env(table, max_tables, 8),
%     application:set_env(table, max_waiters, 4);

% load_configuration(normal) ->
%     application:set_env(customer, table_timeout, 5000),
%     application:set_env(customer, order_timeout, 8000),
%     application:set_env(machine, cooking_time, 2000),
%     application:set_env(waiter, service_timeout, 10000),
%     application:set_env(table, max_tables, 12),
%     application:set_env(table, max_waiters, 6);

% load_configuration(hard) ->
%     application:set_env(customer, table_timeout, 3000),
%     application:set_env(customer, order_timeout, 5000),
%     application:set_env(machine, cooking_time, 1000),
%     application:set_env(waiter, service_timeout, 5000),
%     application:set_env(table, max_tables, 20),
%     application:set_env(table, max_waiters, 10).

% Start restaurant components
start_restaurant_components() ->
    % This function is now handled by restaurant_sup:start_restaurant()
    restaurant_sup:start_restaurant().

% Public API
start_game() ->
    gen_server:call(?MODULE, start_game).

pause_game() ->
    gen_server:call(?MODULE, pause_game).

resume_game() ->
    gen_server:call(?MODULE, resume_game).

stop_game() ->
    gen_server:call(?MODULE, stop_game).

set_difficulty(Difficulty) ->
    gen_server:call(?MODULE, {set_difficulty, Difficulty}).

get_state() ->
    gen_server:call(?MODULE, get_state). 