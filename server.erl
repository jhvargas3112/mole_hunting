-module(server).
-export([start/0, new_mole/1, new_snake/1, new_worm/1, move_mole/1, move_snake/1,
	move_worm/1, adjacent_hideouts/1, adjacent_paths/1, go_surface/1,
	sniff/1, init/1, handle_cast/2, handle_call/3, handle_info/2,
	terminate/2, code_change/3]).

-include("game_state.hrl").

-behaviour(gen_server).

-define(SERVERNAME, ?MODULE).

start() ->
	{_, Pid} = gen_server:start(?SERVERNAME, [], []),
	Pid.

new_mole({new_mole, MoleName, N}) ->
	gen_server:call(?SERVERNAME, {new_mole, MoleName, N}).

new_snake({new_snake, SnakeName, N}) ->
	gen_server:call(?SERVERNAME, {new_snake, SnakeName, N}).

new_worm({new_worm, WormName, N}) ->
	gen_server:call(?SERVERNAME, {new_worm, WormName, N}).

move_mole({move_mole, MoleName, N}) ->
	gen_server:call(?SERVERNAME, {move_mole, MoleName, N}).

move_snake({move_snake, SnakeName, N}) ->
	gen_server:call(?SERVERNAME, {move_snake, SnakeName, N}).

adjacent_hideouts({adjacent_hideouts, N}) ->
	gen_server:call(?SERVERNAME, {adjacent_hideouts, N}).

adjacent_paths({adjacent_paths, {N1,N2}}) ->
	gen_server:call(?SERVERNAME, {adjacent_paths, {N1,N2}}).

go_surface({go_surface, MoleName}) ->
	gen_server:call(?SERVERNAME, {go_surface, MoleName}).

sniff({sniff, SnakeName, N}) ->
	gen_server:call(?SERVERNAME, {sniff, SnakeName, N}).

move_worm({move_worm, WormName, N}) ->
	gen_server:cast(?SERVERNAME, {move_worm, WormName, N}).

init(_) ->
	process_flag(trap_exit, true),
	dets:open_file(winners, [{file, "winners.dets"}]),
	{ok, {state:state_init(build_hideout_network()), [], [], []}}.

handle_call({new_mole, MoleName, N}, From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,
	{MolePid, _} = From,

	case (lists:keymember(MoleName, 1, MoleProcesses)) of
		true ->
			{reply, {error, already_existing}, ServerState};
		false ->
			io:format("New mole at #~p: ~p.~n", [N, MoleName]),
			{reply, mole_created, {state:new_mole(MoleName, N, GameState), [{MoleName, MolePid}|MoleProcesses], SnakeProcesses, WormProcesses}}
	end;

handle_call({new_snake, SnakeName, N}, From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,
	{SnakePid, _} = From,

	case (lists:keymember(SnakeName, 1, SnakeProcesses)) of
		true ->
			{reply, {error, already_existing}, ServerState};
		false ->
			io:format("New snake at #~p: ~p.~n", [N, SnakeName]),
			{reply, snake_created, {state:new_snake(SnakeName, N, GameState), MoleProcesses, [{SnakeName, SnakePid}|SnakeProcesses], WormProcesses}}
	end;

handle_call({new_worm, WormName, N}, From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,
	{WormPid, _} = From,

	case (lists:keymember(WormName, 1, WormProcesses)) of
		true ->
			{reply, {error, already_existing}, ServerState};
		false ->
			io:format("New worm at #~p: ~p.~n", [N, WormName]),
			{reply, worm_created, {state:new_worm(WormName, N, GameState), MoleProcesses, SnakeProcesses, [{WormName, WormPid}|WormProcesses]}}
	end;

handle_call({move_mole, MoleName, N}, _From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,
	
	case (state:move_mole(MoleName, N, GameState)) of
		{error, {no_valid_move, CurrentHideout, N}} ->
			{reply, {error, {no_valid_move, CurrentHideout, N}}, ServerState};
		_ ->
			% Notification to the snakes that the mole has passed below them:
			CurrentHideout = state:get_mole_position(MoleName, GameState),

			% Gets the list of Pids of the snake processes that will be notified.
			SnakesPids = [element(2, lists:keyfind(SnakeName, 1, SnakeProcesses)) ||
				      SnakeName <- [Snake#snake.name || Snake <- GameState#game_state.snakes,
						    Snake#snake.current_edge =:= {CurrentHideout, N} orelse Snake#snake.current_edge =:= {N, CurrentHideout}
						   ]
				     ],

			% The snakes are notified.
			[SnakePid ! {mole_underneath_you, MoleName} || SnakePid <- SnakesPids],

			case (state:is_mole_full(MoleName, GameState) =:= true
			      andalso
			      state:is_mole_at_own_hideout(MoleName, state:move_mole(MoleName, N, GameState)) =:= true)
			      of
				true ->
					end_of_the_game(mole, MoleName, ServerState),
					{stop, normal, {mole_move, you_win}, {state:move_mole(MoleName, N, GameState), MoleProcesses, SnakeProcesses, WormProcesses}};
				false ->
					{reply, mole_move, {state:move_mole(MoleName, N, GameState), MoleProcesses, SnakeProcesses, WormProcesses}}
			end
	end;

handle_call({move_snake, SnakeName, N}, _From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,

	case (state:move_snake(SnakeName, N, GameState)) of
		{error, {invalid_move, CurrentEdge, N}} ->
			{reply, {error, {invalid_move, CurrentEdge, N}}, {GameState, MoleProcesses, SnakeProcesses, WormProcesses}};
		_ ->
			% Notification to the moles that the snake has passed over their burrows:
			{V1, V2} = state:get_snake_position(SnakeName, GameState),
			{V3, V4} = N,

			% Gets the list of Pids of the mole processes to be notified.
			MolesNames = state:get_moles_at(V1, GameState) ++ state:get_moles_at(V2, GameState),

			MolesPids = [element(2, lists:keyfind(MoleName, 1, MoleProcesses)) ||
				     {_, MoleName} <- [{MolePosition, MoleName} ||
						       {MolePosition, MoleName} <- [{state:get_mole_position(MoleName, GameState), MoleName} ||
										    MoleName <- MolesNames
										   ],
										   MolePosition =:= V3 orelse MolePosition =:= V4
						      ]
				    ],

			% The moles are notified.
			[MolePid ! {snake_over_you, SnakeName} || MolePid <- MolesPids],

			{reply, snake_move, {state:move_snake(SnakeName, N, GameState), MoleProcesses, SnakeProcesses, WormProcesses}}
	end;

handle_call({adjacent_hideouts, N}, _From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,
	{reply, state:get_adjacent_hideouts(N, GameState), {GameState, MoleProcesses, SnakeProcesses, WormProcesses}};

handle_call({adjacent_paths, {N1,N2}}, _From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,
	{reply, state:get_adjacent_paths({N1,N2}, GameState), {GameState, MoleProcesses, SnakeProcesses, WormProcesses}};

handle_call({go_surface, MoleName}, _From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,

	SnakesPositions = lists:map(fun(Snake) -> Snake#snake.current_edge end, GameState#game_state.snakes),
	
	MolePosition = state:get_mole_position(MoleName, GameState),

	SnakesPids = lists:map(fun(SnakeProcess) -> {_, SnakePid} = SnakeProcess, SnakePid end, SnakeProcesses),

	[SnakePid ! {mole_at_surface, MoleName, MolePosition} || SnakePid <- SnakesPids],

	WormsHere = state:get_worms_at(MolePosition, GameState),
	IsMoleAtOwnHideout = state:is_mole_at_own_hideout(MoleName, GameState),

	case (length(WormsHere) > 0) of
		true ->
			WormName = lists:nth(1, WormsHere),
			{_, WormPid} = lists:keyfind(WormName, 1, WormProcesses),
			WormPid ! {killed_by, MoleName},

			if (IsMoleAtOwnHideout =:= true) ->
				end_of_the_game(mole, MoleName, ServerState),
				{stop, normal, {SnakesPositions, WormName, true}, {state:set_mole_full(MoleName, GameState), MoleProcesses, SnakeProcesses, lists:keydelete(WormName, 1, WormProcesses)}};
			   true ->
				{reply, {SnakesPositions, WormName}, {state:set_mole_full(MoleName, GameState), MoleProcesses, SnakeProcesses, lists:keydelete(WormName, 1, WormProcesses)}}
			end;
		_ ->
			{reply, SnakesPositions, ServerState}
	end;

handle_call({sniff, SnakeName, N}, _From, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,

	{V1,V2} = state:get_snake_position(SnakeName, GameState),

	case (N =:= V1 orelse N =:= V2) of
		true ->
			% Notification to moles that the snake has sniff into an adjacent burrow:

			MolesNames = 
			sets:to_list(
				sets:from_list(
					lists:flatten(
						[state:get_moles_at(V, GameState) || V <- state:get_adjacent_hideouts(V1, GameState)]
						++
						[state:get_moles_at(W, GameState) || W <- state:get_adjacent_hideouts(V2, GameState)]
					)
				)
			),

			% Gets the list of positions of the moles and the Pids of the mole processes that will be notified.
			MolesPositionsPids = [{MoleName, MolePosition, element(2, lists:keyfind(MoleName, 1, MoleProcesses))} ||
					      {MolePosition, MoleName} <- [{state:get_mole_position(MoleName, GameState), MoleName} ||
									   MoleName <- MolesNames
									  ]
					     ],

			case (length(state:get_moles_at(N, GameState)) > 0) of
				true ->
					{MoleToRemoveName, MoleToRemovePid} = lists:nth(1, [{MoleName, MolePid} || {MoleName, MolePosition, MolePid} <- MolesPositionsPids, MolePosition =:= N]),

					MoleToRemovePid ! {killed_by, SnakeName},

					NewServerState = {GameState, lists:keydelete(MoleToRemovePid, 2, MoleProcesses), SnakeProcesses, WormProcesses},

					[MolePid ! {snake_sniff, SnakeName, N} || {_, MolePosition, MolePid} <- MolesPositionsPids, MolePosition =/= N],

					end_of_the_game(snake, SnakeName, NewServerState),

					{stop, normal, {true, true, MoleToRemoveName}, NewServerState};
				false ->
					% It is reported that the snake has sniff into an adjacent burrow.
					[MolePid ! {snake_sniff, SnakeName, N} || {_, MolePosition, MolePid} <- MolesPositionsPids, MolePosition =/= N],
					{reply, {state:has_mole_trace(N, GameState), false}, ServerState}
			end;

		false ->
			{reply, {error, {no_snake_there,N}}, ServerState}
	end.

handle_cast({move_worm, WormName, N}, ServerState) ->
	{GameState, MoleProcesses, SnakeProcesses, WormProcesses} = ServerState,

	io:format("Worm ~p moved to ~p.~n", [WormName, N]),
	
	lists:map(fun(Mole) -> {_, MolePid} = Mole, MolePid ! {worm_moved, WormName, N} end, MoleProcesses),
	lists:map(fun(Snake) -> {_, SnakePid} = Snake, SnakePid ! {worm_moved, WormName, N} end, SnakeProcesses),

	{noreply, {state:move_worm(WormName, N, GameState), MoleProcesses, SnakeProcesses, WormProcesses}}.

handle_info(Message, ServerState) ->
	io:format("Unexpected message: ~w~n", [Message]),
	{noreply, ServerState}.

terminate(Reason, _ServerState) ->
	Reason.

code_change(_PreviousVersion, State, _Extra) ->
	{ok, State}.

build_hideout_network() ->
	G = graph:new(),
	[graph:add_vertex(X, G) || X <- lists:seq(1, 18)],

	graph:add_edge(1, 2, G), graph:add_edge(1, 3, G), graph:add_edge(1, 4, G),
	graph:add_edge(2, 3, G), graph:add_edge(2, 4, G), graph:add_edge(2, 5, G),
	graph:add_edge(2, 8, G), graph:add_edge(3, 5, G), graph:add_edge(3, 6, G),
	graph:add_edge(3, 7, G), graph:add_edge(3, 8, G), graph:add_edge(4, 8, G),
	graph:add_edge(4, 9, G), graph:add_edge(4, 10, G), graph:add_edge(6, 7, G),
	graph:add_edge(6, 8, G), graph:add_edge(7, 8, G), graph:add_edge(7, 11, G),
	graph:add_edge(7, 12, G), graph:add_edge(8, 9, G), graph:add_edge(8, 11, G),
	graph:add_edge(8, 13, G), graph:add_edge(8, 14, G), graph:add_edge(9, 10, G),
	graph:add_edge(10, 14, G), graph:add_edge(11, 12, G), graph:add_edge(11, 13, G),
	graph:add_edge(12, 13, G), graph:add_edge(12, 15, G), graph:add_edge(12, 16, G),
	graph:add_edge(13, 14, G), graph:add_edge(13, 15, G), graph:add_edge(14, 15, G),
	graph:add_edge(14, 17, G), graph:add_edge(15, 16, G), graph:add_edge(15, 17, G),
	graph:add_edge(16, 17, G), graph:add_edge(16, 18, G), graph:add_edge(17, 18, G),

	G.

end_of_the_game(Character, NameOfTheWinner, ServerState) ->
	io:format("Game finished: ~p ~p won!.~n", [Character, NameOfTheWinner]),
	{_, MoleProcesses, SnakeProcesses, _} = ServerState,

	MolesPids = [element(2, MoleProcess) || MoleProcess <- MoleProcesses, element(1, MoleProcess) =/= NameOfTheWinner],
	SnakesPids = [element(2, SnakeProcess) || SnakeProcess <- SnakeProcesses, element(1, SnakeProcess) =/= NameOfTheWinner],

	% It is notified that there has been a winner.
	[MolePid ! {game_finished, Character, NameOfTheWinner} || MolePid <- MolesPids],
	[SnakePid ! {game_finished, Character, NameOfTheWinner} || SnakePid <- SnakesPids],

	% A victory is added to the player and this information is stored in a .dets file.
	NumVictories = dets:match(winners, {NameOfTheWinner, '$1'}),

	case (length(NumVictories)) of
		0 ->
			dets:insert(winners, [{NameOfTheWinner, 1}]),
			io:format("~p ~p has won a total of 1 times.~n", [Character, NameOfTheWinner]);
		_ ->
			TotalVictories = lists:nth(1, lists:nth(1, NumVictories)) + 1,
			io:format("~p ~p has won a total of ~p times.~n", [Character, NameOfTheWinner, TotalVictories]),
			dets:insert(winners, [{NameOfTheWinner, TotalVictories}])
	end,

	dets:close(winners).

