-module(snake).
-export([rattly/0, lucy/0]).

rattly() ->
	init_snake(rattly, {2,8}).

lucy() ->
	init_snake(lucy, {14,15}).

init_snake(SnakeName, Path) ->
	connect_to_game_server(SnakeName, Path).

connect_to_game_server(SnakeName, Path) ->
	io:format("Don't forget to write a dot (.) at the end of every input!~n"),
	{_, Input, _} = io:scan_erl_exprs("Node name of the server: "),
	{Status, ServerNodeName} = erl_parse:parse_term(Input),

	case(Status) of
		ok ->
			Self = spawn_link(fun()->
						Resp = gen_server:call({server, ServerNodeName}, {new_snake, SnakeName, Path}),
						input:show_snake_game_controls(),

						case (Resp) of
							{error, already_existing} ->
								io:format("~nCannot create snake ~p: {already_existing, ~p}.~n", [SnakeName, SnakeName]);
							snake_created ->
								io:format("~p starting at #~p~n", [SnakeName, Path]),
								process_messages(SnakeName, Path, ServerNodeName)
						end
					  end
			       ),

			input:input_loop(SnakeName, Self);
		error ->
			io:format("A valid Erlang atom is expected: ~w~n",[ServerNodeName]),
			connect_to_game_server(SnakeName, Path)
	end.

process_messages(SnakeName, Path, ServerNodeName) ->
	receive
		{input, a} ->
			AdjacentPaths = gen_server:call({server, ServerNodeName}, {adjacent_paths, Path}),
			io:format("Adjacent paths: ~p.~n", [AdjacentPaths]);
		{input, {s,N}} ->
			Resp = gen_server:call({server, ServerNodeName}, {sniff, SnakeName, N}),

			case (Resp) of
				{error, {no_snake_there, N}} ->
					io:format("Cannot sniff there: {no_snake_there,~p}.~n", [N]);
				{true, true, MoleName}->
					io:format("A mole has been here before...~n"),
					io:format("You caught a mole! ~p.~n", [MoleName]),
					io:format("Congratulations! You won!~n");
				{true, false} ->
					io:format("A mole has been here before...~n"),
					io:format("No moles here :-(~n");
				{false, false} ->
					io:format("No mole traces found.~n"),
					io:format("No moles here :-(~n")
			end,

			process_messages(SnakeName, Path, ServerNodeName);

		{input, {N1,N2}} ->
			Resp = gen_server:call({server, ServerNodeName}, {move_snake, SnakeName, {N1,N2}}),
		 
			case (Resp) of
				{error, {invalid_move, CurrentEdge, N}} ->
					io:format("Cannot move to ~p: {invalid_move,~p,~p}.~n", [N, CurrentEdge, N]),
					process_messages(SnakeName, Path, ServerNodeName);
				snake_move ->
					io:format("Snake ~p moved to path between hideouts #~p and #~p.~n", [SnakeName, N1, N2]),
					process_messages(SnakeName, {N1,N2}, ServerNodeName)
			end;

		{worm_moved, WormName, N} ->
			io:format("Worm ~p moved to ~p.~n", [WormName, N]);
		{mole_underneath_you, MoleName} ->
			io:format("A mole (~p) has passed below you!~n", [MoleName]);
		{mole_at_surface, MoleName, N} ->
			io:format("Mole ~p seen at position ~p~n", [MoleName, N]);
		{game_finished, Character, NameOfTheWinner} ->
			io:format("Sorry! The ~p ~p has won.~n", [Character, NameOfTheWinner])
	end,

	process_messages(SnakeName, Path, ServerNodeName).

