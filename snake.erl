-module(snake).
-export([rattly/0, lucy/0]).

rattly() ->
	init_snake(rattly, {2,8}).

lucy() ->
	init_snake(lucy, {14,15}).

init_snake(NombreSerpiente, Path) ->
	connect_to_game_server(NombreSerpiente, Path).

connect_to_game_server(NombreSerpiente, Path) ->
	io:format("Don't forget to write a dot (.) at the end of every input!~n"),
	{_, Input, _} = io:scan_erl_exprs("Node name of the server: "),
	{Status, ServerNodeName} = erl_parse:parse_term(Input),

	case(Status) of
		ok ->
			Self = spawn_link(fun()->
						Resp = gen_server:call({server, ServerNodeName}, {new_snake, NombreSerpiente, Path}),
						input:show_snake_game_controls(),

						case (Resp) of
							{error, already_existing} ->
								io:format("~nCannot create snake ~p: {already_existing, ~p}.~n", [NombreSerpiente, NombreSerpiente]);
							snake_created ->
								io:format("~p starting at #~p~n", [NombreSerpiente, Path]),
								process_messages(NombreSerpiente, Path, ServerNodeName)
						end
					  end
			       ),

			input:input_loop(NombreSerpiente, Self);
		error ->
			io:format("A valid Erlang atom is expected: ~w~n",[ServerNodeName]),
			connect_to_game_server(NombreSerpiente, Path)
	end.

process_messages(NombreSerpiente, Path, ServerNodeName) ->
	receive
		{input, a} ->
			AdjacentPaths = gen_server:call({server, ServerNodeName}, {adjacent_paths, Path}),
			io:format("Adjacent paths: ~p.~n", [AdjacentPaths]);
		{input, {s,N}} ->
			Resp = gen_server:call({server, ServerNodeName}, {sniff, NombreSerpiente, N}),

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

			process_messages(NombreSerpiente, Path, ServerNodeName);

		{input, {N1,N2}} ->
			Resp = gen_server:call({server, ServerNodeName}, {move_snake, NombreSerpiente, {N1,N2}}),
		 
			case (Resp) of
				{error, {invalid_move, CurrentEdge, N}} ->
					io:format("Cannot move to ~p: {invalid_move,~p,~p}.~n", [N, CurrentEdge, N]),
					process_messages(NombreSerpiente, Path, ServerNodeName);
				snake_move ->
					io:format("Snake ~p moved to path between hideouts #~p and #~p.~n", [NombreSerpiente, N1, N2]),
					process_messages(NombreSerpiente, {N1,N2}, ServerNodeName)
			end;

		{worm_moved, NombreGusano, N} ->
			io:format("Worm ~p moved to ~p.~n", [NombreGusano, N]);
		{mole_underneath_you, NombreTopo} ->
			io:format("A mole (~p) has passed below you!~n", [NombreTopo]);
		{mole_at_surface, NombreTopo, N} ->
			io:format("Mole ~p seen at position ~p~n", [NombreTopo, N]);
		{game_finished, Character, NameOfTheWinner} ->
			io:format("Sorry! The ~p ~p has won.~n", [Character, NameOfTheWinner])
	end,

	process_messages(NombreSerpiente, Path, ServerNodeName).

