-module(mole).
-export([moley/0, bob/0]).

moley() ->
	init_mole(moley, 17).

bob() ->
	init_mole(bob, 16).

init_mole(NombreTopo, Hideout) ->
	connect_to_game_server(NombreTopo, Hideout).

connect_to_game_server(NombreTopo, Hideout) ->
	io:format("Don't forget to write a dot (.) at the end of every input!~n"),
	{_, Input, _} = io:scan_erl_exprs("Node name of the server: "),
	{Status, ServerNodeName} = erl_parse:parse_term(Input),

	case(Status) of
		ok ->
			Self = spawn_link(fun()-> 
						Resp = gen_server:call({server, ServerNodeName}, {new_mole, NombreTopo, Hideout}),
						input:show_mole_game_controls(),

						case (Resp) of
							{error, already_existing} ->
								io:format("~nCannot create mole ~p: {already_existing, ~p}.~n", [NombreTopo, NombreTopo]);
							mole_created ->
								io:format("~p starting at #~p~n", [NombreTopo, Hideout]),
								process_messages(NombreTopo, Hideout, ServerNodeName)
						end
					  end
			       ),

			input:input_loop(NombreTopo, Self);
		error ->
			io:format("A valid Erlang atom is expected: ~w~n",[ServerNodeName]),
			connect_to_game_server(NombreTopo, Hideout)
	end.

process_messages(NombreTopo, Hideout, ServerNodeName) ->
	receive
		{input, a} ->
			AdjacentHideouts = gen_server:call({server, ServerNodeName}, {adjacent_hideouts, Hideout}),
			io:format("Adjacent hideouts: ~p.~n", [AdjacentHideouts]);
		{input, s} ->
			Resp = gen_server:call({server, ServerNodeName}, {go_surface, NombreTopo}),

			case (Resp) of
				{SnakePositions, WormName, true} ->
					io:format("Snake positions: ~p~n", [SnakePositions]),
					io:format("Caught a worm! (~p)~n", [WormName]),
					io:format("Congratulations! You won!~n");
				{SnakePositions, WormName} ->
					io:format("Snake positions: ~p~n", [SnakePositions]),
					io:format("Caught a worm! (~p)~n", [WormName]);
				SnakePositions ->
					io:format("Snake positions: ~p~n", [SnakePositions]),
					io:format("No worms here :-(~n")
			end;
		{input, N} ->
			if (is_integer(N)) ->
				Resp = gen_server:call({server, ServerNodeName}, {move_mole, NombreTopo, N}),

				case (Resp) of
					{error, {no_valid_move, CurrentHideout, N}} ->
						io:format("Mole ~p cannot move to ~p: {no_valid_move,~p,~p}.~n", [NombreTopo, N, CurrentHideout, N]),
						process_messages(NombreTopo, Hideout, ServerNodeName);
					mole_move ->
						io:format("~p moved to ~p.~n", [NombreTopo, N]),
						process_messages(NombreTopo, N, ServerNodeName);
					{mole_move, you_win} ->
						io:format("~p moved to ~p.~n", [NombreTopo, N]),
						io:format("Congratulations! You won!~n")
				end;
			   true ->
				process_messages(NombreTopo, Hideout, ServerNodeName)
			end;
		{worm_moved, NombreGusano, N} ->
			io:format("Worm ~p moved to ~p.~n", [NombreGusano, N]);
		{killed_by, NombreSerpiente} ->
			io:format("Ouch! You have been eaten by ~p.~n", [NombreSerpiente]);
		{snake_over_you, NombreSerpiente} ->
			io:format("Warning! Snake ~p has just passed above you!~n", [NombreSerpiente]);
		{snake_sniff, NombreSerpiente, N} ->
			io:format("Warning! Snake ~p has sniffed in hideout #~p nearby.~n", [NombreSerpiente, N]);
		{game_finished, Character, NameOfTheWinner} ->
			io:format("Sorry! The ~p ~p has won.~n", [Character, NameOfTheWinner])
	end,

	process_messages(NombreTopo, Hideout, ServerNodeName).


