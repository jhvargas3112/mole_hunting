-module(state).
-include("game_state.hrl").
-export([state_init/1, new_mole/3, new_snake/3, new_worm/3, remove_mole/2,
	remove_snake/2, remove_worm/2, move_mole/3, move_snake/3, move_worm/3,
	get_mole_position/2, get_snake_position/2, get_worm_position/2,
	get_moles_at/2, get_snakes_at/2, get_worms_at/2, get_adjacent_hideouts/2,
	get_adjacent_paths/2, has_mole_trace/2, is_mole_at_own_hideout/2,
	is_mole_full/2, set_mole_full/2]).

state_init(Grafo) ->
	#game_state{g=Grafo}.

new_mole(Nombre, N, Estado) ->
	Moles = Estado#game_state.moles,
	Estado#game_state{moles = [#mole{name=Nombre,base_hideout=N,current_hideout=N}|Moles]}.

new_snake(Nombre, Edge, Estado) ->
	Snakes = Estado#game_state.snakes,
	Estado#game_state{snakes = [#snake{name=Nombre,current_edge=Edge}|Snakes]}.

new_worm(Nombre, N, Estado) ->
	Worms = Estado#game_state.worms,
	Estado#game_state{worms = [#worm{name=Nombre,current_hideout=N}|Worms]}.

remove_mole(Nombre, Estado) ->
	Mole = lists:keyfind(Nombre, #mole.name, Estado#game_state.moles),

	if (Mole =/= false) ->
		Estado#game_state{moles=lists:delete(Mole, Estado#game_state.moles)};
	   true ->
		Estado
	end.

remove_snake(Nombre, Estado) ->
	Snake = lists:keyfind(Nombre, #snake.name, Estado#game_state.snakes),

	if (Snake =/= false) ->
		Estado#game_state{snakes=lists:delete(Snake, Estado#game_state.snakes)};
	   true ->
		Estado
	end.

remove_worm(Nombre, Estado) ->
	Worm = lists:keyfind(Nombre, #worm.name, Estado#game_state.worms),

	if (Worm =/= false) ->
		Estado#game_state{worms=lists:delete(Worm, Estado#game_state.worms)};
	   true ->
		Estado
	end.

move_mole(Nombre, N, Estado) ->
	Mole = lists:keyfind(Nombre, #mole.name, Estado#game_state.moles),
	
	if (Mole =/= false) ->
		case (graph:is_adjacent_vertex(N, Mole#mole.current_hideout, Estado#game_state.g)) of
			true ->
				Moles = Estado#game_state.moles,
				VisitedHideouts = Estado#game_state.visited_hideouts,

				Estado#game_state{moles=lists:keyreplace(Nombre, #mole.name, Moles, Mole#mole{current_hideout=N}),
						  visited_hideouts = sets:add_element(N, VisitedHideouts)};
			false ->
				{error, {no_valid_move, Mole#mole.current_hideout, N}}
		end;
	   true ->
		{error, {no_valid_move, Mole#mole.current_hideout, N}}
	end.

move_snake(Nombre, Edge, Estado) ->
	Snake = lists:keyfind(Nombre, #snake.name, Estado#game_state.snakes),
	
	if (Snake =/= false) ->
		case graph:is_adjacent_edge(Edge, Snake#snake.current_edge, Estado#game_state.g) of
			true ->
				Snakes = Estado#game_state.snakes,
				Estado#game_state{snakes=lists:keyreplace(Nombre, #snake.name, Snakes, Snake#snake{current_edge=Edge})};
			false ->
				{error, {invalid_move, Snake#snake.current_edge, Edge}}
		end;
	   true ->
		{error, {invalid_move, Snake#snake.current_edge, Edge}}
	end.

move_worm(Nombre, N, Estado) ->
	Worm = lists:keyfind(Nombre, #worm.name, Estado#game_state.worms),
	if (Worm =/= false) ->
		case (graph:is_adjacent_vertex(N, Worm#worm.current_hideout, Estado#game_state.g)) of
			true ->
				Worms = Estado#game_state.worms,
				Estado#game_state{worms=lists:keyreplace(Nombre, #worm.name, Worms, Worm#worm{current_hideout=N})};
			false ->
				Estado
		end;
	   true ->
		Estado
	end.

get_mole_position(Nombre, Estado) ->
	Mole = lists:keyfind(Nombre, #mole.name, Estado#game_state.moles),

	if (Mole =/= false) ->
		Mole#mole.current_hideout
	end.

get_snake_position(Nombre, Estado) ->
	Snake = lists:keyfind(Nombre, #snake.name, Estado#game_state.snakes),

	if (Snake =/= false) ->
		Snake#snake.current_edge
	end.

get_worm_position(Nombre, Estado) ->
	Worm = lists:keyfind(Nombre, #worm.name, Estado#game_state.worms),

	if (Worm =/= false) ->
		Worm#worm.current_hideout
	end.

get_moles_at(N, Estado) ->
	lists:filtermap(fun(Mole) ->
				if (Mole#mole.current_hideout =:= N) ->
					{true, Mole#mole.name};
				   true ->
					false
				end
			end,
			Estado#game_state.moles
	).

get_snakes_at(Edge, Estado) ->
	lists:filtermap(fun(Snake) ->
				if (Snake#snake.current_edge =:= Edge) ->
					{true, Snake#snake.name};
				   true ->
					false
				end
			end,
			Estado#game_state.snakes
	).

get_worms_at(N, Estado) ->
	lists:filtermap(fun(Worm) ->
				if (Worm#worm.current_hideout =:= N) ->
					{true, Worm#worm.name};
				   true ->
					false
				end
			end,
			Estado#game_state.worms
	).

get_adjacent_hideouts(N, Estado) ->
	graph:adjacent_vertices_to(N, Estado#game_state.g).

get_adjacent_paths(P, Estado) ->
	graph:adjacent_edges_to(P, Estado#game_state.g).

has_mole_trace(N, Estado) ->
	sets:is_element(N, Estado#game_state.visited_hideouts).

is_mole_at_own_hideout(Nombre, Estado) ->
	Mole = lists:keyfind(Nombre, #mole.name, Estado#game_state.moles),

	Mole#mole.current_hideout =:= Mole#mole.base_hideout.

is_mole_full(Nombre, Estado) ->
	Mole = lists:keyfind(Nombre, #mole.name, Estado#game_state.moles),

	Mole#mole.has_eaten.

set_mole_full(Nombre, Estado) ->
	Mole = lists:keyfind(Nombre, #mole.name, Estado#game_state.moles),
	Moles = Estado#game_state.moles,

	Estado#game_state{moles=lists:keyreplace(Nombre, #mole.name, Moles, Mole#mole{has_eaten=true})}.
