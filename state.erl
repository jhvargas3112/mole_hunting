-module(state).
-include("game_state.hrl").
-export([state_init/1, new_mole/3, new_snake/3, new_worm/3, remove_mole/2,
	remove_snake/2, remove_worm/2, move_mole/3, move_snake/3, move_worm/3,
	get_mole_position/2, get_snake_position/2, get_worm_position/2,
	get_moles_at/2, get_snakes_at/2, get_worms_at/2, get_adjacent_hideouts/2,
	get_adjacent_paths/2, has_mole_trace/2, is_mole_at_own_hideout/2,
	is_mole_full/2, set_mole_full/2]).

state_init(Graph) ->
	#game_state{g=Graph}.

new_mole(Name, N, State) ->
	Moles = State#game_state.moles,
	State#game_state{moles = [#mole{name=Name,base_hideout=N,current_hideout=N}|Moles]}.

new_snake(Name, Edge, State) ->
	Snakes = State#game_state.snakes,
	State#game_state{snakes = [#snake{name=Name,current_edge=Edge}|Snakes]}.

new_worm(Name, N, State) ->
	Worms = State#game_state.worms,
	State#game_state{worms = [#worm{name=Name,current_hideout=N}|Worms]}.

remove_mole(Name, State) ->
	Mole = lists:keyfind(Name, #mole.name, State#game_state.moles),

	if (Mole =/= false) ->
		State#game_state{moles=lists:delete(Mole, State#game_state.moles)};
	   true ->
		State
	end.

remove_snake(Name, State) ->
	Snake = lists:keyfind(Name, #snake.name, State#game_state.snakes),

	if (Snake =/= false) ->
		State#game_state{snakes=lists:delete(Snake, State#game_state.snakes)};
	   true ->
		State
	end.

remove_worm(Name, State) ->
	Worm = lists:keyfind(Name, #worm.name, State#game_state.worms),

	if (Worm =/= false) ->
		State#game_state{worms=lists:delete(Worm, State#game_state.worms)};
	   true ->
		State
	end.

move_mole(Name, N, State) ->
	Mole = lists:keyfind(Name, #mole.name, State#game_state.moles),
	
	if (Mole =/= false) ->
		case (graph:is_adjacent_vertex(N, Mole#mole.current_hideout, State#game_state.g)) of
			true ->
				Moles = State#game_state.moles,
				VisitedHideouts = State#game_state.visited_hideouts,

				State#game_state{moles=lists:keyreplace(Name, #mole.name, Moles, Mole#mole{current_hideout=N}),
						  visited_hideouts = sets:add_element(N, VisitedHideouts)};
			false ->
				{error, {no_valid_move, Mole#mole.current_hideout, N}}
		end;
	   true ->
		{error, {no_valid_move, Mole#mole.current_hideout, N}}
	end.

move_snake(Name, Edge, State) ->
	Snake = lists:keyfind(Name, #snake.name, State#game_state.snakes),
	
	if (Snake =/= false) ->
		case graph:is_adjacent_edge(Edge, Snake#snake.current_edge, State#game_state.g) of
			true ->
				Snakes = State#game_state.snakes,
				State#game_state{snakes=lists:keyreplace(Name, #snake.name, Snakes, Snake#snake{current_edge=Edge})};
			false ->
				{error, {invalid_move, Snake#snake.current_edge, Edge}}
		end;
	   true ->
		{error, {invalid_move, Snake#snake.current_edge, Edge}}
	end.

move_worm(Name, N, State) ->
	Worm = lists:keyfind(Name, #worm.name, State#game_state.worms),
	if (Worm =/= false) ->
		case (graph:is_adjacent_vertex(N, Worm#worm.current_hideout, State#game_state.g)) of
			true ->
				Worms = State#game_state.worms,
				State#game_state{worms=lists:keyreplace(Name, #worm.name, Worms, Worm#worm{current_hideout=N})};
			false ->
				State
		end;
	   true ->
		State
	end.

get_mole_position(Name, State) ->
	Mole = lists:keyfind(Name, #mole.name, State#game_state.moles),

	if (Mole =/= false) ->
		Mole#mole.current_hideout
	end.

get_snake_position(Name, State) ->
	Snake = lists:keyfind(Name, #snake.name, State#game_state.snakes),

	if (Snake =/= false) ->
		Snake#snake.current_edge
	end.

get_worm_position(Name, State) ->
	Worm = lists:keyfind(Name, #worm.name, State#game_state.worms),

	if (Worm =/= false) ->
		Worm#worm.current_hideout
	end.

get_moles_at(N, State) ->
	lists:filtermap(fun(Mole) ->
				if (Mole#mole.current_hideout =:= N) ->
					{true, Mole#mole.name};
				   true ->
					false
				end
			end,
			State#game_state.moles
	).

get_snakes_at(Edge, State) ->
	lists:filtermap(fun(Snake) ->
				if (Snake#snake.current_edge =:= Edge) ->
					{true, Snake#snake.name};
				   true ->
					false
				end
			end,
			State#game_state.snakes
	).

get_worms_at(N, State) ->
	lists:filtermap(fun(Worm) ->
				if (Worm#worm.current_hideout =:= N) ->
					{true, Worm#worm.name};
				   true ->
					false
				end
			end,
			State#game_state.worms
	).

get_adjacent_hideouts(N, State) ->
	graph:adjacent_vertices_to(N, State#game_state.g).

get_adjacent_paths(P, State) ->
	graph:adjacent_edges_to(P, State#game_state.g).

has_mole_trace(N, State) ->
	sets:is_element(N, State#game_state.visited_hideouts).

is_mole_at_own_hideout(Name, State) ->
	Mole = lists:keyfind(Name, #mole.name, State#game_state.moles),

	Mole#mole.current_hideout =:= Mole#mole.base_hideout.

is_mole_full(Name, State) ->
	Mole = lists:keyfind(Name, #mole.name, State#game_state.moles),

	Mole#mole.has_eaten.

set_mole_full(Name, State) ->
	Mole = lists:keyfind(Name, #mole.name, State#game_state.moles),
	Moles = State#game_state.moles,

	State#game_state{moles=lists:keyreplace(Name, #mole.name, Moles, Mole#mole{has_eaten=true})}.
