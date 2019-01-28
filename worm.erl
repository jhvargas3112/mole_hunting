-module(worm).
-export([new_worm/3, worm/3]).

new_worm(Server, NombreGusano, Hideout) ->
	spawn(fun() ->
			Resp = gen_server:call(Server, {new_worm, NombreGusano, Hideout}),

			case (Resp) of
				{error, already_existing} ->
					io:format("Cannot create worm ~p: {already_existing,~p}~n", [NombreGusano, NombreGusano]);
				worm_created ->
					worm(Server, NombreGusano, Hideout)
			end
	      end
	).

worm(Server, NombreGusano, Hideout) ->
	receive
		{killed_by, NombreTopo} ->
			io:format("The worm ~p has been eaten by ~p~n", [NombreGusano, NombreTopo])
	after
		rand:uniform(15000 - 10000) + 5000 ->
			AdjacentHideouts = gen_server:call(Server, {adjacent_hideouts, Hideout}),
			NewHideout = lists:nth(rand:uniform(length(AdjacentHideouts)), AdjacentHideouts),
			
			gen_server:cast(Server, {move_worm, NombreGusano, NewHideout}),

			worm(Server, NombreGusano, NewHideout)

	end.
