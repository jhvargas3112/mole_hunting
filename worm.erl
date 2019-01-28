-module(worm).
-export([new_worm/3, worm/3]).

new_worm(Server, WormName, Hideout) ->
	spawn(fun() ->
			Resp = gen_server:call(Server, {new_worm, WormName, Hideout}),

			case (Resp) of
				{error, already_existing} ->
					io:format("Cannot create worm ~p: {already_existing,~p}~n", [WormName, WormName]);
				worm_created ->
					worm(Server, WormName, Hideout)
			end
	      end
	).

worm(Server, WormName, Hideout) ->
	receive
		{killed_by, MoleName} ->
			io:format("The worm ~p has been eaten by ~p~n", [WormName, MoleName])
	after
		rand:uniform(15000 - 10000) + 5000 ->
			AdjacentHideouts = gen_server:call(Server, {adjacent_hideouts, Hideout}),
			NewHideout = lists:nth(rand:uniform(length(AdjacentHideouts)), AdjacentHideouts),
			
			gen_server:cast(Server, {move_worm, WormName, NewHideout}),

			worm(Server, WormName, NewHideout)

	end.
