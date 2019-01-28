-module(input).
-export([show_mole_game_controls/0,
	show_snake_game_controls/0, input_loop/2]).

input_loop(NombreTopo, Pid) ->
	{_, Input, _} = io:scan_erl_exprs(lists:append(atom_to_list(NombreTopo), ">")),

	{Status, ErlangTerm} = erl_parse:parse_term(Input),

	case (Status) of
		ok ->
			if (is_atom(ErlangTerm) andalso ErlangTerm =:= q) ->
				ok;
			    true ->
				case (is_tuple(ErlangTerm)) of
					true ->
						if (size(ErlangTerm) == 2) ->
							{T1, T2} = ErlangTerm,
							if (T1 =:= s andalso is_integer(T2)) ->
								Pid ! {input, {s,T2}};
							    is_integer(T1) andalso is_integer(T2) ->
								Pid ! {input, {T1,T2}};
							    true -> ok
							end;
						true ->
							ok
						end;
					false ->
						if (is_integer(ErlangTerm)) ->
							Pid ! {input, ErlangTerm};
						    is_atom(ErlangTerm) ->
							case (ErlangTerm) of
								a ->
									Pid ! {input, a};
								s ->
									Pid ! {input, s};
								_ ->
									ok
							end;
						true -> 
							ok
						end
				end,
				input_loop(NombreTopo, Pid)
			end;
		error ->
			io:format("Wrong input. It must be an Erlang term\n"),
			input_loop(NombreTopo, Pid)
	end.

show_mole_game_controls() ->
	io:format("USAGE~n"),
	io:format("a         : Show adjacent hideouts~n"),
	io:format("<number>. : Show adjacent hideouts~n"),
	io:format("s         : Go to surface~n"),
	io:format("q         : Back to Erlang shell~n").

show_snake_game_controls() ->
	io:format("USAGE~n"),
	io:format("a.                   : Show adjacent paths~n"),
	io:format("{<number>,<number>}. : Go to path {<number>,<number>}~n"),
	io:format("{s, <number>}.       : Sniff at hideout <number>~n"),
	io:format("q.                   : Back to Erlang shell~n").
