% Implementación de un grafo no dirigido, utilizando el módulo digraph.

-module(graph).
-export([new/0, add_vertex/2, has_vertex/2, has_edge/2, add_edge/3, vertices/1, 
		edges/1, is_adjacent_vertex/3, adjacent_vertices_to/2, is_adjacent_edge/3,
		adjacent_edges_to/2]).

new() ->
	digraph:new([protected]).

add_vertex(V, G) ->
	digraph:add_vertex(G, V, V).

has_vertex(V, G) ->
	lists:member(V, digraph:vertices(G)).

has_edge(E, G) ->
	{V1, V2} = E,
	Edges = graph:edges(G),

	lists:member({V1, V2}, Edges)
	orelse
	lists:member({V2, V1}, Edges).

add_edge(V1, V2, G) ->
	digraph:add_edge(G, V1, V2, {V1,V2}),
	digraph:add_edge(G, V2, V1, {V1,V2}).

vertices(G) ->
	digraph:vertices(G).

edges(G) ->
	sets:to_list(
		sets:from_list(
			lists:map(
				fun(E) ->
					{_, _, _, ELabel} = digraph:edge(G, E),
					ELabel
				end,
				digraph:edges(G)
			)
		)
	).

% Dos vértices son adyacentes si tienen una arista que los une.
is_adjacent_vertex(V1, V2, G) ->
	has_edge({V1, V2}, G).

adjacent_vertices_to(V, G) ->
	lists:filter(fun(W) -> is_adjacent_vertex(V, W, G) end, vertices(G)).

% O(n).
is_adjacent_edge(E1, E2, G) ->
	case (has_edge(E1, G)) of
		true ->
			AdjacentEdges = adjacent_edges_to(E2, G),
			{V1, V2} = E1,
			lists:member({V1, V2}, AdjacentEdges)
			orelse
			lists:member({V2, V1}, AdjacentEdges);
		false ->
			false
	end.

% O(n).
adjacent_edges_to(E, G) ->
	case (graph:has_edge(E, G)) of
		true ->
			[AdjacentEdge ||
			 AdjacentEdge <- [Ex ||
					  Ex <- edges(G),
					  has_common_verterx(E, Ex)
					 ]
			];
		false ->
			[]
	end.

% O(1).
% Dos aristas son adyacentes si tienen un vértice en común.
has_common_verterx(E1, E2) ->
	{Vu, Vw} = E1,

	if ({Vu, Vw} =/= E2 andalso {Vw, Vu} =/= E2) ->
		{Vx, Vy} = E2,
		((Vu =:= Vx) orelse (Vu =:= Vy))
		orelse
		((Vw =:= Vx) orelse (Vw =:= Vy));
	   true ->
		false
	end.


