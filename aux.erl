-module(aux).
-import(lists, [nth/2, map/2, member/2, sum/1, flatten/1]).
-export([winner/1, msgrest/2]).

% Envia un mensaje al resto de los nodos (excluyendo el emisor).
msgrest(Proceso, Mensaje) -> [{Proceso, Node}!Mensaje || Node <- nodes()].

% Matriz transpuesta.
transpose([ [] | _ ]) -> [];
transpose(M) -> [map(fun hd/1, M) | transpose(map(fun tl/1, M))].

% Determina que jugador gano (1/2), si hay empate (3) o nada de lo anterior (0).
winner(Tablero) ->
    [Fila1, Fila2, Fila3] = Tablero,
    [Columna1, Columna2, Columna3] = transpose(Tablero),

    Sumas = [ sum(Fila1), sum(Fila2), sum(Fila3) ] ++
            [ sum(Columna1), sum(Columna2), sum(Columna3) ] ++
            [ nth(1, Fila1) + nth(2, Fila2) + nth(3, Fila3) ] ++
            [ nth(1, Fila3) + nth(2, Fila2) + nth(3, Fila3) ],

    Ganador1 = member(9, Sumas),
    Ganador2 = member(27, Sumas),
    Vacios   = member(1, flatten(Tablero)),

    if
        Ganador1   -> 1;
        Ganador2   -> 2;
        not Vacios -> 3;
        true       -> 0
    end.

