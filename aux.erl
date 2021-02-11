-module(aux).
-export([winner/1, boardtoascii/1]).

% Matriz transpuesta
transpose([ [] | _ ]) -> [];
transpose(M) -> [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

% Determina si que jugador gano (1/2), si hay empate (3) o nada de lo anterior (0).
winner(Tablero) ->
  Sumas = [ lists:sum(lists:nth(X, Tablero)) || X <- [1, 2, 3] ] ++ % Sumas por filas
          [ lists:sum(lists:nth(X, transpose(Tablero))) || X <- [1, 2, 3] ] ++ % Sumas por columnas
          [ lists:nth(1, lists:nth(1, Tablero)) + lists:nth(2, lists:nth(2, Tablero)) + lists:nth(3, lists:nth(3, Tablero)) ] ++
          [ lists:nth(1, lists:nth(3, Tablero)) + lists:nth(2, lists:nth(2, Tablero)) + lists:nth(3, lists:nth(1, Tablero)) ],

  Ganador1 = lists:member(9, Sumas),
  Ganador2 = lists:member(27, Sumas),
  Empate   = lists:member(1, lists:nth(1, Tablero)) or
             lists:member(1, lists:nth(2, Tablero)) or
             lists:member(1, lists:nth(3, Tablero)), % Casilleros disponibles

  if
    Ganador1 -> 1;
    Ganador2 -> 2;
    not Empate -> 3;
    true -> 0
  end.

playertoascii(Numero) ->
  case Numero of
    1 -> 46; % ASCII '.'
    3 -> 79; % ASCII 'O'
    9 -> 88  % ASCII 'X'
  end.

% Transforma un tablero de jugadas, en caracteres imprimibles
boardtoascii(Tablero) -> [lists:map(fun playertoascii/1, Fila) || Fila <- Tablero].
