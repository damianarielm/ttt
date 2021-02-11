-module(pstat).
-export([pstat/0]).

% Funcion que calcula la carga del nodo.
load() -> length(erlang:ports()).

pstat() ->
  % Envia la carga actual, a los pbalance de todos los nodos
  [{pbalance, Node}!{node(), load()} || Node <- [node() | nodes()]],

  % Espera 10 segundos
  timer:sleep(10000),

  % Comienza otra vez
  pstat().
