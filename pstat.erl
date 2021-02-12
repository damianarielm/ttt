-module(pstat).
-import(erlang, [ports/0]).
-import(timer, [sleep/1]).
-export([pstat/0]).

% Funcion que calcula la carga del nodo.
load() -> length(ports()).

pstat() ->
    % Envia la carga actual, a los pbalance de todos los nodos
    [{pbalance, Node}!{node(), load()} || Node <- [node() | nodes()]],

    % Espera 10 segundos
    sleep(10000),

    % Comienza otra vez
    pstat().
