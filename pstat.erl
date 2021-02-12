-module(pstat).
-include("config.hrl").
-import(erlang, [ports/0]).
-import(timer, [sleep/1]).
-export([pstat/0]).

% Funcion que calcula la carga del nodo.
load() -> length(ports()).

pstat() ->
    % Envia la carga actual, a los pbalance de todos los nodos
    [{pbalance, Node}!{node(), load()} || Node <- [node() | nodes()]],

    % Espera
    sleep(?SLEEP),

    % Comienza otra vez
    pstat().
