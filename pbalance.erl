-module(pbalance).
-import(lists, [keysort/2, keyreplace/4]).
-import(io, [format/2]).
-export([pbalance/1]).

pbalance(LoadList) ->
    receive
        % Imprime por terminal la carga de los nodos
        {print} -> format("Balance de cargas: ~p", [LoadList]),
                   pbalance(LoadList);

        % Responde a un psocket, cual es el servidor con menor carga
        {Who, where} -> [{BestNode, _} | _ ] = keysort(2, LoadList),
                        Who!{BestNode},
                        pbalance(LoadList);

        % Recibe la informacion de un pstat y actualiza la lista
        {Node, Load} -> pbalance(keyreplace(Node, 1, LoadList, {Node, Load}))
    end.
