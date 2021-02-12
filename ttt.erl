
-module(ttt).
-include("config.hrl").
-import(io, [format/2]).
-import(gen_tcp, [listen/2]).
-import(inet, [port/1]).
-import(lists, [zip/2]).
-import(net_adm, [ping/1]).
-export([init/0]).

% Inicializa el servidor.
init() ->
  {Atomo, ListenSocket} = listen(0, [{active, false}]),
  if
    Atomo /= ok -> format(">> Se ha producido un error ", []);
    true ->
      {ok, Port} = port(ListenSocket), % Busca un puerto disponible
      [ping(Node) || Node <- ?SERVERS], % Reconoce a los otros nodos
      spawn(dispatcher, dispatcher, [ListenSocket]), % Lanza el dispatcher
      spawn(pstat , pstat, []), % Lanza el pstat
      register(gamelist, spawn(gamelist, gamelist, [[]])), % Lanza el gamelist
      register(checkuser, spawn(checkuser, checkuser, [[]])), % Lanza el registro de usuarios
      register(pbalance, spawn(pbalance, pbalance, [zip(?SERVERS, ?LOADS)])), % Lanza el pbalance
      format(">> Servidor ~p escuchando en puerto: ~p.~n>> Asegurese de iniciar el resto de los servidores antes de comenzar ", [node(), Port])
  end.
