-module(ttt).
-include("config.hrl").
-import(io, [fwrite/2]).
-import(gen_tcp, [listen/2]).
-import(inet, [port/1]).
-import(lists, [zip/2]).
-import(net_adm, [ping/1]).
-export([init/0]).

% Inicializa el servidor.
init() ->
    case listen(0, [{active, false}]) of
        {error, Reason} ->
            fwrite(">> Se ha producido un error: ~p.~n", [Reason]);

        {ok, ListenSocket} ->
            case port(ListenSocket) of
                {error, _} ->
                    fwrite(">> Se ha producido un error.~n", []);

                {ok, Port} ->
                    fwrite(">> Servidor ~p escuchando en puerto: ~p.~n", [node(), Port]),
                    fwrite(">> Asegurese de iniciar el resto de los servidores antes de comenzar.~n", []),

                    launch_processes(ListenSocket)
            end
    end.

% Lanza los procesos necesarios.
launch_processes(ListenSocket) ->
    spawn(dispatcher, dispatcher, [ListenSocket]),
    spawn(pstat, pstat, []),
    register(gamelist, spawn(gamelist, gamelist, [[]])),
    register(checkuser, spawn(checkuser, checkuser, [[]])),
    register(pbalance, spawn(pbalance, pbalance, [zip(?SERVERS, ?LOADS)])).
