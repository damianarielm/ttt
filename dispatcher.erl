-module(dispatcher).
-import(io, [fwrite/2]).
-import(gen_tcp, [accept/1, send/2]).
-export([dispatcher/1]).

dispatcher(ListenSocket) ->
    case accept(ListenSocket) of
        {error, Reason} ->
            fwrite(">> Se ha producido un error: ~p.~n", [Reason]);

        {ok, Socket} ->
            fwrite(">> Nuevo cliente: ~p.~n", [Socket]),
            send(Socket, ">> Bienvenido. Escriba 'CON username' para elegir"),
            send(Socket, "su nombre de usuario, o 'BYE' para salir.\n"),

            Mailbox = spawn(mailbox, mailbox, [Socket]),
            spawn(psocket, psocket, [Socket, nousername, Mailbox]),
            dispatcher(ListenSocket)
    end.
