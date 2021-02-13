-module(psocket).
-import(gen_tcp, [recv/2, send/2, close/1]).
-import(io, [fwrite/2]).
-export([psocket/3]).

% Recibe los pedidos del cliente y se los encarga al servidor con menos carga.
psocket(Socket, Username, Mailbox) ->
    case recv(Socket, 0) of
        {error, Reason} ->
            fwrite("Se ha producido un error en el cliente ~p: ~p.~n",
                   [Username, Reason]);

        {ok, CMD} ->
            pbalance!{self(), where},
            receive
                {BestNode} ->
                    fwrite(">> Usuario ~p ejecutado comando ~p en ~p.~n",
                           [Username, CMD, BestNode]),
                    spawn(BestNode, pcomando, pcomando,
                          [Socket, Username, CMD, self(), Mailbox]),

                    receive
                        PcomandoAnswer ->
                            respond_to_user(PcomandoAnswer, Socket, Username, Mailbox)
                    end
            end
    end.

respond_to_user(PcomandoAnswer, Socket, Username, Mailbox) ->
    case (PcomandoAnswer) of
        {con, already} ->
            send(Socket, ">> Usted ya ha elegido un nombre de usuario.\n"),
            psocket(Socket, Username, Mailbox);

        {con, repeat} ->
            send(Socket, ">> Ese nombre de usuario se encuentre en uso.\n"),
            psocket(Socket, Username, Mailbox);

        {con, User} ->
            send(Socket, ">> Nombre de usuario aceptado.\n"),
            psocket(Socket, User, Mailbox);

        {lsg, GameList} ->
            R = io_lib:format("~p ~n", [GameList]),
            send(Socket, [">> Lista de juegos: " | R]),
            psocket(Socket, Username, Mailbox);

        {new, ok} ->
            send(Socket, ">> Partida creada corectamente.\n"),
            psocket(Socket, Username, Mailbox);

        {new, error} ->
            send(Socket, ">> Error: usted ya es miembro de una partida.\n"),
            psocket(Socket, Username, Mailbox);

        {acc, ok} ->
            send(Socket, ">> Usted se ha unido a la partida.\n"),
            psocket(Socket, Username, Mailbox);

        {acc, full} ->
            send(Socket, ">> Error: La partida esta llena.\n"),
            psocket(Socket, Username, Mailbox);

        {acc, notfound} ->
            send(Socket, ">> Error: no se encuentra esa partida.\n"),
            psocket(Socket, Username, Mailbox);

        {acc, error} ->
            send(Socket, ">> Error: Usted ha creado esa partida.\n"),
            psocket(Socket, Username, Mailbox);

        {obs, ok} ->
            send(Socket, ">> Observando la partida.\n"),
            psocket(Socket, Username, Mailbox);

        {obs, error} ->
            send(Socket, ">> Error: usted ya esta observando esa partida.\n"),
            psocket(Socket, Username, Mailbox);

        {obs, notfound} ->
            send(Socket, ">> Error: no se encuentra esa partida.\n"),
            psocket(Socket, Username, Mailbox);

        {pla, ok} ->
            psocket(Socket, Username, Mailbox);

        {pla, error} ->
            send(Socket, ">> Error: no se puede aceptar la jugada.\n"),
            psocket(Socket,Username,Mailbox);

        {bye} ->
            send(Socket, ">> Adios!\n"),
            close(Socket);

        {error, nousername} ->
            send(Socket, ">> Para comenzar envie CON y su nombre de usuario.\n"),
            psocket(Socket, Username, Mailbox);

        {error, nocmd} ->
            send(Socket, ">> Comando incorrecto.\n"),
            send(Socket, "Comandos disponibles: CON, LSG, NEW, ACC, OBS, PLA, BYE.\n"),
            psocket(Socket, Username, Mailbox);

        true ->
            send(Socket, ">> Error inesperado.\n"),
            close(Socket)
    end.
