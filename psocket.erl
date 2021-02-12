-module(psocket).
-import(gen_tcp, [recv/2, send/2, close/1]).
-import(io, [format/2]).
-import(lists, [flatten/1]).
-export([psocket/3]).

% Recibe los pedidos del cliente y se los encarga al servidor con menos carga.
psocket(Socket, Username, Mailbox) ->
  {Atomo, CMD} = recv(Socket, 0),
  if
    Atomo /= ok -> format(">> Se ha producido un error en el cliente ~p. Conexion cerrada.~n", [Username]);
    true ->
      pbalance!{self(), where}, % Pregunta a pbalance en que servidor crear el pcomando
      receive
        {BestNode} ->
          % io:format(">> Usuario ~p ejecutado comando ~p en ~p.~n", [Username, CMD, BestNode]),
          spawn(BestNode, pcomando, pcomando, [Socket, Username, CMD, self(), Mailbox]), % Crea el pcomando en el servidor correspondiente

          % Espera la respuesta de pcomando
          receive
            {con, error} -> send(Socket, ">> Usted ya ha elegido un nombre de usuario."),
                            psocket(Socket, Username, Mailbox);
            {con, User} -> send(Socket, ">> Nombre de usuario aceptado.\n"),
                           psocket(Socket, User, Mailbox);
            {lsg, GameList} ->
                R = io_lib:format("~p ~n", [GameList]),
                    flatten(R),
                    send(Socket, [">> Lista de juegos: " | R]),
                    psocket(Socket, Username, Mailbox);
            {new, ok} -> send(Socket, ">> Partida creada corectamente.\n"),
                         psocket(Socket,Username,Mailbox);
            {new, error} -> send(Socket, ">> Error: usted ya es miembro de una partida.\n"),
                            psocket(Socket,Username,Mailbox);
            {acc, ok} -> send(Socket, ">> Usted se ha unido a la partida.\n"),
                         psocket(Socket,Username,Mailbox);
            {acc, error} -> send(Socket, ">> Error: no se ha podido unir a la partida.\n"),
                            psocket(Socket,Username,Mailbox);
            {obs, ok} -> send(Socket, ">> Usted esta observando la partida.\n"),
                         psocket(Socket,Username,Mailbox);
            {obs, error} -> send(Socket, ">> Error: no se puede observar esa partida.\n"),
                            psocket(Socket,Username,Mailbox);
            {pla, ok} -> psocket(Socket,Username,Mailbox);
            {pla, error} -> send(Socket, ">> Error: no se puede aceptar la jugada.\n"),
                            psocket(Socket,Username,Mailbox);
            {bye} -> send(Socket, ">> Adios!\n"), close(Socket);
            {error, noname} -> send(Socket, [">> Para comenzar envie CON y su nombre de usuario.\n"]), psocket(Socket,Username,Mailbox);
            {_, _} -> send(Socket, [">> Comando incorrecto.\n Comandos disponibles: CON, LSG, NEW, ACC, OBS, PLA, BYE.\n"]), psocket(Socket,Username,Mailbox)
          end
      end
  end.
