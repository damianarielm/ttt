-module(mailbox).
-import(gen_tcp, [send/2]).
-import(lists, [map/2]).
-export([mailbox/1]).

% Transforma un tablero de jugadas, en caracteres imprimibles.
boardtoascii(Tablero) ->
    [map(fun aux:playertoascii/1, Fila) || Fila <- Tablero].

% Bandeja de entrada para cada usuario.
mailbox(Socket) ->
    receive
        {bye} ->
            send(Socket, ">> Un jugador ha abandonado la partida.\n");

        {acc, Username, GameId} ->
            send(Socket, ">> '"), send(Socket, Username),
            send(Socket, "' se ha unido a la partida '"),
            send(Socket, GameId), send(Socket, "'.\n");

        {pla, ok, Tablero} ->
            [Fila1, Fila2, Fila3] = boardtoascii(Tablero),
            send(Socket, ">> Se ha realizado una jugada.\n"),
            send(Socket, Fila1), send(Socket, "\n"),
            send(Socket, Fila2), send(Socket, "\n"),
            send(Socket, Fila3), send(Socket, "\n");

        {fin1, Tablero} ->
            [Fila1, Fila2, Fila3] = boardtoascii(Tablero),
            send(Socket, ">> La partida ha concluido. Gana el jugador O.\n"),
            send(Socket, Fila1), send(Socket, "\n"),
            send(Socket, Fila2), send(Socket, "\n"),
            send(Socket, Fila3), send(Socket, "\n");

        {fin2, Tablero} ->
            [Fila1, Fila2, Fila3] = boardtoascii(Tablero),
            send(Socket, ">> La partida ha concluido. Gana el jugador X.\n"),
            send(Socket, Fila1), send(Socket, "\n"),
            send(Socket, Fila2), send(Socket, "\n"),
            send(Socket, Fila3), send(Socket, "\n");

        {emp, Tablero} ->
            [Fila1, Fila2, Fila3] = boardtoascii(Tablero),
            send(Socket, ">> La partida ha concluido. Ha habido un empate.\n"),
            send(Socket, Fila1), send(Socket, "\n"),
            send(Socket, Fila2), send(Socket, "\n"),
            send(Socket, Fila3), send(Socket, "\n")
    end,
    mailbox(Socket).
