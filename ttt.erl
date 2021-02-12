
-module(ttt).
-include("config.hrl").
-import(aux, [winner/1, msgrest/2]).
-import(io, [format/2]).
-import(gen_tcp, [listen/2, recv/2, send/2, close/1]).
-import(inet, [port/1]).
-import(net_adm, [ping/1]).
-import(lists, [zip/2, flatten/1]).
-export([init/0, gamelist/1, psocket/3]).

% Inicializa el servidor.
init() ->
  {Atomo, ListenSocket} =listen(0, [{active, false}]),
  if
    Atomo /= ok -> format(">> Se ha producido un error ", []);
    true ->
      {ok, Port} = port(ListenSocket), % Busca un puerto disponible
      [ping(Node) || Node <- ?SERVERS], % Reconoce a los otros nodos
      spawn(dispatcher, dispatcher, [ListenSocket]), % Lanza el dispatcher
      spawn(pstat , pstat, []), % Lanza el pstat
      register(gamelist, spawn(?MODULE, gamelist, [[]])), % Lanza el gamelist
      register(checkuser, spawn(checkuser, checkuser, [[]])), % Lanza el registro de usuarios
      register(pbalance, spawn(pbalance, pbalance, [zip(?SERVERS, ?LOADS)])), % Lanza el pbalance
      format(">> Servidor ~p escuchando en puerto: ~p.~n>> Asegurese de iniciar el resto de los servidores antes de comenzar ", [node(), Port])
  end.

% GameList es una lista de tuplas
% { A: Nombre de usuario del creador de la partida,
%   B: Nombre de usuario del usuario que acepto la partida,
%   C: Tablero de la partida,
%   D: Lista de sockets de observadores,
%   E: Socket del usuario creador de la partida,
%   F: Socket del usuario del usuario que acepto la partida,
%   G: Booleano que indica a quien corresponde el turno (True = Local) }
gamelist(GameList) ->
  receive
    % Imprime por terminal la lista de juegos
    {print} -> format(">> Lista de partidas: ~p", [GameList]),
               gamelist(GameList);

    % Informa al jugador la lista de juegos.
    {print, Who} -> Who!{lsg, [{X,Y} || {X, Y, _, _, _, _, _} <- GameList]},
                    gamelist(GameList);

    % Agrega a la lista, un juego creado en otro servidor.
    {newnode, Local, Visitante, Tablero, Observadores, LocalId, VisId, Turno} ->
      gamelist([{Local, Visitante, Tablero, Observadores, LocalId, VisId, Turno} | GameList]);

    % Agrega un juego a la lista local.
    {new, Local, Visitante, Tablero, Observadores, Who} ->
      case [ {A, B, C, D, E, F, G} || {A, B, C, D, E, F, G} <- GameList, (A == Local) or (B == Local) ] of
      [] ->
            msgrest(gamelist,{newnode, Local, Visitante, Tablero, Observadores, Who, empty, true}), % Avisa el cambio a los demas nodos
            Who!{new, ok}, % Avisa que salio todo bien
            gamelist([{Local, Visitante, Tablero, Observadores, Who, empty, true} | GameList]); % Agrega el juego
          true -> Who!{new,error}, gamelist(GameList) % El creador ya ha creado una partida
      end;

    % Actualiza un tablero modificado en otro servidor.
    {acttab, NewList} -> gamelist(NewList);

    % Borra al usuario que se retira, de donde sea necesario.
    {bye,Username,Who} ->
      % TODO ESTO ES INENTENDIBLE.
      Temp = [ {A,B,C,D,E,F,G} || {A,B,C,D,E,F,G} <- GameList, ((E == Who) or (F == Who))],
      FinList = [{A,B,C,D,E,F,G} || {A,B,C,D,E,F,G} <- GameList, F /= Who],
      FinalList = [{A,B,C,D,E,F,G} || {A,B,C,D,E,F,G} <- FinList, E /= Who],

      checkuser!{del, Username}, % Borra al usuario de la lista de usuarios
      Who!{bye}, % Avisa al usuario de que puede retirarse

      % Si es necesario, notifica a los observadores
      if Temp /= [] ->
        [{_,  _, _, Observadores, _, _, _}] = Temp,
        [Mailbox!{bye} || Mailbox <- Observadores],
        gamelist(FinalList);
        true -> gamelist(FinalList)
      end;

    % Juega una jugada.
    {pla, Play, Who} ->
    Temp = [ {A, B, C, D, E, F, G} || {A, B, C, D, E, F, G} <- GameList, ((E == Who) and G) or ((F == Who) and (not G)) and (F /= empty) ],
    if
      Temp /= [] ->
        [{_, _, Tablero, Observadores, Creador, _, Turno}] = Temp,
        [Aux] = Play,
        Jugada = Aux - 48,
        % Fila = trunc((Jugada-1)/3) + 1,
        % Columna = ((Jugada-1) rem 3) + 1,
        [[P1, P2, P3], [P4, P5, P6], [P7, P8, P9]] = Tablero,
        TableroCool = [P1, P2, P3, P4, P5, P6, P7, P8, P9],
        % Pos = (lists:nth(Columna, lists:nth(Fila, Tablero))),
        if
          true -> % Casillero vacio
            if
              Turno -> [J1,J2,J3,J4,J5,J6,J7,J8,J9] = lists:sublist(TableroCool,Jugada-1) ++ [lists:nth(Jugada,TableroCool)+2] ++ lists:nthtail(Jugada,TableroCool);
              true -> [J1,J2,J3,J4,J5,J6,J7,J8,J9] =  lists:sublist(TableroCool,Jugada-1) ++ [lists:nth(Jugada,TableroCool)+8] ++ lists:nthtail(Jugada,TableroCool)
            end,
            NewBoard = [[J1,J2,J3],[J4,J5,J6],[J7,J8,J9]],
            [Mailbox!{pla,ok,NewBoard} || Mailbox <- Observadores],
            NewList = [{A,B,if (E == Who) or (F == Who) -> NewBoard; true -> C end,D,E,F,if (E == Who) or (F == Who) -> not G; true -> G end} || {A,B,C,D,E,F,G} <- GameList],
            [{gamelist, Node}!{acttab, NewList} || Node <- nodes()],
            Result = winner(NewBoard),
            FinList = [{A, B, C, D, E, F, G} || {A, B, C, D, E, F, G} <- GameList, (E /= Creador) ],
            if
              Result == 0 -> Who!{pla, ok}, gamelist(NewList);
              Result == 1 -> [Mailbox!{fin1,NewBoard} || Mailbox <- Observadores], Who!{pla, ok}, gamelist(FinList);
              Result == 2 -> [Mailbox!{fin2,NewBoard} || Mailbox <- Observadores], Who!{pla, ok}, gamelist(FinList);
              Result == 3 -> [Mailbox!{emp,NewBoard} || Mailbox <- Observadores], Who!{pla, ok}, gamelist(FinList)
            end
        end;
      true -> Who!{pla,error}, gamelist(GameList)
    end;

  % Actualiza la lista cuando se une un observador en otro servidor.
  {newobs, NewList} -> gamelist(NewList);

  % Intenta unirse un observador.
  {obs, GameId, Username, Who, Mailbox} ->
      Members = [ {A,B,C,D,E,F,G} || {A,B,C,D,E,F,G} <- GameList, A == GameId, A /= Username, B /= Username, not lists:member(Mailbox,D)],

      if
          Members /= [] ->
              Who!{obs, ok},
              NewList = [{A,B,C, case A of GameId -> [Mailbox] ++ D; _ -> D end,E,F,G} || {A,B,C,D,E,F,G} <- GameList],
              msgrest(gamelist,{newobs,NewList}),
              gamelist(NewList);
          true -> Who!{obs,error}, gamelist(GameList)
      end;

  % Actualiza la lista cuando se une un jugador en otro servidor.
  {actlist, NewList} -> gamelist(NewList);

  % Intenta unirse a una partida.
  {acc, GameId, Username, Who, Mailbox} ->
      Members = [ {A,B,C,D,E,F,G} || {A,B,C,D,E,F,G} <- GameList, A == GameId, B == empty, A /= Username ],

      if
          Members /= [] ->
              Who!{acc, ok},
              NewList = [{A, case A of GameId -> Username; _ -> B end,C,[Mailbox] ++ D,E, Who,G} || {A,B,C,D,E,_,G} <- GameList],
              [{gamelist, Node}!{actlist, NewList} || Node <- nodes()],
              [{_,_,_,Observadores,_,_,_}] = NewList,
              [Mailboxes!{acc,Username,GameId} || Mailboxes <- Observadores, Mailboxes /= Mailbox],
              gamelist(NewList);
          true -> Who!{acc,error}, gamelist(GameList)
      end
  end.

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
                R = format("~p ~n", [GameList]),
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
            {error,noname} -> send(Socket, [">> Para comenzar envie CON y su nombre de usuario.\n"]), psocket(Socket,Username,Mailbox);
            {_, _} -> send(Socket, [">> Comando incorrecto.\n Comandos disponibles: CON, LSG, NEW, ACC, OBS, PLA, BYE.\n"]), psocket(Socket,Username,Mailbox)
          end
      end
  end.
