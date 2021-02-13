-module(gamelist).
-import(io, [fwrite/2]).
-import(aux, [msgrest/2, winner/1]).
-import(lists, [keymember/3, keyfind/3, keyreplace/4, member/2, keydelete/3]).
-export([gamelist/1]).

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
    {print} -> fwrite(">> Lista de partidas: ~p", [GameList]),
               gamelist(GameList);

    % Informa al jugador la lista de juegos.
    {print, Who} -> Who!{lsg, [{X, Y} || {X, Y, _, _, _, _, _} <- GameList]},
                    gamelist(GameList);

    % Agrega a la lista, un juego creado en otro servidor.
    {newnode, Game} -> gamelist([Game | GameList]);

    % Agrega un juego a la lista local.
    {new, Game} ->
      {Local, _, _, _, Who, _, _} = Game,
      case keymember(Local, 1, GameList) or keymember(Local, 2, GameList) of
        false ->
          msgrest(gamelist, {newnode, Game}), % Avisa el cambio a los demas nodos
          Who!{new, ok}, % Avisa que salio todo bien
          gamelist([Game | GameList]); % Agrega el juego
        _ ->
          Who!{new, error},
          gamelist(GameList) % El creador ya ha creado una partida
      end;

    % Actualiza un tablero modificado en otro servidor.
    {acttab, NewList} -> gamelist(NewList);

    % Borra al usuario que se retira, de donde sea necesario.
    {bye, Username, Who} ->
      checkuser!{del, Username}, % Borra al usuario de la lista de usuarios
      Who!{bye}, % Avisa al usuario de que puede retirarse

      % Notifica a los observadores
      {X, Y} = {keyfind(Who, 5, GameList), keyfind(Who, 6, GameList)},
      case X of
          false ->
              case Y of
                {_, _, _, Observadores, _, _, _} ->
                  [Mailbox!{bye} || Mailbox <- Observadores];
                _ -> undefined
              end;

          _ ->
              {_, _, _, Observadores, _, _, _} = X,
              [Mailbox!{bye} || Mailbox <- Observadores]
      end,

      NewList = keydelete(Who, 6, keydelete(Who, 5, GameList)),
      gamelist(NewList);

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
            NewBoard = [[J1, J2, J3], [J4, J5, J6], [J7, J8, J9]],
            [Mailbox!{pla,ok,NewBoard} || Mailbox <- Observadores],
            NewList = [{A,B,if (E == Who) or (F == Who) -> NewBoard; true -> C end,D,E,F,if (E == Who) or (F == Who) -> not G; true -> G end} || {A,B,C,D,E,F,G} <- GameList],
            [{gamelist, Node}!{acttab, NewList} || Node <- nodes()],
            Result = winner(NewBoard),
            FinList = [{A, B, C, D, E, F, G} || {A, B, C, D, E, F, G} <- GameList, (E /= Creador) ],
            if
              Result == 0 -> Who!{pla, ok},
                             gamelist(NewList);
              Result == 1 -> [Mailbox!{fin1, NewBoard} || Mailbox <- Observadores], Who!{pla, ok},
                             gamelist(FinList);
              Result == 2 -> [Mailbox!{fin2, NewBoard} || Mailbox <- Observadores], Who!{pla, ok},
                             gamelist(FinList);
              Result == 3 -> [Mailbox!{emp, NewBoard} || Mailbox <- Observadores], Who!{pla, ok},
                             gamelist(FinList)
            end
        end;

      true -> Who!{pla,error}, gamelist(GameList)
    end;

  % Actualiza la lista cuando se une un observador en otro servidor.
  {newobs, NewList} -> gamelist(NewList);

  % Intenta unirse un observador.
  {obs, GameId, Username, Who, Mailbox} ->
      Game = keyfind(GameId, 1, GameList),
      case Game of
        false -> Who!{obs, notfound}, gamelist(GameList);

        {A, B, C, D, E, F, G} ->
          case (Username == A) or (Username == B) or (member(G, D)) of
              true -> Who!{obs, error}, gamelist(GameList);

              _ ->
                Who!{obs, ok},
                NewGame = {A, B, C, [Mailbox] ++ D, E, F, G},
                NewList = keyreplace(GameId, 1, GameList, NewGame),
                msgrest(gamelist, {newobs, NewList}),
                gamelist(NewList)
          end
      end;

  % Actualiza la lista cuando se une un jugador en otro servidor.
  {actlist, NewList} -> gamelist(NewList);

  % Intenta unirse a una partida.
  {acc, GameId, Username, Who, Mailbox} ->
      Game = keyfind(GameId, 1, GameList),
      case Game of
        false -> Who!{acc, notfound}, gamelist(GameList);

        {A, empty, C, D, E, empty, G} ->
          case Username == A of
            true -> Who!{acc, error}, gamelist(GameList);
            _ ->
              Who!{acc, ok},
              NewGame = {A, Username, C, [Mailbox] ++ D, E, Who, G},
              NewList = keyreplace(GameId, 1, GameList, NewGame),

              msgrest(gamelist, {actlist, NewList}),
              [Mailboxes!{acc, Username, GameId} || Mailboxes <- D],

              gamelist(NewList)
           end;

        _  -> Who!{acc, full}, gamelist(GameList)
      end
    end.
