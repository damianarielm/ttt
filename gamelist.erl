-module(gamelist).
-import(io, [fwrite/2]).
-import(aux, [msgrest/2, winner/1, playertoascii/1]).
-import(lists, [keymember/3, keyfind/3, keyreplace/4, nth/2, nthtail/2,
                member/2, keydelete/3, flatten/1, sublist/2]).
-export([gamelist/1]).

postoindex(7) -> 1;
postoindex(8) -> 2;
postoindex(9) -> 3;
postoindex(4) -> 4;
postoindex(5) -> 5;
postoindex(6) -> 6;
postoindex(1) -> 7;
postoindex(2) -> 8;
postoindex(3) -> 9.

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
      % Busca el juego
      Game = case keyfind(Who, 5, GameList) of
               false -> keyfind(Who, 6, GameList);
               X    -> X
             end,

      case Game of
        % Juego incompleto
        {_, _, _, _, _, empty, _} ->
          Who!{pla, empty},
          gamelist(GameList);

        % No se encontro el juego
        false ->
          Who!{pla, notfound},
          gamelist(GameList);

        {A, B, Tablero, Observadores, Local, Visitante, Turno} ->
          if
            % Turno correcto
            ((Local == Who) and Turno) or ((Visitante == Who) and (not Turno)) ->
              Pos = nth(1, Play) - hd("0"),
              Temp = flatten(Tablero),
              case nth(postoindex(Pos), Temp) of
                % Casillero libre
                1 ->
                  [C7, C8, C9, C4, C5, C6, C1, C2, C3]  =
                    sublist(Temp, postoindex(Pos) - 1) ++
                    [case Turno of true -> 3; _ -> 9 end] ++
                    nthtail(postoindex(Pos), Temp),
                  NewBoard = [[C7, C8, C9], [C4, C5, C6], [C1, C2, C3]],
                  NewGame = {A, B, NewBoard, Observadores, Local, Visitante, not Turno},
                  NewGameList = keyreplace(A, 1, GameList, NewGame),

                  [Mailbox!{pla, ok, NewBoard} || Mailbox <- Observadores],
                  msgrest(gamelist, {acttab, NewGameList}),
                  Who!{pla, ok},
                  case winner(NewBoard) of
                    1 ->
                      [Mailbox!{fin1, NewBoard} || Mailbox <- Observadores],
                      gamelist(keydelete(A, 1, GameList));
                    2 ->
                      [Mailbox!{fin2, NewBoard} || Mailbox <- Observadores],
                      gamelist(keydelete(A, 1, GameList));
                    3 ->
                      [Mailbox!{emp, NewBoard} || Mailbox <- Observadores],
                      gamelist(keydelete(A, 1, GameList));
                    _ ->
                      gamelist(NewGameList)
                  end;

                % Casillero ocupado
                _ ->
                  Who!{pla, ocupado},
                  gamelist(GameList)
              end;

            % Turno incorrecto
            true ->
              Who!{pla, turn},
              gamelist(GameList)
          end
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
