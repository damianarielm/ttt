-module(pcomando).
-include("config.hrl").
-import(string, [strip/3, tokens/2]).
-import(io, [format/2]).
-export([pcomando/5]).

% Realiza los pedidos del cliente.
pcomando(Socket, Username, CMD, Who, Mailbox) ->
  % io:format(">> Servidor ~p ejecutado comando ~p a peticion de ~p.~n", [node(), CMD, Username]),
  if Username == nousername ->
      case tokens(strip(strip(CMD, right, $\n), right, $\r), " ") of
        ["CON", User] ->
          if
            Socket /= Username -> Who!{con, error};
            true ->
              {checkuser, node()}!{self(), User},
              receive
                {ok} -> format(">> Cliente ~p ahora se llama ~p.~n", [Socket, User]), Who!{con, User};
                _ -> Who!{con, Username}
              end
          end;
        _ ->
          Who!{error, nousername}
       end;
    true ->
      case tokens(strip(strip(CMD, right, $\n), right, $\r), " ") of
        ["LSG"] -> {gamelist, node()}!{print, Who};
        ["NEW"] -> {gamelist, node()}!{new, Username, empty, ?TABLEROINICIAL, [Mailbox], Who};
        ["OBS", GameId] -> gamelist!{obs, GameId, Username, Who, Mailbox};
        ["ACC", GameId] -> gamelist!{acc, GameId, Username, Who, Mailbox};
        ["PLA", Play] -> gamelist!{pla,Play,Who};
        ["BYE"] -> gamelist!{bye, Username, Who};
        _ -> Who!{error, nocmd}
      end
  end.
