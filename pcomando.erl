-module(pcomando).
-include("config.hrl").
-import(string, [trim/1, tokens/2]).
-import(io, [fwrite/2]).
-export([pcomando/5]).

% Realiza los pedidos del cliente.
pcomando(Socket, nousername, CMD, Who, _) ->
    case tokens(trim(CMD), " ") of
        ["BYE"] ->
            Who!{bye};
        ["CON", Username] ->
            {checkuser, node()}!{self(), Username},
            receive
                {ok} -> fwrite(">> Cliente ~p ahora se llama ~p.~n", [Socket, Username]),
                        Who!{con, Username};
                _    -> Who!{con, repeat}
            end;
        _ ->
            Who!{error, nousername}
    end;

pcomando(_, Username, CMD, Who, Mailbox) ->
    case tokens(trim(CMD), " ") of
        ["BYE"] ->
            gamelist!{bye, Username, Who};

        ["LSG"] ->
            {gamelist, node()}!{print, Who};

        ["CON", _] ->
            Who!{con, already};

        ["NEW"] ->
            {gamelist, node()}!{new, {Username, empty, ?TABLEROINICIAL, [Mailbox], Who, empty, true}};

        ["OBS", GameId] ->
            gamelist!{obs, GameId, Username, Who, Mailbox};

        ["ACC", GameId] ->
            gamelist!{acc, GameId, Username, Who, Mailbox};

        ["PLA", Play] ->
            gamelist!{pla, Play, Who};

        _ ->
            Who!{error, nocmd}
    end.
