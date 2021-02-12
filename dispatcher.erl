-module(dispatcher).
-import(io, [format/2]).
-import(gen_tcp, [accept/1]).
-export([dispatcher/1]).

dispatcher(ListenSocket) ->
  {ok, Socket} = accept(ListenSocket),
  format(">> Nuevo cliente: ~p.~n", [Socket]),
  Mailbox = spawn(mailbox, mailbox, [Socket]), % Crea la bandeja de entrada para el nuevo usuario.
  spawn(psocket, psocket, [Socket, Socket, Mailbox]),
  dispatcher(ListenSocket).
