-module(checkuser).
-import(aux, [msgrest/2]).
-import(lists, [member/2]).
-import(io, [format/2]).
-export([checkuser/1]).

% Maneja la lista de usuarios.
checkuser(UserList) ->
    receive
        % Imprime por terminal la lista de usuarios
        {print} -> format(">> Lista de usuarios: ~p", [UserList]),
                   checkuser(UserList);

        % Agrega un usuario a la lista
        {add, User} -> checkuser([User | UserList]);

        % Elimina un usuario local que sale del programa
        {del, User} -> NewList = [X || X <- UserList, X /= User],
                       msgrest(checkuser, {update, NewList}),
                       checkuser(NewList);

        % Actualiza la lista de usuarios
        {update, NewList} -> checkuser(NewList);

        % De ser posible, agrega un usuario
        {Who, User} ->
            case member(User, UserList) of
                true -> Who!{con, repeat},
                        checkuser(UserList);

                _    -> msgrest(checkuser, {add, User}),
                        Who!{ok},
                        checkuser([User | UserList])
            end
    end.
