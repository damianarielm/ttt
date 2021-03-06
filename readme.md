# Ta Te Ti #

## Descripcion ##

El presente trabajo es un servidor de *ta te ti* distribuido.

El servidor puede correrse en multiples instancias remotas que se comunican
entre si, logrando atender los pedidos de los clientes de manera descentralidaza;
de manera tal que cada peticion sea atendida por el sevidor que menos carga
tenga en el momento de realizarse por parte del cliente.

## Dependencias ##

Para poder compilar el programa deberá contar con un compilador de *Erlang*.
Si utiliza *Ubuntu*, puede instalarlo con el comando:

```shell
sudo apt install erlang
```

## Manual de uso ##

### Servidores ###

#### Configuracion ####

Para configurar su cluster de servidores deberá editar el archivo *config.hrl*
modificando la constante *SERVERS*.

La misma deberá ser una lista de servidores con el formato `'nombre@ip'`.

#### Compilación ####

Para compilar el servidor, deberá utilizar la siguiente instrucción:

```shell
erl -make
```

#### Ejecución ####

En cada host ejecute el comando `erl -name servidor`, donde `servidor` es uno
de los elementos configurados en la constante *SERVERS*.

Una vez iniciadas todas las consolas, ejecute en cada una de ellas la siguiente
instrucción:

```
ttt:init().
```

### Clientes ###

Este proyecto no cuenta con un cliente especializado, pero puede utilizar uno
genérico. Por ejemplo puede utilizar `telnet`.

Los comandos aceptados por el servidor son:

* CON *username*: Inicia la comunicación con el servidor.
* LSG: Lista los juegos disponibles.
* NEW: Crea una nueva partida.
* ACC *game*: Se une a la partida *game*.
* OBS *game*: Observa el juego *game*.
* BYE: Abandona el servidor.
* PLA *number*: Pone una ficha en el tablero.

## Arquitectura ##

La siguiente analogia pretente ilustrar el trafico de informacion entre los
diferentes procesos y servidores que intervienen en el cluster.

> El *cliente* (cliente) llega a la puerta de una *sucursal* (servidor)
> de su restaurant favorito en donde es atendido por el *recepcionista*
> (dispatcher). Este lo dirige a la mesa en donde sera antendido por un *mozo*
> (psocket).
>
> El *cliente* le indica su pedido al *mozo*, y este se dirige a hablar con el
> *supervisor* (pbalance). El *supervisor* le indica al *mozo* cual es la
> *sucursal* con menor carga de trabajo.
>
> El *mozo* se dirige a la *sucursal* indicada por su *supervisor* y le solicita
> al *cocinero* (pcomando) el pedido del *cliente*. Una vez listo el pedido,
> el *cocinero* le notifica al *mozo*, el cual a su vez notifica al *cliente*.
>
> Notese que para que el *supervisor* pueda indicarle al *mozo* cual es la
> *sucursal* con menor carga de trabajo, es necesario que se mantenga en contacto
> con los *telefonistas* (pstat) de todas las *sucursales*.
>
> Estos se encargan de informar a los *supervisores* de las diferentes
> *sucursales*, cual es la carga de trabajo del local.
