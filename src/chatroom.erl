-module(chatroom).
-export([start_server/1, start_client/2, send_message/2]).

start_server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    spawn(fun() -> acceptor(ListenSocket, []) end).

acceptor(ListenSocket, Clients) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    gen_tcp:controlling_process(ClientSocket, self()),
    NewClients = [ClientSocket | Clients],
    spawn(fun() -> loop(NewClients) end),  % Updated to new loop/1 function
    acceptor(ListenSocket, NewClients).

loop(Clients) -> % Removed ListenSocket as it's not used
    receive
        {tcp, _Socket, Message} ->  % Use _Socket to indicate that it's intentionally unused
            lists:foreach(fun(Client) ->
                gen_tcp:send(Client, Message)
            end, Clients),
            loop(Clients)  % Recursive call without ListenSocket
    end.


start_client(IP, Port) ->
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, true}]),
    spawn(fun() -> client(Socket) end),
    Socket.

client(Socket) ->
    receive
        {tcp, _, Message} ->
            io:format("Received: ~p~n", [Message]),
            client(Socket)
    end.

send_message(Socket, Message) ->
    gen_tcp:send(Socket, Message).
