-module(chatroom).
-export([start/0, start_client/0, send_message/2]).

% Starts the server process
start() ->
    {ok, Socket} = gen_udp:open(8080, [binary, {active,true}]),
    io:format("UDP server listening on port ~p~n", [8080]),
    server(Socket, []).

% Starts the client process
start_client() ->
    {ok, Socket} = gen_udp:open(rand:uniform(64511)+1024, [binary, {active,true}]),
    gen_udp:send(Socket, {35,188,15,57}, 8080, "connect"),
    Socket.

% Server function to handle client connections and messages
server(ListenSocket, Clients) ->
    receive
        {udp, ListenSocket, IP, InPortNo, <<"connect">>} ->
            io:format("Received connection from: ~p~n", [IP]),
            server(ListenSocket, [{IP,InPortNo} | Clients]);
        {udp, ListenSocket, _IP, _InPortNo, Packet} ->
            io:format("Received packet: ~p~n", [Packet]),
            lists:foreach(fun({ClientIP, ClientPort}) ->
                gen_udp:send(ListenSocket, ClientIP, ClientPort, Packet),
                io:format("Sent message to ~p~n", [ClientIP])
                end, Clients),
            server(ListenSocket, Clients);
        {stop} ->
            gen_udp:close(ListenSocket),
            io:format("Server stopped~n")
    end.

% Function to send messages from client to server
send_message(Socket, Message) ->
    gen_udp:send(Socket, {35,188,15,57}, 8080, Message).
