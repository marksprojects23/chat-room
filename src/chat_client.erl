-module(chat_client).
-export([start_client/2]).

start_client(IP, Port) ->
    io:format("Attempting to connect to ~s:~p~n", [IP, Port]),
    case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, true}]) of
        {ok, Socket} ->
            io:format("Connected to server, socket: ~p~n", [Socket]),
            client_loop(Socket);
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason])
    end.

client_loop(Socket) ->
    receive
        {tcp, Socket, Message} ->
            io:format("Received from server: ~p~n", [Message]),
            inet:setopts(Socket, [{active, once}]),
            client_loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Connection closed~n");
        {tcp_error, Socket, Reason} ->
            io:format("Connection error: ~p~n", [Reason])
    after 5000 ->
        io:format("Timeout waiting for message~n"),
        client_loop(Socket)
    end.
