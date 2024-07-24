-module(chat_client).
-export([start_client/2, client_loop/1, send_message/2]).

start_client(IP, Port) ->
    io:format("Attempting to connect to ~s:~p~n", [IP, Port]),
    case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, once}]) of
        {ok, Socket} ->
            io:format("Connected to server, socket: ~p~n", [Socket]),
            spawn(fun() -> client_loop(Socket) end),
            Socket;
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
    end.

send_message(Socket, Message) ->
    gen_tcp:send(Socket, Message).
