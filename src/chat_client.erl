-module(chat_client).
-export([start/2, client_loop/2, send_message/2]).

start(IP, Port) ->
    io:format("Attempting to connect to ~s:~p~n", [IP, Port]),
    case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, true}]) of
        {ok, Socket} ->
            io:format("Connected to server, socket: ~p~n", [Socket]),
            Pid = spawn(fun() -> client_loop(Socket, self()) end),
            io:format("Client process started: ~p~n", [Pid]),
            Pid;
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason])
    end.

client_loop(Socket, Parent) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Message} ->
            io:format("Received from server: ~p~n", [Message]),
            inet:setopts(Socket, [{active, once}]),
            client_loop(Socket, Parent);
        {tcp_closed, Socket} ->
            io:format("Connection closed~n"),
            Parent ! {closed, self()};
        {tcp_error, Socket, Reason} ->
            io:format("Connection error: ~p~n", [Reason]),
            Parent ! {error, Reason};
        {send, Text} ->
            gen_tcp:send(Socket, Text),
            client_loop(Socket, Parent)
    end.

send_message(Pid, Text) ->
    Pid ! {send, Text}.
