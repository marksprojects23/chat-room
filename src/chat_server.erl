-module(chat_server).
-export([start/0, accept_connections/1, handle_client/1]).

-define(TCP_OPTIONS, [{active, false}, {packet, 0}, {reuseaddr, true}]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8080, ?TCP_OPTIONS),
    io:format("Server started, listening on port 8080~n"),
    accept_connections(ListenSocket).

accept_connections(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    io:format("Client connected: ~p~n", [ClientSocket]),
    spawn(fun() -> handle_client(ClientSocket) end),
    gen_tcp:send(ClientSocket, <<"Welcome to the chat server!">>),
    accept_connections(ListenSocket).

handle_client(Socket) ->
    io:format("Setting socket to active once: ~p~n", [Socket]),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            io:format("Received data: ~p~n", [Data]),
            Result = gen_tcp:send(Socket, Data),
            io:format("Sent data result: ~p~n", [Result]),
            inet:setopts(Socket, [{active, once}]),
            handle_client(Socket);
        {tcp_closed, Socket} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("Connection error: ~p~n", [Reason]),
            ok
    end.
