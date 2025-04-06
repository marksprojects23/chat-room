-module(simple_server).
-export([start/0, accept_connections/1, handle_client/1]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active, false}, {packet, 0}, {reuseaddr, true}]),
    io:format("Server started, listening on port 8080~n"),
    accept_connections(ListenSocket).

accept_connections(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    io:format("Client connected: ~p~n", [ClientSocket]),
    gen_tcp:send(ClientSocket, <<"Welcome to the chat server!">>),
    spawn(fun() -> handle_client(ClientSocket) end),
    accept_connections(ListenSocket).

handle_client(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            io:format("Received data: ~p~n", [Data]),
            gen_tcp:send(Socket, Data),
            inet:setopts(Socket, [{active, once}]),
            handle_client(Socket);
        {tcp_closed, Socket} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("Connection error: ~p~n", [Reason]),
            ok
    end.
