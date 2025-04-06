-module(chat_server).
-export([start/0, accept/1, handle_client/2]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active, false}, {packet, 0}, {reuseaddr, true}]),
    io:format("Server started, listening on port 8080~n"),
    accept(ListenSocket).

accept(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    io:format("Client connected: ~p~n", [ClientSocket]),
    spawn(fun() -> handle_client(ClientSocket, []) end),
    accept(ListenSocket).

handle_client(Socket, Clients) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            lists:foreach(fun(Client) ->
                gen_tcp:send(Client, Data),
                io:format("Client sent: ~p~n", [Socket])
            end, [Socket | Clients]),
            handle_client(Socket, [Socket | Clients]);
        {tcp_closed, Socket} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("Connection error: ~p~n", [Reason]),
            handle_client(Socket, Clients)
    end.
