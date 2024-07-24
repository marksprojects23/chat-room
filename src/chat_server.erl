-module(chat_server).
-export([start/0, accept_connections/1, handle_client/2]).

-define(TCP_OPTIONS, [{active, false}, {packet, 0}, {reuseaddr, true}]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8080, ?TCP_OPTIONS),
    io:format("Server started, listening on port 8080~n"),
    accept_connections(ListenSocket).

accept_connections(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    io:format("Client connected: ~p~n", [ClientSocket]),
    inet:setopts(ClientSocket, [{active, once}]),
    spawn(fun() -> handle_client(ClientSocket, self()) end),
    accept_connections(ListenSocket).

handle_client(Socket, ServerPid) ->
    receive
        {tcp, Socket, Data} ->
            io:format("Received data: ~p~n", [Data]),
            ServerPid ! {broadcast, Socket, Data},
            inet:setopts(Socket, [{active, once}]),
            handle_client(Socket, ServerPid);
        {tcp_closed, Socket} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            ServerPid ! {remove_client, Socket};
        {tcp_error, Socket, Reason} ->
            io:format("Connection error: ~p~n", [Reason]),
            ServerPid ! {remove_client, Socket}
    end.

init() ->
    register(chat_server, self()),
    loop([]).

loop(Clients) ->
    receive
        {broadcast, From, Message} ->
            lists:foreach(fun(Client) ->
                if Client =/= From ->
                    gen_tcp:send(Client, Message);
                true -> ok
                end
            end, Clients),
            loop(Clients);
        {remove_client, Socket} ->
            NewClients = lists:delete(Socket, Clients),
            loop(NewClients);
        {new_client, Socket} ->
            loop([Socket | Clients])
    end.
