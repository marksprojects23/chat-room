-module(simple_client).
-export([start_client/2, client/1]).

start_client(IP, Port) ->
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, true}]),
    spawn(fun() -> client(Socket) end),
    Socket.

client(Socket) ->
    receive
        {tcp, _, Message} ->
            io:format("Received from server: ~p~n", [Message]),
            inet:setopts(Socket, [{active, once}]),
            client(Socket);
        {tcp_closed, _} ->
            io:format("Connection closed~n");
        {tcp_error, _, Reason} ->
            io:format("Connection error: ~p~n", [Reason])
    end.
