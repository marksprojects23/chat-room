% -module(chatroom).
% -compile([start_server/0, server/1, start_client/1, client/1, send_message/2]).

% start_server() ->
%     spawn(fun() -> server([]) end),
%     erlang:set_cookie(node(), cookie1). % Starts the server process.

% start_client(Server) ->
%     spawn(fun() -> client(Server) end),
%     erlang:set_cookie(node(), cookie1). % Starts a client process.

% start_server(Port) ->
%     {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, true}]),
%     spawn(fun() -> server([], ListenSocket) end),
%     erlang:set_cookie(node(), cookie1).

% start_client(IP, Port) ->
%     {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 4}, {active, true}]),
%     spawn(fun() -> client(Socket) end),
%     erlang:set_cookie(node(), cookie1).

% server(Clients) ->                      % Pass in the list of clients to the server.
%     receive                             % When the server receives any of these,
%         {connect, Client} ->            % for a connecting client,
%             server([Client | Clients]); % append the client's PID to the list of clients and recursively continue the server with that updated list.
%         {message, From, Message} ->     % Recieves messages, complete with a return PID.
%             lists:foreach(fun(Client) ->% For each Client in the list of clients connected to the server,
%                 Client ! {message, From, Message}   % send that client the message and the return PID
%             end, Clients),              % .
%             server(Clients);            % Recursively keep the server going with the list it still has.
%         disconnect ->                   % When receiving a disconnect,
%             server(Clients)             % Recursively refresh the server process
%     end.                                % .

% client(Server) ->                       % Pass in the server PID to the client process.
%     Server ! {connect, self()},         % Connect to the server by sending it the client PID.
%     receive                             % When the client receives any of this,
%         {message, From, Message} ->     % when it's a message, complete with a return PID,
%             io:format("Received from ~p: ~p~n", [From, Message]),   % display the message and return PID in the terminal.
%             client(Server)              % Recursively continue the client process
%     end.                                % .

% server(Clients, ListenSocket) ->
%     {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
%     NewClients = [ClientSocket | Clients],
%     spawn(fun() -> server(NewClients, ListenSocket) end), %% Keep accepting new connections
%     receive
%         {tcp, ClientSocket, Message} ->
%             lists:foreach(fun(Client) ->
%                 gen_tcp:send(Client, Message)
%             end, NewClients),
%             server(NewClients, ListenSocket)
%     end.

% client(Socket) ->
%     receive
%         {tcp, _, Message} ->
%             io:format("Received from ~p: ~p~n", [Socket, Message]),
%             client(Socket)
%     end.

% send_message(Server, Message) ->        % To send a message, we need the server PID and the message.
%     Server ! {message, self(), Message}.% Send the server the message with the return PID. 

% send_message(Socket, Message) ->
%     gen_tcp:send(Socket, Message)).



-module(chatroom).
-export([start_server/1, start_client/2, send_message/2]).

% Starts the server process
start_server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, true}, {reuseaddr, true}]),
    io:format("Server listening on port ~p~n", [Port]),
    spawn(fun() -> server([], ListenSocket) end),
    ok.

% Starts the client process
start_client(IP, Port) ->
    Self = self(),
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, once}]),
    spawn(fun() -> client(Socket, Self) end),
    Socket.

% Server function to handle client connections and messages
server(Clients, ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    NewClients = [ClientSocket | Clients],
    io:format("Client connected: ~p~n", [ClientSocket]),
    self() ! {update_clients, NewClients},
    spawn(fun() -> server(NewClients, ListenSocket) end), %% Keep accepting new connections
    handle_messages(NewClients).

handle_messages(Clients) ->
    receive
        {tcp, Socket, Message} ->
            io:format("Received message from ~p: ~p~n", [Socket, Message]),
            lists:foreach(fun(Client) ->
                gen_tcp:send(Client, Message),
                io:format("Sent message to ~p: ~p~n", [Client, Message])
            end, Clients),
            handle_messages(Clients);
        {tcp_closed, Socket} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            handle_messages(lists:delete(Socket, Clients));
        {update_clients, NewClients} ->
            io:format("Updating clients list: ~p~n", [NewClients]),
            handle_messages(NewClients)
    end.

% Client function to handle incoming messages
client(Socket, Parent) ->
    receive
        {tcp, _, Message} ->
            Parent ! {client_message, Message},
            client(Socket, Parent);
        {tcp_closed, _} ->
            io:format("Connection closed~n");
        {tcp_error, _, Reason} ->
            io:format("Connection error: ~p~n", [Reason])
    end.

% Function to send messages from client to server
send_message(Socket, Message) ->
    gen_tcp:send(Socket, Message).

%Socket1 = chatroom:start_client("35.188.15.57", 8080).
%chatroom:send_message(Socket1, <<"Hello World!">>).
