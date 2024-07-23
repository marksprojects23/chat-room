-module(chatroom).
-compile([start_server/0, server/1, start_client/1, client/1, send_message/2]).

start_server() ->
    spawn(fun() -> server([]) end).     % Starts the server process.

start_client(Server) ->
    spawn(fun() -> client(Server) end). % Starts a client process.

server(Clients) ->                      % Pass in the list of clients to the server.
    receive                             % When the server receives any of these,
        {connect, Client} ->            % for a connecting client,
            server([Client | Clients]); % append the client's PID to the list of clients and recursively continue the server with that updated list.
        {message, From, Message} ->     % Recieves messages, complete with a return PID.
            lists:foreach(fun(Client) ->% For each Client in the list of clients connected to the server,
                Client ! {message, From, Message}   % send that client the message and the return PID
            end, Clients),              % .
            server(Clients);            % Recursively keep the server going with the list it still has.
        disconnect ->                   % When receiving a disconnect,
            server(Clients)             % Recursively refresh the server process
    end.                                % .

client(Server) ->                       % Pass in the server PID to the client process.
    Server ! {connect, self()},         % Connect to the server by sending it the client PID.
    receive                             % When the client receives any of this,
        {message, From, Message} ->     % when it's a message, complete with a return PID,
            io:format("Received from ~p: ~p~n", [From, Message]),   % display the message and return PID in the terminal.
            client(Server)              % Recursively continue the client process
    end.                                % .

send_message(Server, Message) ->        % To send a message, we need the server PID and the message.
    Server ! {message, self(), Message}.% Send the server the message with the return PID. 