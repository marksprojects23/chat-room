# Overview

As a software engineer, I hope and aspire to learn various technologies and implementations to increase my general knowledge of software and expand my repertiore so that I may contribute to the world at large with my own unique takes on solutions that people need.

I wrote an Erlang UDP handler that acts as a server and a client for a chatroom. To use it, open an Erlang shell for the server node. The machine of the server node will have to have port 8080 (or any port you edit it to) open for UDP. In the source folder, run `c(chatroom).` to compile it, and then run `chatroom:start().` to start the server. To connect a client, open an Erlang shell. You may have to edit the code to apply to your server node. In that Erlang shell, run `Socket = chatroom:start_client().` to run the client (which connects to the server) and store the socket details. With that, you can run `chatroom:send_message(Socket, "Hello, World!").` to send a message. Every client that is connected to the server node will receive it, though you may have to `flush().` to receive it.

I wrote this to test my ability in Erlang and to provide a proof of concept of a simple UDP chatroom that is minimally functional as long as you port forward. With this proof of concept, after a bit of tweaks, it can be a real chatroom.

[Software Demo Video](https://youtu.be/1wrPJCOk4sQ)

# Network Communication

I used Erlang/OTP. It's concurrent and fault-tolerant, meaning this proof of concept can truly become a scaleable and powerful chatroom.

This particular instance is using UDP on port 8080.

The messages being sent are Erlang tuple data types where the 5th of 5 tuple members represent the message being sent.

# Development Environment

I used Erlang documentation including Learn You Some Erlang For Great Good. I also used ChatGPT for consultation. `rebar3` aided in creating a baseline setup for the project and Visual Studio Code with Erlang extensions made for easy development.

I used baseline Erlang/OTP, no external libraries needed.

# Useful Websites

* [Learn You Some Erlang for Great Good](https://learnyousomeerlang.com/)
* [Erlang Documentation](https://www.erlang.org/doc/readme.html)

# Future Work

* Create a GUI to embody the proof of concept
* Make the client easily deployable
* Craft the server deployment so that anyone can deploy it
