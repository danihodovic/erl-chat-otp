-module(client).
-export([tcp_acceptor/1]).

% Accept a connection and spawn another process to handle that client
tcp_acceptor(Listen_sock) ->
    {ok, Sock} = gen_tcp:accept(Listen_sock),
    spawn(fun() -> client_handler(Sock) end),
    tcp_acceptor(Listen_sock).

% Registers a nick with the server and proceeds with the messaging
client_handler(Sock) ->
    gen_tcp:send(Sock, "Enter nick>"),
    case gen_tcp:recv(Sock, 0) of
        {ok, Raw_msg} ->
            Name = hd(string:tokens(Raw_msg, "\r\n")),
            server:add_client(tcp, Sock, Name),
            chat(Sock);
        {error, closed} ->
            ok
    end.

chat(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Raw_msg} ->
            Msg = string:tokens(Raw_msg, "\r\n"),
            server:broadcast(Msg),
            chat(Sock);
        {error, closed} ->
            ok
    end.
