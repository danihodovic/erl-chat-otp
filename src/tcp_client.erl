-module(tcp_client).
-export([tcp_acceptor/1]).

%%%===================================================================
%%% API
%%%===================================================================

% Accept a connection and spawn another process to handle that client
tcp_acceptor(Listen_sock) ->
    {ok, Sock} = gen_tcp:accept(Listen_sock),
    spawn(fun() -> client_handler(Sock) end),
    tcp_acceptor(Listen_sock).


%%%===================================================================
%%% Client handling
%%%===================================================================

%% Registers a nick with the server and proceeds with the messaging
client_handler(Sock) ->
    gen_tcp:send(Sock, "Enter nick>"),
    case gen_tcp:recv(Sock, 0) of
        {ok, Raw_msg} ->
            Name = hd(string:tokens(Raw_msg, "\r\n")),
            server:add_client(Sock, Name),
            chat(Sock);
        {error, closed} ->
            ok
    end.

%% The client interaction process
%% Handles various messages, some by interacting with server, some without
chat(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Raw_msg} ->
            Msg = hd(string:tokens(Raw_msg, "\r\n")),
            case Msg of
                "--help"   -> gen_tcp:send(Sock, get_cmds());
                "--active" -> server:broadcast_active_clients();
                "--cowsay" -> server:broadcast_cowsay();
                _          -> server:broadcast(Msg)
            end,
            chat(Sock);
        {error, closed} ->
            ok
    end.


%%%===================================================================
%%% Helper functions that don't require server interaction.
%%% They need to be server compliant though
%%%===================================================================

get_cmds() ->
  Cmds = [{"--help",            "Display a list of available commands"},
          {"--active",          "Show the people currently in the chat room"},
          {"--cowsay",          "Hmm..."}],

  Strs = [io_lib:format("~-20s~s", [Cmd, Info]) || {Cmd, Info} <- Cmds],
  "\n" ++ string:join(Strs, "\n") ++ "\n".
