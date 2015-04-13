-module(app_test).
-include_lib("eunit/include/eunit.hrl").
-define(PORTNR, 5555).
-define(TIMEOUT, 1000).

%% End to end echo test testing the tcp and messages.
%% Todo: Less implementation specific rewrite
echo_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(_) ->
      {ok, Sock} = gen_tcp:connect(localhost, ?PORTNR, [{active, false}]),
      {ok, Enter_nick} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
      ?_assertEqual(Enter_nick, "Enter nick>"),

      ok = gen_tcp:send(Sock, "Test"),
     {ok, Welcome} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
     ?_assertEqual(Welcome, "Test has joined\n"),

     ok = gen_tcp:send(Sock, "hello"),
     {ok, Echo} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
     ?_assertEqual(Echo, "<Test>hello\n")

     end
     }.

start() ->
    application:start(chatserver).

stop(_) ->
    application:stop(chatserver).
