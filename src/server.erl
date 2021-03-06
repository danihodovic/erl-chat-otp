-module(server).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         stop/0
        ]).

-export([
         add_client/2,
         broadcast/1,
         broadcast_active_clients/0,
         broadcast_cowsay/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients}).
-record(client, {pid, socket, name}).

%%%===================================================================
%%% Server usage API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).


%%%===================================================================
%%% Client API
%%%===================================================================

add_client(Sock, Name) ->
    gen_server:call(?MODULE, {add_client, Sock, Name}).

broadcast(Msg) ->
    gen_server:cast(?MODULE, {broadcast, Msg, self()}).

broadcast_active_clients() ->
    gen_server:cast(?MODULE, {broadcast_active_clients, self()}).

broadcast_cowsay() ->
    gen_server:cast(?MODULE, {cowsay, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{clients=[]}}.

%% Add the client to the active list and notify all. Link to remove
%% when client dies
handle_call({add_client, Sock, Name}, {Pid, _Ref}, State) ->
    io:format("Client ~p connected from ~p~n", [Name, Pid]),
    link(Pid),
    Client = #client{pid=Pid, name=Name, socket=Sock},
    NewState = State#state{clients=[Client|State#state.clients]},
    broadcast(NewState#state.clients, Name ++ " has joined\n"),
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Other, _From, State) ->
    io:format("[Debug] Server got strange message: ~p~n", [_Other]),
    {reply, "???", State}.

%% Remove client from the current list
handle_cast({remove_client, Pid}, State) ->
    Clients = [Cli || Cli <- State#state.clients, Cli#client.pid /= Pid],
    NewState = State#state{clients=Clients},
    {noreply, NewState};

%% Broadcast generic message
handle_cast({broadcast, Msg, Pid}, State) ->
    Cli = find_client(State#state.clients, Pid),
    Str = "<" ++ Cli#client.name ++ ">" ++ Msg ++ "\n",
    broadcast(State#state.clients, Str),
    io:format(Str),
    {noreply, State};

%% Broadcast the active clients to the client requesting it
handle_cast({broadcast_active_clients, Pid}, State) ->
    Cli = find_client(State#state.clients, Pid),
    broadcast([Cli], get_active_clients(State#state.clients)),
    {noreply, State};

% Broadcast cowsay to all
handle_cast({cowsay, Pid}, State) ->
    Cli = find_client(State#state.clients, Pid),
    Msg = "<" ++ Cli#client.name ++ "> Activated cowsay \n",
    broadcast(State#state.clients, Msg),
    broadcast(State#state.clients, get_fortune_str()),
    {noreply, State};

handle_cast(_Other, State) ->
    io:format("[Debug] Server got strange message: ~p~n", [_Other]),
    {noreply, State}.


% Handle clients dying
handle_info({'EXIT', From, _}, State) ->
    Msg = find_client(State#state.clients, From) ++ " has left",
    broadcast(State#state.clients, Msg),
    NewState = [Cli || Cli <- State#state.clients, Cli /= From],
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

broadcast(Clients, Msg) ->
    [gen_tcp:send(Cli#client.socket, Msg) || Cli <- Clients].


find_client(Clients, Pid) ->
    hd([Cli || Cli <- Clients, Cli#client.pid == Pid]).


get_active_clients(Clients) ->
  CliNames = [Cli#client.name ++ "\n" || Cli <- Clients],
  Msgs = ["Clients currently active:",
          "---",
          CliNames,
          "---\n"],
  string:join(Msgs, "\n").

get_fortune_str() ->
  FortuneMsg = os:cmd("fortune | cowsay"),
  {ok, Regex} = re:compile("fortune: not found"),
  case re:run(FortuneMsg, Regex) of
    {match, _} ->
      "`install `cowsay,fortune` on the server\n";
    nomatch ->
      FortuneMsg
  end.
