-module(acceptor_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, No_of_acceptors) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, No_of_acceptors]).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([Port, No_of_acceptors]) ->
    {ok, Listen_sock} = gen_tcp:listen(Port, [{active, false}]),

    Create_workers = fun() -> {make_ref(),
                        {tcp_client, tcp_spawn, [Listen_sock]},
                        transient, brutal_kill, worker, [client]}
                     end,

    %% Creates specs for <n> number of accepting processes
    _Workers = [Create_workers() || _ <- lists:seq(0, No_of_acceptors)],
    io:format("Initializing acceptors...\n"),
    {ok, {{one_for_one, 100, 100}, _Workers}}.

