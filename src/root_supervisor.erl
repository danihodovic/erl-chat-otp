-module(root_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Callbacks
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
    Genserv_specs = {server,
                    {server, start_link, []},
                     transient, 1000, worker, [server]},

    Sock_supervisor_spec = {acceptor_supervisor,
                            {acceptor_supervisor, start_link, [Port, No_of_acceptors]},
                            transient, 1000, supervisor, [acceptor_supervisor]},

    {ok, {{one_for_one, 5, 5}, [Genserv_specs, Sock_supervisor_spec]}}.
