-module(app).

-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================
%%% API, specify args in the .app file
%%%===================================================================
start(_Type, [Port, No_of_acceptors]) ->
    root_supervisor:start_link(Port, No_of_acceptors).

stop(_State) ->
    ok.
