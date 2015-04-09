%%%=================================
%%% Todo
%%%=================================

-module(application).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    root_supervisor:start_link(5555, 1).

stop(_State) ->
    ok.
