%%%-------------------------------------------------------------------
%% @doc data_persistency public API
%% @end
%%%-------------------------------------------------------------------

-module(data_persistency_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    data_persistency_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
