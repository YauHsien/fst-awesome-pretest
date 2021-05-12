%%%-------------------------------------------------------------------
%% @doc orders public API
%% @end
%%%-------------------------------------------------------------------

-module(orders_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    orders_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
