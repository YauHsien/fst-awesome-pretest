%%%-------------------------------------------------------------------
%% @doc trades public API
%% @end
%%%-------------------------------------------------------------------

-module(trades_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    trades_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
