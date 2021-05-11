%%%-------------------------------------------------------------------
%% @doc users public API
%% @end
%%%-------------------------------------------------------------------

-module(users_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    users_sup:start_link().

stop(_State) ->
    io:fwrite(" stopping application ~p~n", [_State]),
    ok.

%% internal functions
