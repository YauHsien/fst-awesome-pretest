-module(incomplete_orders).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-record(state,{}).

init(_) ->
    {ok, #state{}}.

handle_call(_request, _from, State) ->
    {reply, ok, State}.

handle_cast(_msg, State) ->
    {noreply, State}.
