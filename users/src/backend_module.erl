-module(backend_module).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export(
   [ registered_name/0
   ]).
-record(
  state,
  {
  }).

registered_name() ->
    ?MODULE.

% ----- gen_server callbacks -----------------------------

init(_args) ->
    {ok, #state{}}.

handle_info(_msg, State) ->
    {noreply, State}.

handle_call(_request, _from, State) ->
    {reply, {ok,ignored}, State}.

handle_cast(_msg, State) ->
    {noreply, State}.

% ----- internal ----------------------------------------
