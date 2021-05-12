-module(matcher).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-record(
  state,
  {
   incomplete_orders_pid_list
  }).
-include("../../include/constants.hrl").

% ---------- gen_server -------------------------------------

init([[{_amt,_pid}|_]= IncompleteOrdersPidList]) ->
    State =
        #state
        {
          incomplete_orders_pid_list =
              lists:sort(
                fun({Amt1,Pid1}, {Amt2,Pid2}) when Amt1 < Amt2 -> true;
                   (_, _) -> false end,
                IncompleteOrdersPidList)
        },
    {ok, State}.

% This gen_server is one-time match facility.
handle_call({match,order,OrderTS,OrderSeq},
            _from,
            #state{ incomplete_orders_pid_list= PidList }=State
           ) ->
    Result =
        lists:foldl(
          fun({_amt,_pid}, {ok,Result1}) ->
                  {ok, Result1};
             ({_amt,Pid}, nothing) ->
                  case gen_server:call1(Pid, {match,order,OrderTS,OrderSeq}) of
                      {ok,Result1} -> {ok,Result1};
                      nothing -> nothing
                  end
          end,
          nothing,
          PidList),
    {stop, normal, Result, State};
handle_call(_request, _from, State) ->
    {reply, ok, State}.

handle_cast(_msg, State) ->
    {noreply, State}.
