-module(lobby).
-export(
   [
    status/0,
    track_trades/2,
    match_order/1
   ]).
-define(TRADES_MODULE_PID,
        helper:get_sup_child(trades_sup,trades_module)).
-define(BACKEND_MODULE_PID,
        helper:get_sup_child(trades_sup,backend_module)).
-include("../../include/constants.hrl").

status() ->
    {ok, 100}.

-spec track_trades(
        CardType         :: ?Buy | ?Sell,
        TradesCountLimit :: integer()
       ) ->
          { ok,    Result :: binary() } |
          { error, Reason :: binary() }.

track_trades(CardType, TradesCountLimit) ->
    case gen_server:call(?TRADES_MODULE_PID,
                         {track,trades,CardType,TradesCountLimit}) of
        {ok, TrackerPid} ->
            case gen_server:call(TrackerPid, {trades,serialize}) of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    {error, ?ERROR(Reason)}
            end;
        {error, Reason} ->
            {error, ?ERROR(Reason)}
    end.

-spec match_order(
        Order :: tuple()
       ) ->
          ok.

match_order({order,OrderType,OwTS,OwSeq,OrTS,OrSeq,CTp,Amt,?USD}) ->
    Orch =
        whereis(orchestrator:registered_name()),
    case gen_server:call(Orch,{match,order,OrderType,OwTS,OwSeq,OrTS,OrSeq,CTp,Amt,?USD}) of
        nomatch ->
            ok;
        {error, Reason} ->
            ok;
        {ok, MatcherPid} ->
            case gen_server:call(MatcherPid, {match,order,OrTS,OrSeq}) of
                {ok, {order,MOwTS,MOwSeq,MOrTS,MOrSeq}} ->
                    build_complete_orders(
                      {order,OrderType,OwTS,OwSeq,OrTS,OrSeq,CTp,Amt,?USD},
                      {order,MOwTS,MOwSeq,MOrTS,MOrSeq});
                nothing ->
                    ok
            end
    end.

build_complete_orders(
  {order,OrderType,OwTS,OwSeq,OrTS,OrSeq,CTp,Amt,?USD},
  {order,MOwTS,MOwSeq,MOrTS,MOrSeq}
 ) ->
    %% TODO: Put a trade and update complete status of two orders
    not_implemented,
    ok.
