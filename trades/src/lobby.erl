-module(lobby).
-export(
   [
    status/0,
    track_trades/2
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
