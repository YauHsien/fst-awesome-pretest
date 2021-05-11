-module(lobby).
-export(
   [
    status/0,
    find_incomplete_orders/3,
    insert_incomplete_order/8,
    remove_incomplete_order/4,
    insert_order/8,
    insert_order_tracked/8,
    remove_order_tracked/4,
    update_order_tracked/9,
    find_orders_tracked/2,
    find_orders_tracked/3,
    insert_trade/10,
    insert_trade_tracked/15,
    remove_trade_tracked/5,
    find_trades_tracked/2,
    find_trades_tracked/3,
    insert_user/2,
    find_users/2
   ]).
-include("../../include/constants.hrl").
-include("include/database.hrl").
-include("include/user.hrl").
-define(TL, helper:translate).

status() ->
    {ok, 100}.

-spec find_incomplete_orders(
        OrderType :: ?Buy | ?Sell,
        Currency  :: ?USD,
        Amount    :: price_amount()
       ) ->
          {ok, Result :: list()} |
          {error, Reason :: term()}.

find_incomplete_orders(OrderType, Currency, Amount) ->
    {Sql, Params} =
        ?SELECT_RECORD_INCOMPLETE_ORDER(?TL(OrderType), ?TL(Currency), Amount),
    case helper:query(Sql, Params) of
        {ok, List} when is_list(List) ->
            Result =
                lists:map(fun({F1,F2,F3,F4,F5,F6,F7,F8}) ->
                                  {F1,F2,F3,F4,?TL(F5),?TL(F6),?TL(F7),F8}
                          end,
                          List),
            {ok, Result};
        Any ->
            Any
    end.

-spec insert_incomplete_order(
        Ow_TS  :: unix_timestamp(),
        Ow_seq :: sequence(),
        Or_TS  :: unix_timestamp(),
        Or_seq :: sequence(),
        OrderType :: ?Buy | ?Sell,
        CardType  :: ?Pikachu | ?Bulbasaur | ?Charmander | ?Squirtle,
        Currency  :: ?USD,
        Amount    :: price_amount()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

insert_incomplete_order(
  Ow_TS, Ow_seq, Or_TS, Or_seq, OrderType, CardType, Currency, Amount) ->
    {Sql, Params} =
        ?INSERT_RECORD_INCOMPLETE_ORDER(
           #incomplete_order_{
              ownertimestamp = Ow_TS,
              ownerseq = Ow_seq,
              ordertimestamp = Or_TS,
              orderseq = Or_seq,
              ordertype = ?TL(OrderType),
              cardtype = ?TL(CardType),
              currency = ?TL(Currency),
              amount = Amount
             }),
    helper:query(Sql, Params).

-spec remove_incomplete_order(
        Ow_TS  :: unix_timestamp(),
        Ow_seq :: sequence(),
        Or_TS  :: unix_timestamp(),
        Or_seq :: sequence()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

remove_incomplete_order(OwnerTimestamp, OwnerSeq, OrderTimestamp, OrderSeq) ->
    {Sql, Params} =
        ?DELETE_RECORD_INCOMPLETE_ORDER(
           #incomplete_order_{
              ownertimestamp = OwnerTimestamp,
              ownerseq = OwnerSeq,
              ordertimestamp = OrderTimestamp,
              orderseq = OrderSeq
             }),
    helper:query(Sql, Params).

-spec insert_order(
        Or_TS  :: unix_timestamp(),
        Or_seq :: sequence(),
        OrderType :: ?Buy | ?Sell,
        CardType  :: ?Pikachu | ?Bulbasaur | ?Charmander | ?Squirtle,
        Currency  :: ?USD,
        Amount    :: price_amount(),
        Ow_TS  :: unix_timestamp(),
        Ow_seq :: sequence()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

insert_order(
  Or_TS, Or_seq, OrderType, CardType, Currency, Amount, Ow_TS, Ow_seq) ->
    {Sql, Params} =
        ?INSERT_RECORD_ORDER(
           #order_{
              timestamp = Or_TS,
              seq = Or_seq,
              ordertype = ?TL(OrderType),
              cardtype = ?TL(CardType),
              currency = ?TL(Currency),
              amount = Amount,
              ownertimestamp = Ow_TS,
              ownerseq = Ow_seq
             }),
    helper:query(Sql, Params).

-spec insert_order_tracked(
        Ow_TS  :: unix_timestamp(),
        Ow_seq :: sequence(),
        Or_TS  :: unix_timestamp(),
        Or_seq :: sequence(),
        OrderType :: ?Buy | ?Sell,
        CardType  :: ?Pikachu | ?Bulbasaur | ?Charmander | ?Squirtle,
        Currency  :: ?USD,
        Amount    :: price_amount()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

insert_order_tracked(
  Ow_TS, Ow_seq, Or_TS, Or_seq, OrderType, CardType, Currency, Amount
 ) ->
    {Sql, Params} =
        ?INSERT_RECORD_ORDER_TRACKER(
           #order_tracker_{
              ownertimestamp = Ow_TS,
              ownerseq = Ow_seq,
              ordertimestamp = Or_TS,
              orderseq = Or_seq,
              ordertype = ?TL(OrderType),
              cardtype = ?TL(CardType),
              currency = ?TL(Currency),
              amount = Amount
             }),
    helper:query(Sql, Params).

-spec remove_order_tracked(
        Ow_TS  :: unix_timestamp(),
        Ow_seq :: sequence(),
        Or_TS  :: unix_timestamp(),
        Or_seq :: sequence()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

remove_order_tracked(Ow_TS, Ow_seq, Or_TS, Or_seq) ->
    {Sql, Params} =
        ?DELETE_RECORD_ORDER_TRACKER(
           #order_tracker_{
              ownertimestamp = Ow_TS,
              ownerseq = Ow_seq,
              ordertimestamp = Or_TS,
              orderseq = Or_seq
             }),
    helper:query(Sql, Params).

-spec update_order_tracked(
        Timestamp :: unix_timestamp(),
        MOw_TS    :: unix_timestamp(),
        MOw_seq   :: sequence(),
        MOr_TS    :: unix_timestamp(),
        MOr_seq   :: sequence(),
        Ow_TS    :: unix_timestamp(),
        Ow_seq   :: sequence(),
        Or_TS    :: unix_timestamp(),
        Or_seq   :: sequence()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

update_order_tracked(
  Timestamp, MOw_TS, MOw_seq, MOr_TS, MOr_seq, Ow_TS, Ow_seq, Or_TS, Or_seq
 ) ->
    {Sql, Params} =
        ?UPDATE_RECORD_ORDER_TRACKER(
           #order_tracker_{
              matchtimestamp = Timestamp,
              matchownertimestamp = MOw_TS,
              matchownerseq = MOw_seq,
              matchordertimestamp = MOr_TS,
              matchorderseq = MOr_seq,
              ownertimestamp = Ow_TS,
              ownerseq = Ow_seq,
              ordertimestamp = Or_TS,
              orderseq = Or_seq
             }),
    helper:query(Sql, Params).

find_orders_tracked(Ow_TS, Ow_seq) ->
    find_orders_tracked(Ow_TS, Ow_seq, ?ORDERS_COUNT_LIMIT).

-spec find_orders_tracked(
        Ow_TS  :: unix_timestamp(),
        Ow_seq :: sequence(),
        Count :: integer()
       ) ->
          {ok, Result :: list()} |
          {error, Reason :: term()}.

find_orders_tracked(Ow_TS, Ow_seq, Count) ->
    {Sql, Params} =
        ?SELECT_RECORD_ORDER_TRACKER(Ow_TS, Ow_seq),
    case helper:query(Sql, Params) of
        {ok, List} when is_list(List) ->
            Result =
                lists:sort(
                  fun(
                    {_f11,_f12,F13,F14,_f15,_f16,_f17,_f18,_f19,_f110,_f111,_f112,_f113},
                    {_f21,_f22,F23,F24,_f25,_f26,_f27,_f28,_f29,_f210,_f211,_f212,_f213}
                   ) when F13 > F23 andalso F14 > F24 -> true;
                     (_,_) -> false
                  end,
                  lists:map(fun({F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13}) ->
                                    {F1,F2,F3,F4,?TL(F5),?TL(F6),?TL(F7),
                                     F8,F9,F10,F11,F12,F13}
                            end,
                            List)
                 ),
            lists:map(
              fun({F1,F2,F3,F4,_f5,_f6,_f7,_f8,_f9,_f10,_f11,_f12,_f13}) ->
                      remove_order_tracked(F1, F2, F3, F4)
              end,
              lists:sublist(Result,Count+1,erlang:length(Result))),
            {ok, lists:sublist(Result,Count)};
        Any ->
            Any
    end.

-spec insert_trade(
        Timestamp :: unix_timestamp(),
        Seq       :: sequence(),
        Ow_TS     :: unix_timestamp(),
        Ow_seq    :: sequence(),
        Or_TS     :: unix_timestamp(),
        Or_seq    :: sequence(),
        MatchOw_TS  :: unix_timestamp(),
        MatchOw_seq :: sequence(),
        MatchOr_TS  :: unix_timestamp(),
        MatchOr_seq :: sequence()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

insert_trade(
  Timestamp, Seq,
  Ow_TS, Ow_seq, Or_TS, Or_seq,
  MatchOw_TS, MatchOw_seq, MatchOr_TS, MatchOr_seq) ->
    {Sql, Params} =
        ?INSERT_RECORD_TRADE(
           #trade_{
              timestamp = Timestamp,
              seq = Seq,
              ownertimestamp = Ow_TS,
              ownerseq = Ow_seq,
              ordertimestamp = Or_TS,
              orderseq = Or_seq,
              matchownertimestamp = MatchOw_TS,
              matchownerseq = MatchOw_seq,
              matchordertimestamp = MatchOr_TS,
              matchorderseq = MatchOr_seq
             }),
    helper:query(Sql, Params).

-spec insert_trade_tracked(
        CardType  :: ?Pikachu | ?Bulbasaur | ?Charmander | ?Squirtle,
        Timestamp :: unix_timestamp(),
        Ow_TS  :: unix_timestamp(),
        Ow_seq :: sequence(),
        Or_TS  :: unix_timestamp(),
        Or_seq :: sequence(),
        OrderType :: ?Buy | ?Sell,
        Currency  :: ?USD,
        Amount    :: price_amount(),
        MatchOw_TS  :: unix_timestamp(),
        MatchOw_seq :: sequence(),
        MatchOr_TS  :: unix_timestamp(),
        MatchOr_seq :: sequence(),
        MatchCurrency :: ?USD,
        MatchAmount :: price_amount()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

insert_trade_tracked(
  CardType, Timestamp, Ow_TS, Ow_seq, Or_TS, Or_seq, OrderType, Currency, Amount,
  MatchOw_TS, MatchOw_seq, MatchOr_TS, MatchOr_seq, MatchCurrency, MatchAmount) ->
    {Sql, Params} =
        ?INSERT_RECORD_TRADE_TRACKER(
           #trade_tracker_{
              cardtype = ?TL(CardType),
              timestamp = Timestamp,
              ownertimestamp = Ow_TS,
              ownerseq = Ow_seq,
              ordertimestamp = Or_TS,
              orderseq = Or_seq,
              ordertype = ?TL(OrderType),
              currency = ?TL(Currency),
              amount = Amount,
              matchownertimestamp = MatchOw_TS,
              matchownerseq = MatchOw_seq,
              matchordertimestamp = MatchOr_TS,
              matchorderseq = MatchOr_seq,
              matchcurrency = ?TL(MatchCurrency),
              matchamount = MatchAmount
             }),
    helper:query(Sql, Params).

-spec remove_trade_tracked(
        CardType  :: ?Pikachu | ?Bulbasaur | ?Charmander | ?Squirtle,
        Or_TS  :: unix_timestamp(),
        Or_seq :: sequence(),
        MatchOr_TS  :: unix_timestamp(),
        MatchOr_seq :: sequence()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

remove_trade_tracked(CardType, Or_TS, Or_seq, MatchOr_TS, MatchOr_seq) ->
    {Sql, Params} =
        ?DELETE_RECORD_TRADE_TRACKER(
           #trade_tracker_{
              cardtype = ?TL(CardType),
              ordertimestamp = Or_TS,
              orderseq = Or_seq,
              matchordertimestamp = MatchOr_TS,
              matchorderseq = MatchOr_seq
             }),
    helper:query(Sql, Params).

find_trades_tracked(CardType, OrderType) ->
    find_trades_tracked(CardType, OrderType, ?TRADES_COUNT_LIMIT).

-spec find_trades_tracked(
        CardType  :: ?Pikachu | ?Bulbasaur | ?Charmander | ? Squirtle,
        OrderType :: ?Buy | ?Sell,
        Count     :: integer()
       ) ->
          {ok, Result :: list()} |
          {error, Reason :: term()}.

find_trades_tracked(CardType, OrderType, Count) ->
    {Sql, Params} =
        ?SELECT_RECORD_TRADE_TRACKER(?TL(CardType), ?TL(OrderType)),
    case helper:query(Sql, Params) of
        {ok, List} when is_list(List) ->
            Result =
                lists:sort(
                  fun(
                    {_f11,F12,F13,_f14,_f15,_f16,_f17,_f18,_f19,_f110,
                     _f111,_f112,_f113,_f114,_f115,_f116},
                    {_f21,F22,F23,_f24,_f25,_f26,_f27,_f28,_f29,_f210,
                     _f211,_f212,_f213,_f214,_f215,_f216}
                   ) when F12 > F22 andalso F13 > F23 -> true;
                     (_,_) -> false
                  end,
                  lists:map(fun({F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,
                                 F11,F12,F13,F14,F15,F16}) ->
                                    {F1,F2,F3,F4,?TL(F5),?TL(F6),?TL(F7),
                                     F8,F9,F10,F11,F12,F13,F14,F15,F16}
                            end,
                            List)
                 ),
            lists:map(fun({F1,_f2,_f3,_f4,_f5,F6,F7,_f8,_f9,_f10,
                           _f11,_f12,F13,F14,_f15,_f16}) ->
                              remove_trade_tracked(F1, F6, F7, F13, F14)
                      end,
                      lists:sublist(Result,Count+1,erlang:length(Result))),
            {ok, lists:sublist(Result,Count)};
        Any ->
            Any
    end.

-spec insert_user(
        Timestamp :: unix_timestamp(),
        Seq       :: sequence()
       ) ->
          {ok, Result :: count()} |
          {error, Reason :: term()}.

insert_user(Timestamp, Seq) ->
    {Sql, Params} =
        ?INSERT_RECORD_USER(Timestamp, Seq),
    helper:query(Sql, Params).

-spec find_users(
        Timestamp :: unix_timestamp(),
        Seq       :: sequence()
       ) ->
          {ok, Result :: list()} |
          {error, Reason :: term()}.

find_users(Timestamp, Seq) ->
    {Sql, Params} =
        ?SELECT_RECORD_USER(Timestamp, Seq),
    helper:query(Sql, Params).

