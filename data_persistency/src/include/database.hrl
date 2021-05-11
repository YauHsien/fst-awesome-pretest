-record(
    incomplete_order_,
    { ownertimestamp :: integer(),
      ownerseq       :: integer(),
      ordertimestamp :: integer(),
      orderseq       :: integer(),
      ordertype      :: integer(),
      cardtype       :: integer(),
      currency       :: integer(),
      amount         :: float()
    }).
-define(
   SELECT_RECORD_INCOMPLETE_ORDER(O_type, Currency, Amount),
   { lists:flatten(
       [ "select ",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
         "ordertype, cardtype, currency, amount ",
         "from incomplete_order where ",
         "ordertype = $1 and currency = $2 and amount = $3"
       ]),
     [O_type, Currency, Amount]
   }).
-define(
   INSERT_RECORD_INCOMPLETE_ORDER(Ord),
   { lists:flatten(
       [ "insert into incomplete_order (",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
         "ordertype, cardtype, currency, amount",
         ") values ($1, $2, $3, $4, $5, $6, $7, $8)"
       ]),
     [ Ord#incomplete_order_.ownertimestamp,
       Ord#incomplete_order_.ownerseq,
       Ord#incomplete_order_.ordertimestamp,
       Ord#incomplete_order_.orderseq,
       Ord#incomplete_order_.ordertype,
       Ord#incomplete_order_.cardtype,
       Ord#incomplete_order_.currency,
       Ord#incomplete_order_.amount
     ]
   }).
-define(
   DELETE_RECORD_INCOMPLETE_ORDER(Ord),
   { lists:flatten(
       [ "delete from incomplete_order where ",
         "ownertimestamp = $1 and ",
         "ownerseq = $2 and ",
         "ordertimestamp = $3 and ",
         "orderseq = $4"
       ]),
     [ Ord#incomplete_order_.ownertimestamp,
       Ord#incomplete_order_.ownerseq,
       Ord#incomplete_order_.ordertimestamp,
       Ord#incomplete_order_.orderseq
     ]
   }).
-record(
   order_,
   { timestamp :: integer(),
     seq       :: integer(),
     ordertype      :: integer(),
     cardtype       :: integer(),
     currency       :: integer(),
     amount         :: float(),
     ownertimestamp :: integer(),
     ownerseq       :: integer()
   }).
-define(
   INSERT_RECORD_ORDER(Ord),
   { lists:flatten(
       [ "insert into \"order\" (",
         "\"timestamp\", seq, ordertype, cardtype,",
         "currency, amount, ownertimestamp, ownerseq",
         ") values ($1, $2, $3, $4, $5, $6, $7, $8)"
       ]),
     [ Ord#order_.timestamp,
       Ord#order_.seq,
       Ord#order_.ordertype,
       Ord#order_.cardtype,
       Ord#order_.currency,
       Ord#order_.amount,
       Ord#order_.ownertimestamp,
       Ord#order_.ownerseq
    ]
   }).
-record(
   order_tracker_,
   { ownertimestamp      :: integer(),
     ownerseq            :: integer(),
     ordertimestamp      :: integer(),
     orderseq            :: integer(),
     ordertype           :: integer(),
     cardtype            :: integer(),
     currency            :: integer(),
     amount              :: float(),
     matchtimestamp      :: integer(),
     matchownertimestamp :: integer(),
     matchownerseq       :: integer(),
     matchordertimestamp :: integer(),
     matchorderseq       :: integer()
   }).
-define(
   SELECT_RECORD_ORDER_TRACKER(O_TS, O_seq),
   { lists:flatten(
       [ "select ",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq, ",
         "ordertype, cardtype, currency, amount, matchtimestamp, ",
         "matchownertimestamp, matchownerseq, matchordertimestamp, matchorderseq ",
         "from order_tracker where ",
         "ownertimestamp = $1 and ownerseq = $2"
       ]),
     [O_TS, O_seq]
   }).
-define(
   INSERT_RECORD_ORDER_TRACKER(Ord),
   { lists:flatten(
       [ "insert into order_tracker (",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
         "ordertype, cardtype, currency, amount,",
         "matchtimestamp, matchownertimestamp, matchownerseq,",
         "matchordertimestamp, matchorderseq",
         ") values ($1, $2, $3, $4, $5, $6, $7, $8, default, default, default, default, default)"
       ]),
     [ Ord#order_tracker_.ownertimestamp,
       Ord#order_tracker_.ownerseq,
       Ord#order_tracker_.ordertimestamp,
       Ord#order_tracker_.orderseq,
       Ord#order_tracker_.ordertype,
       Ord#order_tracker_.cardtype,
       Ord#order_tracker_.currency,
       Ord#order_tracker_.amount
     ]
   }).
-define(
   DELETE_RECORD_ORDER_TRACKER(Ord),
   { lists:flatten(
       [ "delete from order_tracker where ",
         "ownertimestamp = $1 and ",
         "ownerseq = $2 and ",
         "ordertimestamp = $3 and ",
         "orderseq = $4"
       ]),
     [ Ord#order_tracker_.ownertimestamp,
       Ord#order_tracker_.ownerseq,
       Ord#order_tracker_.ordertimestamp,
       Ord#order_tracker_.orderseq
     ]
   }).
-define(
   UPDATE_RECORD_ORDER_TRACKER(Ord),
   { lists:flatten(
       [ "update order_tracker set ",
         "matchtimestamp = $1,",
         "matchownertimestamp = $2,",
         "matchownerseq = $3,",
         "matchordertimestamp = $4,",
         "matchorderseq = $5 "
         "where ",
         "ownertimestamp = $6 and ",
         "ownerseq = $7 and ",
         "ordertimestamp = $8 and ",
         "orderseq = $9"
       ]),
     [ Ord#order_tracker_.matchtimestamp,
       Ord#order_tracker_.matchownertimestamp,
       Ord#order_tracker_.matchownerseq,
       Ord#order_tracker_.matchordertimestamp,
       Ord#order_tracker_.matchorderseq,
       Ord#order_tracker_.ownertimestamp,
       Ord#order_tracker_.ownerseq,
       Ord#order_tracker_.ordertimestamp,
       Ord#order_tracker_.orderseq
     ]
   }).
-record(
   trade_,
   { timestamp           :: integer(),
     seq                 :: integer(),
     ownertimestamp      :: integer(),
     ownerseq            :: integer(),
     ordertimestamp      :: integer(),
     orderseq            :: integer(),
     matchownertimestamp :: integer(),
     matchownerseq       :: integer(),
     matchordertimestamp :: integer(),
     matchorderseq       :: integer()
   }).
-define(
   INSERT_RECORD_TRADE(Ord),
   { lists:flatten(
       [ "insert into trade (",
         "\"timestamp\", seq,",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
         "matchownertimestamp, matchownerseq, matchordertimestamp, matchorderseq",
         ") values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
       ]),
     [ Ord#trade_.timestamp,
       Ord#trade_.seq,
       Ord#trade_.ownertimestamp,
       Ord#trade_.ownerseq,
       Ord#trade_.ordertimestamp,
       Ord#trade_.orderseq,
       Ord#trade_.matchownertimestamp,
       Ord#trade_.matchownerseq,
       Ord#trade_.matchordertimestamp,
       Ord#trade_.matchorderseq
     ]
   }).
-record(
   trade_tracker_,
   { cardtype            :: integer(),
     timestamp           :: integer(),
     seq                 :: integer(),
     ownertimestamp      :: integer(),
     ownerseq            :: integer(),
     ordertimestamp      :: integer(),
     orderseq            :: integer(),
     ordertype           :: integer(),
     currency            :: integer(),
     amount              :: float(),
     matchownertimestamp :: integer(),
     matchownerseq       :: integer(),
     matchordertimestamp :: integer(),
     matchorderseq       :: integer(),
     matchcurrency       :: integer(),
     matchamount         :: float()
   }).
-define(
   SELECT_RECORD_TRADE_TRACKER(Cardtype, OrderType),
   { lists:flatten(
       [ "select "
         "cardtype, \"timestamp\", seq,",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
         "ordertype, currency, amount,",
         "matchownertimestamp, matchownerseq, matchordertimestamp, matchorderseq,",
         "matchcurrency, matchamount ",
         "from trade_tracker where ",
         "cardtype = $1 and ",
         "ordertype = $2"
      ]),
    [Cardtype, OrderType]
   }).
-define(
   SELECT_RECORD_TRADE_TRACKER(C_type),
   { lists:flatten(
       [ "select "
         "cardtype, \"timestamp\", seq,",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
         "ordertype, currency, amount,",
         "matchownertimestamp, matchownerseq, matchordertimestamp, matchorderseq,",
         "matchcurrency, matchamount ",
         "from trade_tracker where ",
         "cardtype = $1"
      ]),
     [C_type]
   }
  ).
-define(
   INSERT_RECORD_TRADE_TRACKER(Ord),
   { lists:flatten(
       [ "insert into trade_tracker (",
         "cardtype, \"timestamp\", seq,",
         "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
         "ordertype, currency, amount,",
         "matchownertimestamp, matchownerseq, matchordertimestamp, matchorderseq,",
         "matchcurrency, matchamount",
         ") values ($1, $2, nextval('trade_seq'), $3, $4, $5, $6, $7, $8, $9,",
         "$10, $11, $12, $13, $14, $15)"
       ]),
     [ Ord#trade_tracker_.cardtype,
       Ord#trade_tracker_.timestamp,
       Ord#trade_tracker_.ownertimestamp,
       Ord#trade_tracker_.ownerseq,
       Ord#trade_tracker_.ordertimestamp,
       Ord#trade_tracker_.orderseq,
       Ord#trade_tracker_.ordertype,
       Ord#trade_tracker_.currency,
       Ord#trade_tracker_.amount,
       Ord#trade_tracker_.matchownertimestamp,
       Ord#trade_tracker_.matchownerseq,
       Ord#trade_tracker_.matchordertimestamp,
       Ord#trade_tracker_.matchorderseq,
       Ord#trade_tracker_.matchcurrency,
       Ord#trade_tracker_.matchamount
    ]
   }).
-define(
   DELETE_RECORD_TRADE_TRACKER(Ord),
   { lists:flatten(
       [ "delete from trade_tracker where ",
         "cardtype = $1 and "
         "ordertimestamp = $2 and ",
         "orderseq = $3 and ",
         "matchordertimestamp = $4 and ",
         "matchorderseq = $5"
       ]),
     [ Ord#trade_tracker_.cardtype,
       Ord#trade_tracker_.ordertimestamp,
       Ord#trade_tracker_.orderseq,
       Ord#trade_tracker_.matchordertimestamp,
       Ord#trade_tracker_.matchorderseq
     ]
   }).
-record(
   user_,
   { timestamp :: integer(),
     seq       :: integer()
   }).
-define(
   INSERT_RECORD_USER(Timestamp, Seq),
   { "insert into \"user\" (\"timestamp\", seq) values ($1, $2)",
     [ Timestamp, Seq ]
   }).
-define(
   SELECT_RECORD_USER(Timestamp, Seq),
   { "select \"timestamp\", seq from \"user\" where \"timestamp\" = $1 and seq = $2",
     [ Timestamp, Seq ]
   }).
