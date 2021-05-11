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
-record(
   user_,
   { timestamp :: integer(),
     seq       :: integer()
   }).
