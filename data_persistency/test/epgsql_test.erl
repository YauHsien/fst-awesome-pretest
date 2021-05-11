-module(epgsql_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/include/database.hrl").

record_incomplete_order_test() ->
    Record =
        #incomplete_order
        { ownertimestamp = 1,
          ownerseq       = 2,
          ordertimestamp = 3,
          orderseq       = 4,
          ordertype      = 5,
          cardtype       = 6,
          currency       = 7,
          amount         = 8.9
        },
    Build =
        ?INSERT_RECORD_INCOMPLETE_ORDER(Record),
    Expr =
        { lists:flatten(
            [ "insert into incomplete_order (",
              "ownertimestamp, ownerseq, ordertimestamp, orderseq,",
              "ordertype, cardtype, currrency, amount",
              ") values ($1, $2, $3, $4, $5, $6, $7, $8)"
            ]),
          [ Record#incomplete_order.ownertimestamp,
            Record#incomplete_order.ownerseq,
            Record#incomplete_order.ordertimestamp,
            Record#incomplete_order.orderseq,
            Record#incomplete_order.ordertype,
            Record#incomplete_order.cardtype,
            Record#incomplete_order.currency,
            Record#incomplete_order.amount
          ]
        },
    ?assertMatch(Expr, Build).
