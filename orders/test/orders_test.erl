-module(orders_test).
-include_lib("eunit/include/eunit.hrl").
-include("../../include/constants.hrl").
-include("../src/include/order.hrl").

order_type_update_test() ->
    {ok, Order} = order:new_order(self()),
    Order1 = gen_server:call(Order, {set,orderType,?Buy}),
    ?assert(Order1#order.orderType == ?Buy),
    Order2 = gen_server:call(Order, {set,orderType,?Sell}),
    ?assert(Order2#order.orderType == ?Sell),
    Order3 = gen_server:call(Order, {set,orderType,fuckup}),
    ?assert(Order3 == ok).

order_currency_update_test() ->
    {ok, Order} = order:new_order(self()),
    Order4 = gen_server:call(Order, {set,currency,?USD}),
    ?assert(Order4#order.currency == ?USD),
    Order5 = gen_server:call(Order, {set,currency,fuckup}),
    ?assert(Order5 == ok).

order_price_update_test() ->
    {ok, Order} = order:new_order(self()),
    Order1 = gen_server:call(Order, {set,price,1.0}),
    ?assertMatch(1.00, Order1#order.price),
    Order2 = gen_server:call(Order, {set,price,10.00}),
    ?assertMatch(10.0, Order2#order.price),
    Order3 = gen_server:call(Order, {set,price,0,9999991}),
    ?assertMatch(ok, Order3),
    Order4 = gen_server:call(Order, {set,price,10,000000001}),
    ?assertMatch(ok, Order4).

order_card_type_update_test() ->
    {ok, Order} = order:new_order(self()),
    Order1 = gen_server:call(Order, {set,cardType,?Pikachu}),
    ?assertMatch(?Pikachu, Order1#order.cardType),
    Order2 = gen_server:call(Order, {set,cardType,?Bulbasaur}),
    ?assertMatch(?Bulbasaur, Order2#order.cardType),
    Order3 = gen_server:call(Order, {set,cardType,?Charmander}),
    ?assertMatch(?Charmander, Order3#order.cardType),
    Order4 = gen_server:call(Order, {set,cardType,?Squirtle}),
    ?assertMatch(?Squirtle, Order4#order.cardType),
    Order5 = gen_server:call(Order, {set,cardType,fuckup}),
    ?assertMatch(ok, Order5).


