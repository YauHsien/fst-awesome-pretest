-module(orchestrator).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-export(
   [
    %interwinde/3,
    %sequence/4,
    registered_name/0
   ]).
-include("../../include/constants.hrl").
-record(
  state,
  {
   sell = ets:new(sell_incomplete_orders, [bag]),
   buy = ets:new(buy_incomplete_orders, [bag])
  }).

registered_name() ->
    ?MODULE.

% ----- gen_server ------------------------------------

init(_args) ->
    State = #state{},
    SellTb = State#state.sell,
    BuyTb = State#state.buy,
    OrderTypes = [?Buy, ?Sell],
    CardTypes = [?Pikachu, ?Bulbasaur, ?Charmander, ?Squirtle],
    Sequence = sequence(1.0, 10.0, 0.01, 2),
    Interwined = interwinde(OrderTypes, CardTypes, Sequence),
    % Create processes, each for one 1 amount case;
    % 4 * 901 processes for sell orders,
    % while 4 * 901 ones for buy orders.
    lists:map(
      fun({OrderType, CardType, Amount}) ->
              Id =
                  build_id([ atom_to_list(OrderType), "_",
                             atom_to_list(CardType), "_",
                             float_to_list(Amount)
                           ]),
              {ok, Pid} =
                  gen_server:start({local,Id}, incomplete_orders, [Amount], []),
              ets:insert(
                case OrderType of ?Buy -> BuyTb; ?Sell -> SellTb end,
                { 0, CardType, Amount, Pid })
      end,
      Interwined),
    {ok, #state{ sell= SellTb, buy= BuyTb }}.

% Pricing strategy:
% For the incoming sell order, match the incomplete buy order with highest possible amount;
% for the incoming buy order, match one with lowest possible amount.
handle_call(
  {match,order,?Buy,OwTS,OwSeq,OrTS,OrSeq,CTp,Amt,?USD}, _from, #state{ sell= SellTb }=State
 ) ->
    IncompleteOrdersPidList =
        find_incomplete_orders_pids(SellTb, ?Buy, CTp, Amt),
    MatcherInfo =
        gen_server:start(
          {local,build_id([ integer_to_list(OwTS), integer_to_list(OwSeq),
                            integer_to_list(OrTS), integer_to_list(OrSeq)
                          ])},
          matcher,
          [ {order,OwTS,OwSeq,OrTS,OrSeq,Amt,?USD},
            IncompleteOrdersPidList
          ],
          []),
    {reply, MatcherInfo, State};

handle_call(
  {match,order,?Sell,OwTS,OwSeq,OrTS,OrSeq,CTp,Amt,?USD}, _from, #state{ buy= BuyTb }=State
 ) ->
    IncompleteOrdersPidList =
        find_incomplete_orders_pids(BuyTb, ?Sell, CTp, Amt),
    MatcherInfo =
        gen_server:start(
          {local,build_id([ integer_to_list(OwTS), integer_to_list(OwSeq),
                            integer_to_list(OrTS), integer_to_list(OrSeq)
                          ])},
          matcher,
          [ {order,OwTS,OwSeq,OrTS,OrSeq,Amt,?USD},
            IncompleteOrdersPidList
          ],
          []),
    {reply, MatcherInfo, State};

handle_call(_request, _from, #state{ sell= SellTb, buy= BuyTb }=State) ->
    {reply, {SellTb,BuyTb}, State}.

handle_cast(_msg, State) ->
    {noreply, State}.

% ---------- internal -----------------------------------

sequence(From, To, Inc, Precision) when From < To ->
    P = math:pow(10, Precision),
    sequence(round(From*P), round(To*P), round(Inc*P), P, []).

sequence(From, To, _inc, Precision, Acc) when From > To ->
    lists:reverse(lists:map(fun(N)-> N/Precision end, Acc));
sequence(From, To, Inc, Precision, Acc) ->
    sequence(From+Inc, To, Inc, Precision, [From|Acc]).

interwinde(OrderType, CardType, AmountList)
  when is_atom(OrderType) andalso is_atom(CardType) andalso is_list(AmountList) ->
      lists:map(
        fun(Amount) -> {OrderType, CardType, Amount} end,
        AmountList);
interwinde(OrderType, CardTypes, AmountList)
  when is_atom(OrderType) andalso is_list(CardTypes) andalso is_list(AmountList) ->
      lists:map(
        fun(CardType) -> interwinde(OrderType, CardType, AmountList) end,
        CardTypes);
interwinde(OrderTypes, CardTypes, AmountList)
  when is_list(OrderTypes) andalso is_list(CardTypes) andalso is_list(AmountList) ->
    lists:flatten(
      lists:map(
        fun(OrderType) -> interwinde(OrderType, CardTypes, AmountList) end,
        OrderTypes)).

build_id(StrList) when is_list(StrList) ->
    list_to_atom(lists:flatten(lists:foldl(fun(Str,Acc) -> [Str, "_" , Acc] end, "", StrList))).

find_incomplete_orders_pids(EtsTable, OrderType, CardType, Amount) ->
    AmountList =
        ets:select(EtsTable, [{{1,CardType,'$1','_'},[],[{{'$1'}}]}]),
    AmountList1 =
        case OrderType of
            ?Buy -> %% Find lower or equal amount
                lists:filter(fun(A) when A =< Amount -> true; (_) -> false end,
                             AmountList);
            ?Sell -> %% Find higher or equal amount
                lists:filter(fun(A) when A >= Amount -> true; (_) -> false end,
                             AmountList)
        end,
    lists:map(
      fun(A) ->
              [[Pid]] = ets:select(EtsTable, [{{1,CardType,A,'$1'},[],['$$']}]),
              Pid
      end,
      AmountList1).

switch_to_nonempty_status(EtsTable, CardType, Amount) ->
    case ets:select(EtsTable,[{{0,CardType,Amount,'$1'},[],['$_']}]) of
        [{ 0, CardType1, Amount1, Pid }=Item|_] ->
            ets:delete_object(EtsTable, Item),
            ets:insert(EtsTable, {1,CardType1,Amount1,Pid}),
            ok;
        _ ->
            ok
    end.

switch_to_empty_status(EtsTable, CardType, Amount) ->
    case ets:select(EtsTable,[{{1,CardType,Amount,'$1'},[],['$_']}]) of
        [{ 1, CardType1, Amount1, Pid }=Item|_] ->
            ets:delete_object(EtsTable, Item),
            ets:insert(EtsTable, {0,CardType1,Amount1,Pid}),
            ok;
        _ ->
            ok
    end.
