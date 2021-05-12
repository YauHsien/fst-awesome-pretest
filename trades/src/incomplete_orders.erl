-module(incomplete_orders).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-include("../../include/constants.hrl").
-record(
   state,
   {
    order_type :: ?Buy | ?Sell,
    card_type :: ?Pikachu | ?Bulbasaur | ?Charmander | ? Squirtle,
    amount :: price_amount(),
    currency = ?USD,
    queue = queue:new()
   }).

init([OrderType, CardType, Amount]) ->
    State =
        #state
        {
          order_type = OrderType,
          card_type = CardType,
          amount = Amount
        },
    {ok, State}.

handle_call({rest,order,OwTS,OwSeq,OrTS,OrSeq}, _from, #state{ queue= Q }=State) ->
    Q1 = queue:in(Q, {order,OwTS,OwSeq,OrTS,OrSeq}),
    {reply, ok, #state{ queue= Q1 }=State};
handle_call({match, order, _order_TS, _order_seq},
            _from,
            #state{ order_type= OrderType, card_type= CardType, amount= Amount, queue= Q }=State) ->
    {Result, Q1} =
        case queue:out(Q) of
            {empty, Q2} ->
                {nothing, Q2};
            {{value,Item}, Q2} ->
                {{ok,Item}, Q2}
        end,
    case queue:is_empty(Q1) of
        true ->
            Orch =
                whereis(orchestrator:registered_name()),
            gen_server:call(Orch, {state,empty,OrderType,CardType,Amount});
        false -> ok
    end,
    {reply, Result, #state{ queue= Q1 }=State};
handle_call(_request, _from, State) ->
    {reply, ok, State}.

handle_cast(_msg, State) ->
    {noreply, State}.
