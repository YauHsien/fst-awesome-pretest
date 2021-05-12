-module(order).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([
  init_rand/0,
  new_order/1
]).
-include("../../include/constants.hrl").
-include("include/order.hrl").

%% ---- module services --------------------

init_rand() ->
    rand:seed(exsss, {erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()}).

new_order(TraderPid) ->
    Order = #order{ owner = TraderPid },
    TS = erlang:integer_to_list(Order#order.timestamp),
    Seq = erlang:integer_to_list(Order#order.sequence),
    Rand = erlang:integer_to_list(rand:uniform(255)),
    Id = erlang:list_to_atom(lists:flatten(["order_", TS, $., Seq, $., Rand])),
    gen_server:start({local,Id}, order, Order, []).

%% ---- gen_server callbacks ---------------

init(#order{} = Args) ->
    {ok, Args}.

handle_call({set,orderType,Case}, {TraderPid,[alias|_]}, #order{ owner = TraderPid }=State)
when Case == ?Buy orelse Case == ?Sell ->
    State1 = State#order { orderType = Case },
    {reply, State1, State1};
handle_call({set,currency,Case}, {TraderPid,[alias|_]}, #order{ owner = TraderPid }=State) when ?IsCurrency(Case) ->
    State1 = State#order { currency = Case },
    {reply, State1, State1};
handle_call({set,price,Case}, {TraderPid,[alias|_]}, #order{ owner = TraderPid }=State) when 1.0 =< Case andalso Case =< 10.0 ->
    State1 = State#order { price = Case},
    {reply, State1, State};
handle_call({set,cardType,Case}, {TraderPid,[alias|_]}, #order{ owner = TraderPid }=State) when ?IsCard(Case) ->
    State1 = State#order { cardType = Case },
    {reply, State1, State1};
handle_call(_request, _from, State) ->
    {reply, ok, State}.

handle_cast(_msg, State) ->
    {noreply, State}.

handle_info(_info, State) ->
    {noreply, State}.

terminate(_reason, _state) ->
    ok.
