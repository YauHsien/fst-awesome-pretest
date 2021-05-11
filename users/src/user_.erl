-module(user_).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-include("../../include/constants.hrl").
-include("include/user.hrl").
-record(
   state,
   { user = #user{} :: #user{}
   }).

% ---- gen_server callbacks ---------------

init([Timestamp, Sequence, OrdersCountLimit]) ->
    Me = #user{ timestamp= Timestamp, sequence= Sequence },
    case rpc:call(helper:node_data_persistency(),
                  lobby, find_orders_tracked,
                  [Timestamp, Sequence, OrdersCountLimit])
    of
        {ok, ListOfOrders} ->
            OrderIdList =
                lists:filter(
                  fun({ok, {_owts,_owsq,_orts,_orsq}= _order_id}) -> true;
                     (_) -> false
                  end,
                  lists:map(
                    fun(Record) -> rpc:call(helper:node_orders(),
                                            lobby, claim_order,
                                            [Record,{user,holds}])
                    end,
                    ListOfOrders)),
            {ok, Me#user{ orders= OrderIdList }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({user,serialize}, _from, #state{ user= User }=State) ->
    {reply, serialize(User), State};
handle_call({draw,cards,CardsCount}, _from, #state{ user= User }=State) ->
    {reply, draw_cards(CardsCount,User), State};
handle_call(
  {place,order,C_TS,C_seq,C_type,Currency,Price,O_type},
  _from,
  #state{ user= User }= State
 ) ->
    {reply, place_order(C_TS,C_seq,C_type,Currency,Price,O_type,User), State};
handle_call(_request, _from, State) ->
    {reply, {ok, ignored}, State}.

handle_cast(_msg, State) ->
    {noreply, State}.

% ---- internal --------------------------

-spec serialize(
        User :: tuple()
       ) ->
          {ok, Result :: binary()} |
          {error, Reason :: term()}.

serialize(
  #user{ timestamp= Timestamp, sequence= Sequence, orders= OrderIdList }
 ) ->
    OrdersSer =
        lists:foldl(
          fun({OwTS, OwSeq, OrTS, OrSeq}, Acc) ->
                  case rpc:call(helper:node_orders(),
                                lobby, serialize_order, [OwTS, OwSeq, OrTS, OrSeq])
                  of
                      {badrpc,_reason} = BadResult ->
                          {error, BadResult};
                      {error, Reason} ->
                          {error, Reason};
                      {ok, Binary} ->
                          << Binary/binary, $,, Acc/binary>>
                  end
          end,
          <<>>,
          OrderIdList),
    {ok, <<"{\"user\":{\"timestamp\":", (helper:bin(Timestamp))/binary,
            ",\"seq\":", (helper:bin(Sequence))/binary, ",\"orders\":[",
            OrdersSer/binary, "]}}"
         >> }.

-spec draw_cards(
        Count :: integer(),
        User :: tuple()
       ) ->
          {ok, Result :: binary()} |
          {error, Reason :: term()}.

-spec place_order(
        C_TS :: unix_timestamp(),
        C_seq :: sequence(),
        C_type :: ?Pikachu | ?Bulbasaur | ?Charmander | ?Squirtle,
        Currency :: ?USD,
        Price :: price_amount(),
        O_type :: ?Buy | ?Sell,
        User :: tuple()
       ) ->
          {ok, Result :: binary()} |
          {error, Reason :: term()}.
