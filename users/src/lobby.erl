-module(lobby).
-export(
   [
    status/0,
    find_user/2,
    find_user/3,
    draw_cards/3,
    place_order/8
   ]).
-define(USERS_MODULE_PID,
        helper:get_sup_child(users_sup,users_module)).
-include("../../include/constants.hrl").
-define(DEFAULT_CARDS_COUNT, 1).

status() ->
    {ok, 100}.

-spec find_user(
        Timestamp :: unix_timestamp(),
        Seq       :: sequence()
       ) ->
          {ok, Result :: binary()} |
          {error, Reason :: binary()}.

find_user(TS, Seq) ->
    find_user(TS, Seq, ?ORDERS_COUNT_LIMIT).

-spec find_user(
        Timestamp        :: unix_timestamp(),
        Seq              :: sequence(),
        OrdersCountLimit :: ?ORDERS_COUNT_LIMIT | integer()
       ) ->
          { ok,    Result :: binary() } |
          { error, Reason :: binary() }.

find_user(TS, Seq, OrdersCountLimit) ->
    try gen_server:call(?USERS_MODULE_PID,
                         {claim,user,TS,Seq,OrdersCountLimit}) of
        {ok, UserId} ->
            case gen_server:call(whereis(UserId), {user,serialized}) of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    {error, ?ERROR(Reason)}
            end;
        {error, Reason} ->
            {error, ?ERROR(Reason)}
    catch
        Class : Exception : Trace ->
            logger:error("~p: ~p~n~p", [Class,Exception,Trace]),
            {error, {Class,Exception}}
    end.

-spec draw_cards(
        Timestamp  :: unix_timestamp(),
        Seq        :: sequence(),
        CardsCount :: ?DEFAULT_CARDS_COUNT | integer()
       ) ->
          { ok,    Result :: binary() } |
          { error, Reason :: binary() }.

draw_cards(TS, Seq, CardsCount) ->
    case find_user(TS,Seq) of
        {ok, UserPid} ->
            case gen_server:call(UserPid, {draw,cards,CardsCount}) of
                {ok, ListOfString} when is_list(ListOfString) ->
                    Result =
                        << "{\"cards\":[",
                           (erlang:list_to_binary(lists:join($,, ListOfString)))/binary,
                           "]" >>,
                    {ok, Result};
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    {error, ?ERROR(Reason)}
            end;
        {error, Reason} ->
            {error, ?ERROR(Reason)}
    end.

-spec place_order(
        U_TS    :: unix_timestamp(),
        U_seq   :: sequence(),
        C_TS    :: unix_timestamp(),
        U_seq   :: sequence(),
        C_type  :: ?Pikachu | ?Bulbasaur | ?Charmander | ?Squirtle,
        Current :: ?USD,
        Price   :: price_amount(),
        O_type  :: ?Buy | ?Sell
       ) ->
          { ok,    Result :: binary() } |
          { error, Reason :: binary() }.

place_order(U_TS, U_seq, C_TS, C_seq, C_type, Currency, Price, O_type) ->
    case find_user(U_TS,U_seq) of
        {ok, UserPid} ->
            case gen_server:call(
                   UserPid,
                   {place, order,
                    C_TS, C_seq, C_type, Currency, Price, O_type})
            of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    {error, ?ERROR(Reason)}
            end;
        {error, Reason} ->
            {error, ?ERROR(Reason)}
    end.
