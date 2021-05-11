-type unix_timestamp() :: integer().
-type sequence() :: integer().
-type price_amount() :: float().
-type count() :: integer().
-define(UnixEpoch, 62167219200).
-define(CommonRNGRange, 25535). %% Common random number generation range.
-define(InitRand(), rand:seed(exsss, {erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()})).
-define(Pikachu, 'Pikachu').
-define(Bulbasaur, 'Bulbasaur').
-define(Charmander, 'Charmander').
-define(Squirtle, 'Squirtle').
-define(IsCard(C), (
   C == 'Pikachu' orelse
   C == 'Bulbasaur' orelse
   C == 'Charmander' orelse
   C == 'Squirtle'
)).
-define(USD, 'USD').
-define(IsCurrency(C), (
   C == 'USD'
)).
-define(Buy, buy).
-define(Sell, sell).
-define(ORDERS_COUNT_LIMIT, 50).
-define(TRADES_COUNT_LIMIT, 50).
-define(CARDS_TO_DRAW, 1).
-define(ERROR(Msg),
        << "{\"errors\":{\"detail\":\"", (helper:bin(Msg))/binary, "\"}}" >>).
-define(CHECK_RECORD(Record,RecordName),
        (is_tuple(Record) andalso
         size(Record) >= 1 andalso
         erlang:element(1,Record) == RecordName)
       ).
