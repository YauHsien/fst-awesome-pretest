-module(lobby).
-export(
   [ claim_order/2,
     serialize_order/4
   ]).

-spec claim_order(
        {OwTS, OwSeq, OrTS, OrSeq} :: tuple(),
        Flag :: {user,holds}
       ) ->
          {ok, {OwTS, OwSeq, OrTS, OrSeq}} |
          {error, Reason :: term()}.

-spec serialize_order(
        OwTS :: unix_timestamp(),
        OwSeq :: sequence(),
        OrTS :: unix_timestamp(),
        OrSeq :: sequence()
       ) ->
          {ok, Result :: binary()} |
          {error, Reason :: term()}.
