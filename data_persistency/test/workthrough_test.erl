-module(workthrough_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/include/database.hrl").

workaround_test() ->
    IncOrd =
        {incomplete_order, 1, 2, 3, 4, 5, 6, 7, 8.9 },
    {I1, P1} =
        ?INSERT_RECORD_INCOMPLETE_ORDER(IncOrd),
    helper:query(I1, P1).
