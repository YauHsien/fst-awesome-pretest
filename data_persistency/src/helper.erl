-module(helper).
-export(
   [ get_sup_child/2,
     bin/1,
     to_integer/2,
     query/2,
     translate/1
   ]).
-include("../../include/constants.hrl").

get_sup_child(Sup, ChildId) ->
    case lists:filter(fun({Id,_,_,_}) when ChildId == Id -> true;
                         (_)-> false end,
                      supervisor:which_children(Sup)) of
        [{_,Pid,_,_}|_] ->
            Pid;
        [] ->
            nil
    end.

bin(Msg) when is_atom(Msg) ->
    atom_to_binary(Msg);
bin(Msg) when is_integer(Msg) ->
    integer_to_binary(Msg);
bin(Msg) when is_float(Msg) ->
    float_to_binary(Msg);
bin(Msg) when is_list(Msg) ->
    list_to_binary(Msg);
bin(Msg) ->
    Msg.

to_integer(String, Default) when is_list(String) ->
    case string:to_integer(String) of
        {error, _reason} ->
            Default;
        {Int, _rest} ->
            Int
    end;
to_integer(Atom, _) when is_atom(Atom) ->
    Atom;
to_integer(_, Int) ->
    Int.

query(Sql, Params) ->
    try
        poolboy:transaction(
          pool1,
          fun(Worker) -> gen_server:call(Worker, {equery, Sql, Params}) end
         )
    of
        {ok, N} when is_integer(N) ->
            {ok, N};
        {ok, _columns_spec, Rows} ->
            {ok, Rows};
        {error, {error,_extra,IntCod,Mean,Message,_trace}} ->
            {error, {e,IntCod,Mean,Message}};
        Any ->
            {not_imp, Any}
    catch
        Class : Exception : Trace ->
            {error, {ex,Class,Exception,Trace}}
    end.

translate(?Buy)        -> 1; translate(1) -> ?Buy;
translate(?Sell)       -> 2; translate(2) -> ?Sell;
translate(?USD)        -> 3; translate(3) -> ?USD;
translate(?Pikachu)    -> 4; translate(4) -> ?Pikachu;
translate(?Bulbasaur)  -> 5; translate(5) -> ?Bulbasaur;
translate(?Charmander) -> 6; translate(6) -> ?Charmander;
translate(?Squirtle)   -> 7; translate(7) -> ?Squirtle.
