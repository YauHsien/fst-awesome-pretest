-module(helper).
-export(
   [ get_sup_child/2,
     bin/1
   ]).

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
