-module(helper).
-export(
   [ node_data_persistency/0,
     node_orders/0,
     get_sup_child/2,
     bin/1
   ]).
-include("include/constants.hrl").

node_data_persistency() ->
    NodeNameString = os:getenv(?NODE_DATA_PERSISTENCY),
    NodeName = erlang:list_to_atom(NodeNameString),
    case net_adm:ping(NodeName) of
        pang ->
            logger:critical("ping: ~p returned pang", [NodeNameString]),
            nil;
        pong ->
            NodeName
    end.

node_orders() ->
    NodeNameString = os:getenv(?NODE_ORDERS),
    NodeName = erlang:list_to_atom(NodeNameString),
    case net_adm:ping(NodeName) of
        pang ->
            logger:critical("ping: ~p returned pang", {NodeNameString}),
            nil;
        pong ->
            NodeName
    end.

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
