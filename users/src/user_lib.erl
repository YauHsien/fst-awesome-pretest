-module(user_lib).
-export(
   [ get_id/1,
     build_id/2
   ]).
-include("../include/constants.hrl").
-include("include/user.hrl").

get_id(#user{} = User) ->
    TS = erlang:integer_to_list(User#user.timestamp),
    Seq = erlang:integer_to_list(User#user.sequence),
    Rand = erlang:integer_to_list(rand:uniform(255)),
    erlang:list_to_atom(lists:flatten(["user_", TS, $., Seq, $., Rand])).

build_id(Timestamp, Seq) ->
    erlang:list_to_atom(
      lists:flatten(
        ["user_", $_,
         erlang:integer_to_list(Timestamp), $.,
         erlang:integer_to_list(Seq)])).
