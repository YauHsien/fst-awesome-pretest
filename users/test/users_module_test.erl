-module(users_module_test).
-include_lib("eunit/include/eunit.hrl").
-include("../../include/constants.hrl").
-include("../src/include/user.hrl").

% Here an Important Messaging Mechanism
%
% When the process {local,user_}, linked to another process, is asked
% to exit normally, it will relay the message {'EXIT', _, normal} to
% the linked process.
%
% The messaging mechanism is used the users_module, which is a process
% registry and keeps track of user processes.
% -----------------------------
management_test() ->
    {ok, Manager} =
        gen_server:start_link({local,users_module1}, users_module, [], []),
    User = #user{},

    {ok, User1} =
        gen_server:call(
          Manager,
          {claim, user, User#user.timestamp, User#user.sequence}),
    Pid1 = io_lib:format("~p", [User1]),
    exit(User, normal),

    {ok,User2} =
        gen_server:call(
          Manager,
          {claim, user, User#user.timestamp, User#user.sequence}),
    Pid2 = io_lib:format("~p", [User2]),

    ?assertNotMatch(Pid2, Pid1).
