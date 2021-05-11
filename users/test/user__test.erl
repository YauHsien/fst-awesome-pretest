-module(user__test).
-include_lib("eunit/include/eunit.hrl").

% Here an Important Messaging Mechanism
%
% When the process {local,user_}, linked to another process, is asked
% to exit normally, it will relay the message {'EXIT', _, normal} to
% the linked process.
%
% The messaging mechanism is used the users_module, which is a process
% registry and keeps track of user processes.
% -----------------------------
exit_normal_test() ->
    process_flag(trap_exit, true),
    {ok,Pid} = gen_server:start_link({local,user_}, user_, [], []),
    exit(Pid, normal),
    Result =
        receive
            {'EXIT',Pid,normal} ->
                {ok, normal}
        after
            500 ->
                timeouted
        end,
    ?assertMatch({ok,normal}, Result).

exit_kill_test() ->
    process_flag(trap_exit, true),
    {ok,Pid} = gen_server:start_link({local,user_}, user_, [], []),
    exit(Pid, kill),
    Result =
        receive
            {'EXIT',Pid,kill} ->
                {ok,kill}
        after
            500 ->
                timeouted
        end,
    ?assertMatch(timeouted, Result).
