-module(users_module).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([
  registered_name/0
]).
-include("../../include/constants.hrl").
-record(state, {}).

registered_name() ->
    ?MODULE.

%% ---- gen_server callbacks ---------------

init(_args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_info({'EXIT',Pid,normal}, State) when Pid /= self() ->
    logger:alert("~p: {'EXIT',~p,normal} received", [?MODULE,Pid]),
    {noreply, State}.

handle_call({claim,user,TS,Seq,OrdersCountLimit}, _from, State) ->
    {reply, claim_user(TS,Seq,OrdersCountLimit), State};
handle_call(_request, _from, State) ->
    {reply, ok, State}.

handle_cast(_msg, State) ->
    {noreply, State}.

% ----- internal ----------------------------------

-spec claim_user(
        Timestamp        :: unix_timestamp(),
        Seq              :: sequence(),
        OrdersCountLimit :: ?ORDERS_COUNT_LIMIT | integer()
       ) ->
          { ok, UserId :: atom() } |
          { error, Reason :: term() }.

claim_user(Timestamp, Seq, OrdersCountLimit) ->
    UserId = user_lib:build_id(Timestamp, Seq),
    case whereis(UserId) of
        undefined ->
            case rpc:call(helper:node_data_persistency(),
                          lobby, find_users, [Timestamp,Seq])
            of
                {ok, []} ->
                    case rpc:call(helper:node_data_persistency(),
                                  lobby, insert_user, [Timestamp,Seq])
                    of
                        {error, _reason} = BadResult ->
                            BadResult;
                        {badrpc, _reason} = BadResult ->
                            {error, BadResult};
                        _ ->
                            try
                                gen_server:start(
                                  {local,UserId},
                                  user_, [Timestamp,Seq,OrdersCountLimit], [])
                            of
                                {ok, _pid} ->
                                    UserId;
                                {stop, Reason} ->
                                    {error, Reason};
                                {error, _reason} = BadResult ->
                                    BadResult
                            catch
                                Class : Exception : Trace ->
                                    logger:error("~p: ~p~n~p",
                                                 [Class,Exception,Trace]),
                                    {error, {Class,Exception,Trace}}
                            end
                    end;
                {error, _reason} = BadResult ->
                    BadResult;
                {badrpc, _reason} = BadResult ->
                    {error, BadResult}
            end;
        UserPid ->
            {ok, UserPid}
    end.
