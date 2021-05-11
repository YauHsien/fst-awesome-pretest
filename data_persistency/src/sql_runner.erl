-module(sql_runner).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export(
   [ start_link/1,
     registered_name/0
   ]).
-record(state, {owner,conn}).

registered_name() ->
    ?MODULE.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% ---- gen_server callbacks -------------------

init(Args) ->
    process_flag(trap_exit, true),
    OwnerTable = ets:new(owner_table, [set]),
    case proplists:get_value(owner, Args) of
        List when is_list(List) ->
            lists:map(
              fun(Owner)-> ets:insert(OwnerTable,{Owner,owner}) end,
              List);
        _ ->
            ok
    end,
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    Port     = proplists:get_value(db_port,  Args),
    {ok, Conn} = epgsql:connect(
                   #{host => Hostname,
                     database => Database,
                     username => Username,
                     password => Password,
                     port => Port
                    }),
    {ok, #state{owner=OwnerTable,conn=Conn}}.

%handle_call(Request, {From,[alias|_]}, #state{ owner= OwnerTable, conn= Conn }=State) ->
%    case ets:lookup(OwnerTable,From) of
%        [] ->
%            {reply, ok, State};
%        [{From,owner}|_] ->
%            {reply, handle(Request,Conn), State}
%    end;
handle_call({equery,Sql,Params}, _from, #state{ conn= Conn }=State) ->
    {reply, epgsql:equery(Conn,Sql,Params), State};
handle_call(_request, _from, State) ->
    {reply, ok, State}.

handle_cast(_msg, State) ->
    {noreply, State}.

handle_info(_msg, State) ->
    {noreply, State}.

terminate(_reason, #state{conn= Conn }) ->
    ok = epgsql:close(Conn),
    ok.

code_change(_old_version, State, _extra) ->
    {ok, State}.

%% ---- procedures ------------------------------

%handle({find,user,TS,Seq}, Conn) ->
%    "select \"timestamp\", seq from \"user\" where \"timestamp\" = $1 and seq = $2"
handle({find,user,TS,Seq}, Conn) ->
    GoodOrBad =
        case epgsql:equery(Conn, "select count(*) from \"user\" where \"timestamp\" = $1 and seq = $2", [TS,Seq]) of
            {ok,_columnSpecs,[{Count}]} when 0=:=Count ->
                                                %我就生一個 timestamp 與 sequence
            
                good;
            {ok,_columnSpecs,[{1}]} ->
                good;
            _ ->
                bad
    end,
    case GoodOrBad of
        good ->
            ok;
        bad ->
            bad_result
    end;
handle(_, _) ->
    ok.
