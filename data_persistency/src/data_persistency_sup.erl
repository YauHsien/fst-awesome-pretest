%%%-------------------------------------------------------------------
%% @doc data_persistency top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(data_persistency_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->

    PoolSize = helper:to_integer(os:getenv("POOLSIZE"), 30),
    PoolName = pool1,
    PoolSpec =
        poolboy:child_spec(
          PoolName,
          [{name, {local,PoolName}},
           {worker_module, sql_runner},
           {size, PoolSize},
           {max_overflow, PoolSize}
          ],
          [{hostname, os:getenv("DB_HOST")},
           {database, os:getenv("DATABASE")},
           {username, os:getenv("DB_USER")},
           {password, os:getenv("DB_PASS")},
           {db_port,
            helper:to_integer(os:getenv("DB_PORT"),5432)
           }]),

    SupFlags = #{strategy => one_for_one, intensity => 1, period => 1},
    ChildSpecs = [PoolSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
