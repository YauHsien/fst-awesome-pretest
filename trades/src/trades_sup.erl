%%%-------------------------------------------------------------------
%% @doc trades top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(trades_sup).

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
    OrchestratorName = orchestrator:registered_name(),
    OrchestratorSpec =
        #{ id => OrchestratorName,
           start => { gen_server,
                      start_link,
                      [{global,OrchestratorName}, orchestrator, [], []]},
           restart => permanent,
           shutdown => 50000,
           type => worker,
           modules => [gen_server]
         },

    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [OrchestratorSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
