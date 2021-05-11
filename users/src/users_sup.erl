%%%-------------------------------------------------------------------
%% @doc users top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(users_sup).

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

    BackendModuleName = backend_module:registered_name(),
    BackendModuleSpec =
        #{ id => BackendModuleName,
           start => { gen_server,
                      start_link,
                      [{global,BackendModuleName},backend_module,[],[]]},
           restart => permanent,
           shutdown => 50000,
           type => worker,
           modules => [gen_server]
         },

    UsersModuleName = users_module:registered_name(),
    UsersModuleSpec =
        #{ id => UsersModuleName,
           start => { gen_server,
                      start_link,
                      [{global,UsersModuleName},users_module,[],[]]},
           restart => permanent,
           shutdown => 50000,
           type => worker,
           modules => [gen_server]
         },

    SupFlags = #{strategy => one_for_one, intensity => 0, period => 1},
    ChildSpecs = [BackendModuleSpec, UsersModuleSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
