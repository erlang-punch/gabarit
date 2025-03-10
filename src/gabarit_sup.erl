%%%-------------------------------------------------------------------
%% @doc gabarit top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(gabarit_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    %% TODO: which option is better for the application
    StoreOpts = case application:get_env(gabarit, persistence, none) of
                    none -> [];
                    dets ->
                        BaseDir = application:get_env(gabarit, dets_dir, "priv/gabarit_store"),
                        [{persistence_module, gabarit_dets_persistence}, {persistence_dir, BaseDir}];
                    mnesia ->
                        [{persistence_module, gabarit_mnesia_persistence}]
                end,

    ChildSpecs = [
        #{id => gabarit_store,
          start => {gabarit_store, start_link, [StoreOpts]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [gabarit_store]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
