%%%-------------------------------------------------------------------
%% @doc gabarit top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(gabarit_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-export([get_persistence_options/0, gabarit_store_spec/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    StoreOpts = get_persistence_options(),
    ChildSpecs = [gabarit_store_spec(StoreOpts)],
    {ok, {SupFlags, ChildSpecs}}.

%% @doc Gets persistence options based on application configuration.
%% @returns List of options to pass to the gabarit_store.
-spec get_persistence_options() -> list().
get_persistence_options() ->
    case application:get_env(gabarit, persistence, none) of
        none ->
            [];
        dets ->
            BaseDir = application:get_env(gabarit, dets_dir, "priv/gabarit_store"),
            [{persistence_module, gabarit_dets_persistence},
             {persistence_dir, BaseDir}];
        mnesia ->
            [{persistence_module, gabarit_mnesia_persistence}]
    end.

%% @doc Creates a child spec for the gabarit_store process.
%% @param StoreOpts Options to pass to the store when starting it.
%% @returns A supervisor child specification.
-spec gabarit_store_spec(list()) -> supervisor:child_spec().
gabarit_store_spec(StoreOpts) ->
    #{id => gabarit_store,
      start => {gabarit_store, start_link, [StoreOpts]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [gabarit_store]}.
