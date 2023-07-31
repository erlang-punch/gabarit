%%%-------------------------------------------------------------------
%% @doc gabarit public API
%% @end
%%%-------------------------------------------------------------------

-module(gabarit_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gabarit_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
