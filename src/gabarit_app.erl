%%%-------------------------------------------------------------------
%% @doc gabarit public API
%% @end
%%%-------------------------------------------------------------------
-module(gabarit_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ensure_templates_dir(),
    gabarit_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

%% @doc Ensures that the templates directory exists
%% @private
ensure_templates_dir() ->
    TemplatesPath = application:get_env(gabarit, templates_path, "priv/templates"),
    case filelib:is_dir(TemplatesPath) of
        true -> ok;
        false ->
            filelib:ensure_dir(filename:join(TemplatesPath, "dummy")),
            file:make_dir(TemplatesPath)
    end.
