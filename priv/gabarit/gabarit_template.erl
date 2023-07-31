%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module('@module_name').
-export([id/0, version/0, created_at/0, updated_at/0]).
-export([path/0, path/1, template/0]).
-export([format_opts/0, format/1, format/2]).
-export([render_opts/0, render/1, render/2]).
-include_lib("syntax_tools/include/merl.hrl").

% special attributes
-vsn('@vsn').
-gabarit('@module_version').
-created_at('@created_at').
-updated_at('@updated_at').

%%--------------------------------------------------------------------
%% @doc Returns the special attributes.
%% @end
%%--------------------------------------------------------------------
id() -> '@module_name'.
version() -> '@module_version'.
created_at() -> '@created_at'.
updated_at() -> '@updated_at'.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec path() -> string().
path() -> path(relative).

-spec path(atom) -> string().
path(relative) -> '@relative_path';
path(absolute) -> '@absolute_path'.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
% -spec reload() -> {ok, atom()}.
% reload() -> gabarit:reload('@module_name').

%%--------------------------------------------------------------------
%% @doc Returns the raw template.
%% @end
%%--------------------------------------------------------------------
-spec template() -> binary().
template() -> '@template'.

%%--------------------------------------------------------------------
%% @doc Returns the default format_opts used.
%% @end
%%--------------------------------------------------------------------
-spec format_opts() -> list().
format_opts() -> '@format_opts'.

%%--------------------------------------------------------------------
%% @doc Render the template and print it to stdout.
%% @end
%%--------------------------------------------------------------------
-spec format(Context) -> Return when
      Context :: term(),
      Return :: ok.
format(Context) -> 
    Opts = format_opts(),
    format(Context, Opts).

%%--------------------------------------------------------------------
%% @doc Render the template and print it to stdout.
%% @end
%%--------------------------------------------------------------------
-spec format(Context, Opts) -> Return when
      Context :: term(),
      Opts :: term(),
      Return :: ok.
format(Context, Opts) ->
    Rendering = render(Context, Opts),
    io:format("~s", [Rendering]).
    
%%--------------------------------------------------------------------
%% @doc Returns the default render_opts used.
%% @end
%%--------------------------------------------------------------------
-spec render_opts() -> list().
render_opts() -> '@render_opts'.

%%--------------------------------------------------------------------
%% @doc Render the template and return it.
%% @end
%%--------------------------------------------------------------------
-spec render(Context) -> Return when
      Context :: term(),
      Return :: any().
render(Context) -> 
    Opts = render_opts(),
    render(Context, Opts).
    
%%--------------------------------------------------------------------
%% @doc Render the template and return it.
%% @end
%%--------------------------------------------------------------------
-spec render(Context, Opts) -> Return when
      Context :: term(),
      Opts :: term(),
      Return :: any().
render(Context, Opts) ->
    Template = template(),
    erlang:apply('@callback_module', '@callback_function', [Template, Context, Opts]).

