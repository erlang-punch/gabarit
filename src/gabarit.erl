%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(gabarit).
-export([new/1]).
-export([template/1]).
-export([format/2, format/3]).
-export([render/2, render/3]).

%%--------------------------------------------------------------------
%% @doc Create a new template from file present in default template
%% path.
%%
%% == Example ==
%%
%% ```
%% {ok, Module} = gabaric:new("index.html").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Filename) -> Return when
      Filename :: string() | binary(),
      Return :: {ok, Module},
      Module :: atom().
new(Filename) ->
    Template = gabarit_compiler:template_file(Filename),
    gabarit_compiler:compile_file(Template).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
template(Filename) ->
    ModuleName = gabarit_compiler:find_template_file_module(Filename),
    ModuleName:template().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
format(Filename, Context) ->
    format(Filename, Context, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
format(Filename, Context, Opts) ->
    ModuleName = gabarit_compiler:find_template_file_module(Filename),
    ModuleName:format(Context, Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
render(Filename, Context) ->
    render(Filename, Context, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
render(Filename, Context, Opts) ->
    ModuleName = gabarit_compiler:find_template_file_module(Filename),
    ModuleName:render(Context, Opts).
