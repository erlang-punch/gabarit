%%%===================================================================
%%% @author Maartz <maartz@protonmail.com>
%%%
%%% @doc Main interface for the Gabarit template management system.
%%%
%%% This module provides a public API for interacting with the Gabarit
%%% template system, including:
%%%  - Namespace management for organizing templates
%%%  - Template creation, retrieval, and management
%%%  - Template versioning and version control
%%%  - Template rendering and formatting with context data
%%%
%%% Gabarit allows for efficient storage, versioning, and rendering of
%%% templates with support for namespaces, compression, and version control.
%%% @end
%%%===================================================================
-module(gabarit).

-export([
    create_namespace/1,
    create_namespace/2,
    list_namespaces/0,
    reset_namespace/2
]).

-export([
    new/1,
    new_template/1,
    new_template/2,
    add_template/3,
    get_template/1,
    get_template_version/2,
    update_template/2,
    compile_template/2,
    list_templates/0,
    list_templates/1,
    list_versions/1,
    rollback/2
]).

-export([
    template/1,
    format/2,
    format/3,
    render/2,
    render/3
]).

%%====================================================================
%% Namespace Management Functions
%%====================================================================

%% @doc Creates a new namespace with a specified template limit.
%%
%% @param Namespace The namespace identifier
%% @returns ok | {error, Reason}
%% @end
-spec create_namespace(Namespace) -> Return when
      Namespace :: string() | binary() | atom(),
      Return :: ok | {error, term()}.
create_namespace(Namespace) ->
    create_namespace(Namespace, #{}).

%% @doc Creates a new namespace with a specified template limit.
%%
%% @param Namespace The namespace identifier
%% @param Opts A map of options: Default maximum number of templates allowed in the namespace
%% @returns ok | {error, Reason}
%% @end
-spec create_namespace(Namespace, Opts) -> Return when
      Namespace :: string() | binary() | atom(),
      Opts :: map(),
      Return :: ok | {error, term()}.
create_namespace(Namespace, Opts) ->
    Limit = maps:get(limit, Opts, infinity),
    gabarit_store:add_namespace(Namespace, Limit).

%% @doc Lists all available namespaces.
%%
%% @returns {ok, [{Namespace, Limit}]} | {error, Reason}
%% @end
-spec list_namespaces() -> Return when
      Return :: {ok, [{term(), non_neg_integer()}]} | {error, term()}.
list_namespaces() ->
    gabarit_store:list_namespaces().

%% @doc Lists all templates across all namespaces.
%%
%% @returns {ok, [{TemplateId, Path}]} | {error, Reason}
%% @end
-spec list_templates() -> Return when
      Return :: {ok, [{term(), string()}]} | {error, term()}.
list_templates() ->
    gabarit_store:list_templates().

%% @doc Lists all templates in a specific namespace.
%%
%% @param Namespace The namespace to list templates from
%% @returns {ok, [{TemplateId, Path}]} | {error, Reason}
%% @end
-spec list_templates(Namespace) -> Return when
      Namespace :: string() | binary() | atom(),
      Return :: {ok, [{term(), string()}]} | {error, term()}.
list_templates(Namespace) ->
    gabarit_store:list_templates(Namespace).

%% @doc Lists all versions of a template.
%%
%% @param TemplateId The template identifier
%% @returns {ok, [{Version, Timestamp}]} | {error, Reason}
%% @end
list_versions(TemplateId) ->
    gabarit_store:list_versions(TemplateId).

%%====================================================================
%% Template Creation and Management Functions
%%====================================================================

%% @doc Creates a new template from a file in the default template path.
%%
%% @param Filename The template file name
%% @returns {ok, Module} | {error, Reason}
%% @example
%%   ```
%%   {ok, Module} = gabarit:new("index.html").
%%   '''
%% @end
-spec new(Filename) -> Return when
      Filename :: string() | binary(),
      Return :: {ok, Module},
      Module :: atom().
new(Filename) ->
    Template = gabarit_compiler:template_file(Filename),
    gabarit_compiler:compile_file(Template).

%% @doc Creates a new template with default options.
%%
%% @param Filename The template file name
%% @returns {ok, TemplateId} | {error, Reason}
%% @end
new_template(Filename) ->
    new_template(Filename, []).

%% @doc Creates a new template with advanced options.
%%
%% @param Filename The template file name
%% @param Opts Options list, supports [{namespace, Namespace}]
%% @returns {ok, TemplateId} | {error, Reason}
%% @end
new_template(Filename, Opts) ->
    Namespace = proplists:get_value(namespace, Opts, default),
    Template = gabarit_compiler:template_file(Filename),
    Content = maps:get(template, Template, <<>>),
    Path = maps:get(name, Template, Filename),
    case gabarit_store:add_template(Namespace, Path, Content) of
        {ok, TemplateId} ->
            TemplateMap = Template#{name => TemplateId},
            compile_template(TemplateId, TemplateMap);
        Error ->
            Error
    end.

%% @doc Adds a template directly with content.
%%
%% @param Namespace The namespace to add the template to
%% @param TemplatePath The template path/identifier
%% @param Content The template content
%% @returns {ok, TemplateId} | {error, Reason}
%% @end
add_template(Namespace, TemplatePath, Content) ->
    gabarit_store:add_template(Namespace, TemplatePath, Content).

%% @doc Retrieves the content of a template by its ID.
%%
%% @param TemplateId The template identifier
%% @returns {ok, Content} | {error, Reason}
%% @end
get_template(TemplateId) ->
    gabarit_store:get_template(TemplateId).

%% @doc Updates an existing template.
%%
%% @param TemplateId The template identifier
%% @param Content The new template content
%% @returns {ok, NewVersion} | {error, Reason}
%% @end
update_template(TemplateId, Content) ->
    gabarit_store:update_template(TemplateId, Content).

%% @doc Compiles a template into an Erlang module.
%%
%% @param TemplateId The template identifier
%% @param Template The template structure
%% @returns {ok, Module} | {error, Reason}
%% @end
compile_template(TemplateId, Template) ->
  gabarit_store:compile_template(TemplateId, Template).

%% @doc Rolls back to a previous version of a template.
%%
%% @param TemplateId The template identifier
%% @param Version The version to roll back to
%% @returns {ok, Result} | {error, Reason}
%% @end
rollback(TemplateId, Version) ->
  gabarit_store:rollback(TemplateId, Version).

%% @doc Retrieves a specific version of a template.
%%
%% @param TemplateId The template identifier
%% @param Version The version number to retrieve
%% @returns {ok, Content} | {error, Reason}
%% @end
-spec get_template_version(TemplateId, Version) -> Result when
    TemplateId :: term(),
    Version :: integer(),
    Result :: {ok, binary()} | {error, term()}.
get_template_version(TemplateId, Version) ->
  gabarit_store:get_template_version(TemplateId, Version).

%%====================================================================
%% Template Rendering Functions
%%====================================================================

%% @doc Retrieves the raw content of a template.
%%
%% @param Filename The template file name
%% @returns Binary content of the template
%% @end
template(Filename) ->
    ModuleName = gabarit_compiler:find_template_file_module(Filename),
    ModuleName:template().

%% @doc Formats a template with the given context.
%%
%% @param Filename The template file name
%% @param Context The data context for formatting
%% @returns Formatted template as binary
%% @end
format(Filename, Context) ->
    format(Filename, Context, []).

%% @doc Formats a template with the given context and options.
%%
%% @param Filename The template file name
%% @param Context The data context for formatting
%% @param Opts Formatting options
%% @returns Formatted template as binary
%% @end
format(Filename, Context, Opts) ->
    ModuleName = gabarit_compiler:find_template_file_module(Filename),
    ModuleName:format(Context, Opts).

%% @doc Renders a template with the given context.
%%
%% @param Filename The template file name
%% @param Context The data context for rendering
%% @returns Rendered template as binary
%% @end
render(Filename, Context) ->
    render(Filename, Context, []).

%% @doc Renders a template with the given context and options.
%%
%% @param Filename The template file name
%% @param Context The data context for rendering
%% @param Opts Rendering options
%% @returns Rendered template as binary
%% @end
render(Filename, Context, Opts) ->
    ModuleName = gabarit_compiler:find_template_file_module(Filename),
    ModuleName:render(Context, Opts).

%% @doc Resets a namespace to have 0 templates and optionally updates its limit.
%% @param Namespace The namespace to reset
%% @param NewLimit Optional new limit (use 'keep' to retain the current limit)
%% @returns ok | {error, Reason}
%% @end
reset_namespace(Namespace, NewLimit) ->
    gabarit_store:reset_namespace(Namespace, NewLimit).
