%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(gabarit_compiler).
-compile(export_all).
-export([path/0, tree/0, tree/1]).
-define(DEFAULT_PATH, "priv/templates").
-define(DEFAULT_MERL_TEMPLATE, "priv/gabarit/gabarit_template.erl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
compile(#{ name := Identifier } = Struct) ->
    ModuleName = create_module_name("gabarit@", Identifier),
    merl_compile_and_load(Struct#{ module_name => ModuleName }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
find_and_compile() ->
    Tree = tree(),
    lists:map(fun compile/1, Tree).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
map_compile(Struct) ->
    case compile(Struct) of
	{ok, Compiled} -> Struct#{ module => Compiled };
	Error -> Struct#{ module => Error }
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
create_module_name(Prefix, Identifier) ->
    ModuleName = string:concat(Prefix, Identifier),
    try
	erlang:list_to_existing_atom(ModuleName)
    catch
	_:_ -> erlang:list_to_atom(ModuleName)
    end.

%%--------------------------------------------------------------------
%% @doc list the current path used to load the templates.
%% @end
%%--------------------------------------------------------------------
path() -> 
    application:get_env(awesome, templates_path, ?DEFAULT_PATH).

%%--------------------------------------------------------------------
%% @doc list all templates in default templates path.
%% @end
%%--------------------------------------------------------------------
tree() -> 
    Tree = tree(path()),
    lists:foldl(fun tree_filter/2, [], Tree).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
tree(Path) -> tree(Path, []).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
tree(Path, Buffer) ->
    case file:list_dir(Path) of
	{ok, Files} ->
	    tree(Path, Files, Buffer);
	{error, Error} -> 
	    throw({error, Error})
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
tree(_, [], Buffer) -> Buffer;
tree(Path, [File|Files], Buffer) ->
    FilePath = filename:join([Path, File]),
    case filelib:is_dir(FilePath) of
        true -> tree(FilePath, Buffer);
        false -> tree(Path, Files, [{Path, FilePath}|Buffer])
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc paths filtering.
%% @end
%%--------------------------------------------------------------------
tree_filter({Key, Value}, Acc) ->
    Origin = filename:split(Key),
    Root = filename:split(path()),
    Filename = lists:last(filename:split(Value)),
    AbsolutePath = filename:absname(Value),
    Subtract = lists:subtract(Origin, Root),
    Identifier = case Subtract of
		     [] -> filename:join(["/", Filename]);
		     _ -> filename:join(["/", Subtract, Filename])
		 end,
    {ok, Content} = file:read_file(AbsolutePath),
    [#{ name => Identifier
      , filename => Filename
      , absolute_path => AbsolutePath
      , relative_path => Key
      , template => Content
      }|Acc].

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_template() -> 
    merl_template(?DEFAULT_MERL_TEMPLATE).
     
%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_template(TemplateFile) -> 
    case file:read_file(TemplateFile) of
	{ok, Content} ->
	    Content;
	Elsewise -> throw(Elsewise)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_compile_and_load(Opts) ->
    Template = merl_template(),
    case merl_qquote(Template, Opts) of
	{ok, AST} ->
	    merl:compile_and_load(AST);
	Error -> Error
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
merl_qquote(Template, Opts) ->
    Quote = merl:qquote(Template, []),
    merl_subst_module_name(Quote, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_module_name(Template, #{ module_name := ModuleName } = Opts) ->
    Term = merl:term(ModuleName),
    Result = merl:subst(Template, [{module_name, Term}]),
    merl_subst_module_version(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_module_version(Template, #{ module_name := ModuleName } = Opts) ->
    Version = case erlang:module_loaded(ModuleName) of
		  true ->
		      LoadedVersion = ModuleName:version(),
		      maps:get(module_version, Opts, LoadedVersion);
		  false -> 0
	      end,
    Term = merl:term(Version+1),
    Result = merl:subst(Template, [{module_version, Term}]),
    merl_subst_path(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_path(Template, #{ relative_path := RelativePath
			   , absolute_path := AbsolutePath } = Opts) ->
    Result = merl:subst(Template, [{relative_path, merl:term(RelativePath)}
				  ,{absolute_path, merl:term(AbsolutePath)}]),
    merl_subst_vsn(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_vsn(Template, Opts) ->
    Version = maps:get(vsn, Opts, undefined),
    Term = merl:term(Version),
    Result = merl:subst(Template, [{vsn, Term}]),
    merl_subst_created_at(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_created_at(Template, #{ module_name := ModuleName } = Opts) ->
    CreatedAt = case erlang:module_loaded(ModuleName) of
		    true ->
			Original = ModuleName:created_at(),
			maps:get(created_at, Opts, Original);
		    false -> 
			erlang:system_time()
		end,
    Term = merl:term(CreatedAt),
    Result = merl:subst(Template, [{created_at, Term}]),
    merl_subst_updated_at(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_updated_at(Template, Opts) ->
    UpdatedAt = maps:get(updated_at, Opts, erlang:system_time()),
    Term = merl:term(UpdatedAt),
    Result = merl:subst(Template, [{updated_at, Term}]),
    merl_subst_format_opts(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_format_opts(Template, Opts) ->
    FormatOpts = maps:get(updated_at, Opts, []),
    Term = merl:term(FormatOpts),
    Result = merl:subst(Template, [{format_opts, Term}]),
    merl_subst_render_opts(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_render_opts(Template, Opts) ->
    RenderOpts = maps:get(render_opts, Opts, []),
    Term = merl:term(RenderOpts),
    Result = merl:subst(Template, [{render_opts, Term}]),
    merl_subst_callback(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_callback(Template, Opts) ->
    CallbackModule = maps:get(callback_module, Opts, bbmustache),
    CallbackFunction = maps:get(callback_function, Opts, render),
    Result = merl:subst(Template, [{callback_module, merl:term(CallbackModule)}
				  ,{callback_function, merl:term(CallbackFunction)}
				  ]),
    merl_subst_template(Result, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
merl_subst_template(Template, Opts) ->
    Content = maps:get(template, Opts, <<>>),
    Term = merl:term(Content),
    {ok, merl:subst(Template, [{template, Term}])}.
