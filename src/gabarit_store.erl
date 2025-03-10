%%%-------------------------------------------------------------------
%%% @doc Template storage and management module.
%%%
%%% This module implements a state machine for managing templates within namespaces.
%%% It provides functionality for:
%%%  - Managing namespaces with template limits
%%%  - Adding, updating, and retrieving templates
%%%  - Version control with rollback capability
%%%  - Content compression for efficient storage
%%%
%%% The module uses three ETS tables:
%%%  - gabarit_namespaces: Stores namespace definitions and limits
%%%  - gabarit_templates: Stores template metadata
%%%  - gabarit_versions: Stores versioned template content
%%%
%%% The state machine has two states:
%%%  - unlocked: Normal operation, all functions available
%%%  - locked: Reached namespace limits, template addition restricted
%%% @end
%%%-------------------------------------------------------------------
-module(gabarit_store).
-behavior(gen_statem).

%% API exports
-export([start_link/1,
         add_namespace/2,
         add_template/3,
         update_template/2,
         get_template/1,
         get_template_info/1,
         compile_template/2,
         get_template_version/2,
         list_namespaces/0,
         list_templates/1,
         list_versions/1,
         rollback/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, unlocked/3, locked/3]).

%% Define ETS table names
-define(NAMESPACE_TABLE, gabarit_namespaces).
-define(TEMPLATE_TABLE, gabarit_templates).
-define(VERSION_TABLE, gabarit_versions).

%% Define default prefixes
-define(DEFAULT_PREFIX_FILE, "gabarit@").
-define(DEFAULT_PREFIX_STRING, "gabarit$").

%% State record definition
-record(state,
        {persistence_module = none :: atom() | none,
         cleanup_interval = 3600000 :: non_neg_integer(),
         compress_threshold = 10240 :: non_neg_integer()}).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

%% @doc Returns the callback mode for this gen_statem.
%% @end
callback_mode() ->
  state_functions.

%% @doc Initializes the gen_statem.
%% Creates the ETS tables and sets up the initial state.
%% @end
init(_Args) ->
  ets:new(?NAMESPACE_TABLE, [named_table, set, protected]),
  ets:new(?TEMPLATE_TABLE, [named_table, set, protected]),
  ets:new(?VERSION_TABLE, [named_table, ordered_set, protected]),

  State = #state{},

  erlang:send_after(State#state.cleanup_interval, self(), cleanup),

  {ok, unlocked, State}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the template store as a linked process.
%% @param Opts Configuration options for the template store
%% @returns {ok, Pid} | {error, Reason}
%% @end
start_link(Opts) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Adds a new namespace with a template limit.
%% @param Namespace The namespace identifier
%% @param Limit Maximum number of templates allowed in the namespace
%% @returns ok | {error, Reason}
%% @end
add_namespace(Namespace, Limit) ->
  gen_statem:call(?MODULE, {add_namespace, Namespace, Limit}).

%% @doc Adds a new template to a namespace.
%% @param Namespace The namespace to add the template to
%% @param Template The template path/identifier
%% @param Content The template content
%% @returns {ok, TemplateId} | {error, Reason}
%% @end
add_template(Namespace, Template, Content) ->
  gen_statem:call(?MODULE,
                  {add_template, Namespace, #{path => Template, content => Content}}).

%% @doc Updates an existing template with new content.
%% Creates a new version of the template.
%% @param TemplateId The template identifier
%% @param Content The new template content
%% @returns {ok, NewVersion} | {error, Reason}
%% @end
update_template(TemplateId, Content) ->
  gen_statem:call(?MODULE, {update_template, TemplateId, Content}).

%% @doc Retrieves the current version of a template.
%% @param TemplateId The template identifier
%% @returns {ok, Content} | {error, Reason}
%% @end
get_template(TemplateId) ->
  gen_statem:call(?MODULE, {get_template, TemplateId}).

%% @doc Retrieves a specific version of a template.
%% @param TemplateId The template identifier
%% @param Version The version number to retrieve
%% @returns {ok, Content} | {error, Reason}
%% @end
get_template_version(TemplateId, Version) ->
  gen_statem:call(?MODULE, {get_template_version, TemplateId, Version}).

%% @doc Lists all defined namespaces.
%% @returns {ok, [{Namespace, Limit}]} | {error, Reason}
%% @end
list_namespaces() ->
  gen_statem:call(?MODULE, list_namespaces).

%% @doc Lists all templates in a namespace.
%% @param Namespace The namespace to list templates from
%% @returns {ok, [{TemplateId, Path}]} | {error, Reason}
%% @end
list_templates(Namespace) ->
  gen_statem:call(?MODULE, {list_templates, Namespace}).

%% @doc Lists all versions of a template.
%% @param TemplateId The template identifier
%% @returns {ok, [{Version, Timestamp}]} | {error, Reason}
%% @end
list_versions(TemplateId) ->
  gen_statem:call(?MODULE, {list_versions, TemplateId}).

%% @doc Rolls back a template to a previous version.
%% Creates a new version with the content from the specified version.
%% @param TemplateId The template identifier
%% @param Version The version to roll back to
%% @returns {ok, Result} | {error, Reason}
%% @end
rollback(TemplateId, Version) ->
  case get_template_version(TemplateId, Version) of
    {ok, Content} ->
      {ok, _NewVersion} = update_template(TemplateId, Content),

      case get_template_info(TemplateId) of
        {ok, TemplateInfo} ->
          TemplateStruct = #{
            name => TemplateId,
            filename => maps:get(path, TemplateInfo),
            template => Content
          },
          % Compile the template
          compile_template(TemplateId, TemplateStruct);
        Error ->
          Error
      end;
    Error ->
      Error
  end.

%% @doc Retrieves metadata about a template.
%% @param TemplateId The template identifier
%% @returns {ok, #{namespace => Namespace, path => Path}} | {error, Reason}
%% @end
get_template_info(TemplateId) ->
  case ets:lookup(?TEMPLATE_TABLE, TemplateId) of
    [{TemplateId, Namespace, Path, _}] ->
      {ok, #{namespace => Namespace, path => Path}};
    [] ->
      {error, template_not_found}
  end.

%% @doc Compiles a template.
%% @param TemplateId The template identifier
%% @param Template The template structure
%% @returns {ok, Result} | {error, Reason}
%% @end
compile_template(TemplateId, Template) ->
  ModuleName = gabarit_module:name(?DEFAULT_PREFIX_FILE, TemplateId),
  gabarit_compiler:compile_file(Template#{module_name => ModuleName}).

%%====================================================================
%% State Functions
%%====================================================================

%% @doc Handles events in the unlocked state.
%% In this state, all operations are permitted.
%% @end
unlocked(internal, cleanup, State) ->
  perform_cleanup(),
  erlang:send_after(State#state.cleanup_interval, self(), cleanup),
  {keep_state, State};

unlocked({call, From}, {add_namespace, Namespace, Limit}, State) ->
  ets:insert(?NAMESPACE_TABLE, {Namespace, Limit, 0}),
  gen_statem:reply(From, ok),
  {keep_state, State};

unlocked({call, From}, {add_template, Namespace, Template}, State) ->
  case check_namespace_limit(Namespace) of
    {ok, _} ->
      TemplateId = generate_template_id(),
      Path = maps:get(path, Template, ""),
      Content = maps:get(content, Template, <<>>),

      {CompressedContent, IsCompressed} =
        compress_if_needed(Content, State#state.compress_threshold),

      ets:insert(?TEMPLATE_TABLE, {TemplateId, Namespace, Path, 1}),
      ets:insert(?VERSION_TABLE, {{TemplateId, 1}, CompressedContent, IsCompressed, erlang:system_time(millisecond)}),

      ets:update_counter(?NAMESPACE_TABLE, Namespace, {3, 1}),

      case ets:lookup(?NAMESPACE_TABLE, Namespace) of
        [{_, Limit, Count}] when Count >= Limit ->
          gen_statem:reply(From, {ok, TemplateId}),
          {next_state, locked, State};
        _ ->
          gen_statem:reply(From, {ok, TemplateId}),
          {keep_state, State}
      end;
    {error, Reason} ->
      gen_statem:reply(From, {error, Reason}),
      {keep_state, State}
  end;

unlocked({call, From}, {update_template, TemplateId, Content}, State) ->
  case update_template_impl(TemplateId, Content, State) of
    {ok, Version} ->
      gen_statem:reply(From, {ok, Version});
    {error, Reason} ->
      gen_statem:reply(From, {error, Reason})
  end,
  {keep_state, State};

unlocked({call, From}, {get_template, TemplateId}, State) ->
  case get_template_impl(TemplateId) of
    {ok, Content} ->
      gen_statem:reply(From, {ok, Content});
    {error, Reason} ->
      gen_statem:reply(From, {error, Reason})
  end,
  {keep_state, State};

unlocked({call, From}, {get_template_version, TemplateId, Version}, State) ->
  case get_template_version_impl(TemplateId, Version) of
    {ok, Content} ->
      gen_statem:reply(From, {ok, Content});
    {error, Reason} ->
      gen_statem:reply(From, {error, Reason})
  end,
  {keep_state, State};

unlocked({call, From}, list_namespaces, State) ->
  Namespaces = ets:tab2list(?NAMESPACE_TABLE),
  gen_statem:reply(From, {ok, [{N, L} || {N, L, _} <- Namespaces]}),
  {keep_state, State};

unlocked({call, From}, {list_templates, Namespace}, State) ->
  Templates = ets:match_object(?TEMPLATE_TABLE, {'_', Namespace, '_', '_'}),
  gen_statem:reply(From, {ok, [{Id, Path} || {Id, _, Path, _} <- Templates]}),
  {keep_state, State};

unlocked({call, From}, {list_versions, TemplateId}, State) ->
  Versions = ets:match_object(?VERSION_TABLE, {{TemplateId, '_'}, '_', '_', '_'}),
  gen_statem:reply(From, {ok, [{V, T} || {{_, V}, _, _, T} <- Versions]}),
  {keep_state, State};

unlocked(_EventType, _EventContent, State) ->
  {keep_state, State}.

%% @doc Handles events in the locked state.
%% In this state, adding new templates is restricted.
%% @end
locked(internal, cleanup, State) ->
  perform_cleanup(),
  erlang:send_after(State#state.cleanup_interval, self(), cleanup),
  {keep_state, State};

locked({call, From}, {add_template, _Namespace, _Template}, State) ->
  gen_statem:reply(From, {error, namespace_limit_reached}),
  {keep_state, State};

locked({call, From}, {update_template, TemplateId, Content}, State) ->
  case update_template_impl(TemplateId, Content, State) of
    {ok, Version} ->
      gen_statem:reply(From, {ok, Version});
    {error, Reason} ->
      gen_statem:reply(From, {error, Reason})
  end,
  {keep_state, State};

locked({call, From}, {get_template, TemplateId}, State) ->
  case get_template_impl(TemplateId) of
    {ok, Content} ->
      gen_statem:reply(From, {ok, Content});
    {error, Reason} ->
      gen_statem:reply(From, {error, Reason})
  end,
  {keep_state, State};

locked({call, From}, {get_template_version, TemplateId, Version}, State) ->
  case get_template_version_impl(TemplateId, Version) of
    {ok, Content} ->
      gen_statem:reply(From, {ok, Content});
    {error, Reason} ->
      gen_statem:reply(From, {error, Reason})
  end,
  {keep_state, State};

locked({call, From}, list_namespaces, State) ->
  Namespaces = ets:tab2list(?NAMESPACE_TABLE),
  gen_statem:reply(From, {ok, [{N, L} || {N, L, _} <- Namespaces]}),
  {keep_state, State};

locked({call, From}, {list_templates, Namespace}, State) ->
  Templates = ets:match_object(?TEMPLATE_TABLE, {'_', Namespace, '_', '_'}),
  gen_statem:reply(From, {ok, [{Id, Path} || {Id, _, Path, _} <- Templates]}),
  {keep_state, State};

locked({call, From}, {list_versions, TemplateId}, State) ->
  Versions = ets:match_object(?VERSION_TABLE, {{TemplateId, '_'}, '_', '_', '_'}),
  gen_statem:reply(From, {ok, [{V, T} || {{_, V}, _, _, T} <- Versions]}),
  {keep_state, State};

locked(_EventType, _EventContent, State) ->
  {keep_state, State}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Checks if a namespace has reached its template limit.
%% @param Namespace The namespace to check
%% @returns {ok, CurrentCount} | {error, Reason}
%% @private
check_namespace_limit(Namespace) ->
  case ets:lookup(?NAMESPACE_TABLE, Namespace) of
    [{Namespace, Limit, Count}] when Count < Limit ->
      {ok, Count};
    [{_, _, _}] ->
      {error, namespace_limit_reached};
    [] ->
      {error, namespace_not_found}
  end.

%% @doc Generates a unique template identifier.
%% @returns TemplateId :: integer()
%% @private
generate_template_id() ->
  erlang:phash2({node(), self(), erlang:system_time(), make_ref()}).

%% @doc Compresses content if it exceeds the specified threshold.
%% @param Content The content to potentially compress
%% @param Threshold Size threshold for compression in bytes
%% @returns {CompressedContent, IsCompressed}
%% @private
compress_if_needed(Content, Threshold) ->
  if byte_size(Content) > Threshold ->
       {zlib:compress(Content), true};
     true ->
       {Content, false}
  end.

%% @doc Decompresses content if it was previously compressed.
%% @param Content The potentially compressed content
%% @param IsCompressed Boolean indicating if content is compressed
%% @returns DecompressedContent
%% @private
decompress_if_needed(Content, true) ->
  zlib:uncompress(Content);
decompress_if_needed(Content, false) ->
  Content.

%% @doc Performs cleanup of old template versions.
%% Keeps a configurable number of the most recent versions.
%% @returns ok
%% @private
perform_cleanup() ->
  Templates = ets:tab2list(?TEMPLATE_TABLE),

  MaxVersionsToKeep = application:get_env(gabarit, max_versions_to_keep, 5),

  lists:foreach(fun({TemplateId, _Namespace, _Path, CurrentVersion}) ->
                   AllVersions = ets:match_object(?VERSION_TABLE, {{TemplateId, '_'}, '_', '_', '_'}),
                   Versions = [{V, T} || {{_, V}, _, _, T} <- AllVersions],

                   SortedVersions = lists:sort(fun({_V1, T1}, {_V2, T2}) -> T1 > T2 end, Versions),

                   case SortedVersions of
                     [] -> ok;
                     _ ->
                       VersionsToDelete =
                         lists:filter(fun({V, _}) ->
                                         V =/= CurrentVersion
                                         andalso length(lists:takewhile(fun({Vx, _}) -> Vx =/= V
                                                                        end,
                                                                        SortedVersions))
                                                 >= MaxVersionsToKeep
                                      end,
                                      SortedVersions),

                       lists:foreach(fun({V, _}) -> ets:delete(?VERSION_TABLE, {{TemplateId, V}}) end,
                                     VersionsToDelete)
                   end
                end,
                Templates),

  ok.

%% @doc Implementation of template update.
%% @param TemplateId The template identifier
%% @param Content The new content
%% @param State The current state
%% @returns {ok, NewVersion} | {error, Reason}
%% @private
update_template_impl(TemplateId, Content, State) ->
    case ets:lookup(?TEMPLATE_TABLE, TemplateId) of
        [{TemplateId, Namespace, Path, CurrentVersion}] ->
            {CompressedContent, IsCompressed} = compress_if_needed(Content, State#state.compress_threshold),

            NewVersion = CurrentVersion + 1,

            ets:insert(?VERSION_TABLE, {{TemplateId, NewVersion}, CompressedContent, IsCompressed, erlang:system_time(millisecond)}),

            ets:insert(?TEMPLATE_TABLE, {TemplateId, Namespace, Path, NewVersion}),

            {ok, NewVersion};
        [] ->
            {error, template_not_found}
    end.

%% @doc Implementation of template retrieval.
%% @param TemplateId The template identifier
%% @returns {ok, Content} | {error, Reason}
%% @private
get_template_impl(TemplateId) ->
  case ets:lookup(?TEMPLATE_TABLE, TemplateId) of
    [{TemplateId, _Namespace, _Path, CurrentVersion}] ->
      get_template_version_impl(TemplateId, CurrentVersion);
    [] ->
      {error, template_not_found}
  end.

%% @doc Implementation of template version retrieval.
%% @param TemplateId The template identifier
%% @param Version The version to retrieve
%% @returns {ok, Content} | {error, Reason}
%% @private
get_template_version_impl(TemplateId, Version) ->
    case ets:lookup(?VERSION_TABLE, {TemplateId, Version}) of
        [{{_, _}, Content, IsCompressed, _}] ->
            DecompressedContent = decompress_if_needed(Content, IsCompressed),
            {ok, DecompressedContent};
        [] ->
            {error, version_not_found}
    end.
