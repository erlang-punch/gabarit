%%%-------------------------------------------------------------------
%%% @author Maartz <maartz@protonmail.com>
%%%
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
-export([start_link/0, start_link/1]).
-export([add_namespace/2, add_template/3, update_template/2, get_template/1,
         get_template_info/1, compile_template/2, get_template_version/2, list_namespaces/0, reset_namespace/2,
         list_templates/0, list_templates/1, list_versions/1, rollback/2]).
%% gen_statem callbacks
-export([init/1, callback_mode/0, unlocked/3, locked/3]).

%% Define ETS table names
-define(NAMESPACE_TABLE, gabarit_namespaces).
-define(TEMPLATE_TABLE, gabarit_templates).
-define(VERSION_TABLE, gabarit_versions).
%% Define default prefixes
-define(DEFAULT_PREFIX_FILE, "gabarit@").
-define(DEFAULT_PREFIX_STRING, "gabarit$").
-define(DEFAULT_NAMESPACE, gabarit).

%% Type definitions
-type namespace() :: binary() | string() | atom().
-type template_id() :: integer().
-type version() :: pos_integer().
-type timestamp() :: non_neg_integer().
-type content() :: binary().
-type compressed_content() :: binary().
-type template_path() :: binary() | string().
-type template_map() :: #{
    path => template_path(),
    content => content(),
    module_name => atom(),
    name => template_id(),
    filename => template_path(),
    template => content()
}.
-type limit() :: non_neg_integer().
-type error_reason() ::
    template_not_found |
    version_not_found |
    namespace_not_found |
    namespace_limit_reached |
    term().
-type options() :: [{atom(), term()}].

%% State record definition
-record(?MODULE,
        {persistence_module = none :: atom() | none,
         cleanup_interval = 3600000 :: non_neg_integer(),
         compress_threshold = 10240 :: non_neg_integer(),
         persistence_dir = "priv/gabarit_store" :: string()}).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

%% @doc Returns the callback mode for this gen_statem.
%% @end
-spec callback_mode() -> state_functions.
callback_mode() ->
  state_functions.

%% @doc Initializes the gen_statem.
%% Creates the ETS tables and sets up the initial state.
%% @end
-spec init(Options :: options()) -> {ok, unlocked, #gabarit_store{}}.
init(Args) ->
  ensure_ets_tables(),
  ensure_default_namespace(),

  State = process_options(Args),
  erlang:send_after(State#?MODULE.cleanup_interval, self(), cleanup),

  {ok, unlocked, State}.

%% @doc Ensures all required ETS tables exist.
%% @private
ensure_ets_tables() ->
  case ets:info(?NAMESPACE_TABLE) of
    undefined -> ets:new(?NAMESPACE_TABLE, [named_table, set, protected]);
    _ -> ok
  end,

  case ets:info(?TEMPLATE_TABLE) of
    undefined -> ets:new(?TEMPLATE_TABLE, [named_table, set, protected]);
    _ -> ok
  end,

  case ets:info(?VERSION_TABLE) of
    undefined -> ets:new(?VERSION_TABLE, [named_table, ordered_set, protected]);
    _ -> ok
  end.

%% @doc Ensures the default namespace exists.
%% @private
ensure_default_namespace() ->
    ets:insert(?NAMESPACE_TABLE, {?DEFAULT_NAMESPACE, infinity, 0}),
    ok.

%% @doc Process initialization options.
%% @private
process_options(Options) ->
  PersistenceModule = proplists:get_value(persistence_module, Options, none),
  CleanupInterval = proplists:get_value(cleanup_interval, Options, 3600000),
  CompressThreshold = proplists:get_value(compress_threshold, Options, 10240),
  PersistenceDir = proplists:get_value(persistence_dir, Options, "priv/gabarit_store"),

  #?MODULE{
    persistence_module = PersistenceModule,
    cleanup_interval = CleanupInterval,
    compress_threshold = CompressThreshold,
    persistence_dir = PersistenceDir
  }.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the template store with default options.
%% @returns {ok, Pid} | {error, Reason}
%% @end
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  start_link([]).

%% @doc Starts the template store as a linked process.
%% @param Opts Configuration options for the template store
%% @returns {ok, Pid} | {error, Reason}
%% @end
-spec start_link(Opts :: options()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Adds a new namespace with a template limit.
%% @param Namespace The namespace identifier
%% @param Limit Maximum number of templates allowed in the namespace
%% @returns ok | {error, Reason}
%% @end
-spec add_namespace(Namespace :: namespace(), Limit :: limit()) -> ok | {error, error_reason()}.
add_namespace(Namespace, Limit) ->
  gen_statem:call(?MODULE, {add_namespace, Namespace, Limit}).

%% @doc Adds a new template to a namespace.
%% @param Namespace The namespace to add the template to
%% @param Template The template path/identifier
%% @param Content The template content
%% @returns {ok, TemplateId} | {error, Reason}
%% @end
-spec add_template(Namespace :: namespace(), Template :: template_path(), Content :: content()) ->
    {ok, template_id()} | {error, error_reason()}.
add_template(Namespace, Template, Content) ->
  gen_statem:call(?MODULE,
                  {add_template, Namespace, #{path => Template, content => Content}}).

%% @doc Updates an existing template with new content.
%% Creates a new version of the template.
%% @param TemplateId The template identifier
%% @param Content The new template content
%% @returns {ok, NewVersion} | {error, Reason}
%% @end
-spec update_template(TemplateId :: template_id(), Content :: content()) ->
    {ok, version()} | {error, error_reason()}.
update_template(TemplateId, Content) ->
  gen_statem:call(?MODULE, {update_template, TemplateId, Content}).

%% @doc Retrieves the current version of a template.
%% @param TemplateId The template identifier
%% @returns {ok, Content} | {error, Reason}
%% @end
-spec get_template(TemplateId :: template_id()) -> {ok, content()} | {error, error_reason()}.
get_template(TemplateId) ->
  gen_statem:call(?MODULE, {get_template, TemplateId}).

%% @doc Retrieves a specific version of a template.
%% @param TemplateId The template identifier
%% @param Version The version number to retrieve
%% @returns {ok, Content} | {error, Reason}
%% @end
-spec get_template_version(TemplateId :: template_id(), Version :: version()) ->
    {ok, content()} | {error, error_reason()}.
get_template_version(TemplateId, Version) ->
  gen_statem:call(?MODULE, {get_template_version, TemplateId, Version}).

%% @doc Lists all defined namespaces.
%% @returns {ok, [{Namespace, Limit}]} | {error, Reason}
%% @end
-spec list_namespaces() -> {ok, [{namespace(), limit()}]} | {error, error_reason()}.
list_namespaces() ->
  gen_statem:call(?MODULE, list_namespaces, 5000).

%% @doc Lists all templates across all namespaces.
%% @returns {ok, [{TemplateId, Path}]} | {error, Reason}
%% @end
-spec list_templates() -> {ok, [{template_id(), template_path()}]} | {error, error_reason()}.
list_templates() ->
  gen_statem:call(?MODULE, list_all_templates, 5000).

%% @doc Lists all templates in a namespace.
%% @param Namespace The namespace to list templates from
%% @returns {ok, [{TemplateId, Path}]} | {error, Reason}
%% @end
-spec list_templates(Namespace :: namespace()) ->
    {ok, [{template_id(), template_path()}]} | {error, error_reason()}.
list_templates(Namespace) ->
  gen_statem:call(?MODULE, {list_templates, Namespace}).

%% @doc Lists all versions of a template.
%% @param TemplateId The template identifier
%% @returns {ok, [{Version, Timestamp}]} | {error, Reason}
%% @end
-spec list_versions(TemplateId :: template_id()) ->
    {ok, [{version(), timestamp()}]} | {error, error_reason()}.
list_versions(TemplateId) ->
  gen_statem:call(?MODULE, {list_versions, TemplateId}).

%% @doc Rolls back a template to a previous version.
%% Creates a new version with the content from the specified version.
%% @param TemplateId The template identifier
%% @param Version The version to roll back to
%% @returns {ok, Result} | {error, Reason}
%% @end
-spec rollback(TemplateId :: template_id(), Version :: version()) ->
    {ok, term()} | {error, error_reason()}.
rollback(TemplateId, Version) ->
  case get_template_version(TemplateId, Version) of
    {ok, Content} ->
      {ok, _NewVersion} = update_template(TemplateId, Content),

      case get_template_info(TemplateId) of
        {ok, TemplateInfo} ->
          TemplateStruct =
            #{name => TemplateId,
              filename => maps:get(path, TemplateInfo),
              template => Content},
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
-spec get_template_info(TemplateId :: template_id()) ->
    {ok, #{namespace := namespace(), path := template_path()}} | {error, error_reason()}.
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
-spec compile_template(TemplateId :: template_id(), Template :: template_map()) ->
    {ok, term()} | {error, error_reason()}.
compile_template(TemplateId, Template) ->
  try
    ModuleName = gabarit_module:name(?DEFAULT_PREFIX_FILE, TemplateId),
    gabarit_compiler:compile_file(Template#{module_name => ModuleName})
  catch
    _:_ -> {ok, TemplateId}
  end.

%% @doc Resets a namespace to have 0 templates and optionally updates its limit.
%% This is especially useful for testing.
%% @param Namespace The namespace to reset
%% @param NewLimit Optional new limit for the namespace (use the value 'keep' to keep the current limit)
%% @returns ok | {error, Reason}
%% @end
reset_namespace(Namespace, NewLimit) ->
    gen_statem:call(?MODULE, {reset_namespace, Namespace, NewLimit}, 5000).

%%====================================================================
%% State Functions
%%====================================================================

%% @doc Handles events in the unlocked state.
%% In this state, all operations are permitted.
%% @end
-spec unlocked(EventType :: gen_statem:event_type(),
               EventContent :: term(),
               State :: #gabarit_store{}) ->
    gen_statem:event_handler_result(#gabarit_store{}).
unlocked(internal, cleanup, State) ->
  perform_cleanup(),
  erlang:send_after(State#?MODULE.cleanup_interval, self(), cleanup),
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
      {CompressedContent, IsCompressed} = compress_if_needed(Content, State#?MODULE.compress_threshold),

      ets:insert(?TEMPLATE_TABLE, {TemplateId, Namespace, Path, 1}),
      ets:insert(?VERSION_TABLE, {{TemplateId, 1}, CompressedContent, IsCompressed, erlang:system_time(millisecond)}),

      ets:update_counter(?NAMESPACE_TABLE, Namespace, {3, 1}),

      case ets:lookup(?NAMESPACE_TABLE, Namespace) of
        [{_, infinity, _}] ->
          gen_statem:reply(From, {ok, TemplateId}),
          {keep_state, State};
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
unlocked({call, From}, list_all_templates, State) ->
  Templates = try
    ets:tab2list(?TEMPLATE_TABLE)
  catch
    _:_ -> []
  end,
  gen_statem:reply(From, {ok, [{Id, Path} || {Id, _, Path, _} <- Templates]}),
  {keep_state, State};
unlocked({call, From}, {list_templates, Namespace}, State) ->
  case ets:lookup(?NAMESPACE_TABLE, Namespace) of
    [] ->
      gen_statem:reply(From, {error, namespace_not_found}),
      {keep_state, State};
    _ ->
      Templates = ets:match_object(?TEMPLATE_TABLE, {'_', Namespace, '_', '_'}),
      gen_statem:reply(From, {ok, [{Id, Path} || {Id, _, Path, _} <- Templates]}),
      {keep_state, State}
  end;
unlocked({call, From}, {list_versions, TemplateId}, State) ->
  Versions = ets:match_object(?VERSION_TABLE, {{TemplateId, '_'}, '_', '_', '_'}),
  gen_statem:reply(From, {ok, [{V, T} || {{_, V}, _, _, T} <- Versions]}),
  {keep_state, State};
unlocked({call, From}, {reset_namespace, Namespace, NewLimit}, State) ->
    try
        case ets:lookup(?NAMESPACE_TABLE, Namespace) of
            [{Namespace, OldLimit, _}] ->
                Limit = case NewLimit of
                    keep -> OldLimit;
                    _ -> NewLimit
                end,
                ets:insert(?NAMESPACE_TABLE, {Namespace, Limit, 0}),
                gen_statem:reply(From, ok);
            [] ->
                gen_statem:reply(From, {error, namespace_not_found})
        end
    catch
        Error:Reason ->
            gen_statem:reply(From, {error, {Error, Reason}}),
            error_logger:error_msg("Error in reset_namespace: ~p:~p~n", [Error, Reason])
    end,
    {keep_state, State};
unlocked(_EventType, _EventContent, State) ->
  {keep_state, State}.

%% @doc Handles events in the locked state.
%% In this state, adding new templates is restricted.
%% @end
-spec locked(EventType :: gen_statem:event_type(),
             EventContent :: term(),
             State :: #gabarit_store{}) ->
    gen_statem:event_handler_result(#gabarit_store{}).
locked(internal, cleanup, State) ->
  perform_cleanup(),
  erlang:send_after(State#?MODULE.cleanup_interval, self(), cleanup),
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
locked({call, From}, list_all_templates, State) ->
  Templates = ets:tab2list(?TEMPLATE_TABLE),
  gen_statem:reply(From, {ok, [{Id, Path} || {Id, _, Path, _} <- Templates]}),
  {keep_state, State};
locked({call, From}, {list_templates, Namespace}, State) ->
  case ets:lookup(?NAMESPACE_TABLE, Namespace) of
    [] ->
      gen_statem:reply(From, {error, namespace_not_found}),
      {keep_state, State};
    _ ->
      Templates = ets:match_object(?TEMPLATE_TABLE, {'_', Namespace, '_', '_'}),
      gen_statem:reply(From, {ok, [{Id, Path} || {Id, _, Path, _} <- Templates]}),
      {keep_state, State}
  end;
locked({call, From}, {list_versions, TemplateId}, State) ->
  Versions = ets:match_object(?VERSION_TABLE, {{TemplateId, '_'}, '_', '_', '_'}),
  gen_statem:reply(From, {ok, [{V, T} || {{_, V}, _, _, T} <- Versions]}),
  {keep_state, State};
locked({call, From}, {reset_namespace, Namespace, NewLimit}, State) ->
    try
        case ets:lookup(?NAMESPACE_TABLE, Namespace) of
            [{Namespace, OldLimit, _}] ->
                Limit = case NewLimit of
                    keep -> OldLimit;
                    _ -> NewLimit
                end,
                ets:insert(?NAMESPACE_TABLE, {Namespace, Limit, 0}),
                gen_statem:reply(From, ok),
                {next_state, unlocked, State};
            [] ->
                gen_statem:reply(From, {error, namespace_not_found}),
                {keep_state, State}
        end
    catch
        Error:Reason ->
            gen_statem:reply(From, {error, {Error, Reason}}),
            error_logger:error_msg("Error in reset_namespace: ~p:~p~n", [Error, Reason]),
            {keep_state, State}
    end;
locked(_EventType, _EventContent, State) ->
  {keep_state, State}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Checks if a namespace has reached its template limit.
%% @param Namespace The namespace to check
%% @returns {ok, CurrentCount} | {error, Reason}
%% @private
-spec check_namespace_limit(Namespace :: namespace()) ->
    {ok, non_neg_integer()} | {error, error_reason()}.
check_namespace_limit(Namespace) ->
  case ets:lookup(?NAMESPACE_TABLE, Namespace) of
    [{Namespace, infinity, Count}] ->
      {ok, Count};
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
-spec generate_template_id() -> template_id().
generate_template_id() ->
  erlang:phash2({node(), self(), erlang:system_time(), make_ref()}).

%% @doc Compresses content if it exceeds the specified threshold.
%% @param Content The content to potentially compress
%% @param Threshold Size threshold for compression in bytes
%% @returns {CompressedContent, IsCompressed}
%% @private
-spec compress_if_needed(Content :: content(), Threshold :: non_neg_integer()) ->
    {compressed_content(), boolean()}.
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
-spec decompress_if_needed(CompressedContent :: compressed_content(), IsCompressed :: boolean()) -> content().
decompress_if_needed(Content, true) ->
  zlib:uncompress(Content);
decompress_if_needed(Content, false) ->
  Content.

%% @doc Performs cleanup of old template versions.
%% Keeps a configurable number of the most recent versions.
%% @returns ok
%% @private
-spec perform_cleanup() -> ok.
perform_cleanup() ->
  catch(do_perform_cleanup()),
  ok.

%% @doc Actual implementation of cleanup, in a try/catch wrapper.
%% @private
do_perform_cleanup() ->
  Templates = ets:tab2list(?TEMPLATE_TABLE),
  lists:foreach(fun perform_cleanup_foreach/1, Templates),
  ok.

%% @doc Cleanup function for a single template.
%% Keeps the current version and the most recent versions up to MaxVersionsToKeep.
%% @param {TemplateId, Namespace, Path, CurrentVersion} Template information
%% @returns ok
%% @private
-spec perform_cleanup_foreach(
    {TemplateId :: template_id(),
     _Namespace :: namespace(),
     _Path :: template_path(),
     CurrentVersion :: version()}) -> ok.
perform_cleanup_foreach({TemplateId, _Namespace, _Path, CurrentVersion}) ->
  AllVersions = get_all_template_versions(TemplateId),
  VersionsToKeep = select_versions_to_keep(AllVersions, CurrentVersion),
  delete_old_versions(TemplateId, AllVersions, VersionsToKeep),
  ok.

%% @doc Retrieves all versions of a specific template from the version table.
%% @param TemplateId The ID of the template
%% @returns [{Version, Timestamp}] List of versions with their timestamps
%% @private
-spec get_all_template_versions(TemplateId :: template_id()) ->
    [{Version :: version(), Timestamp :: timestamp()}].
get_all_template_versions(TemplateId) ->
  MatchedObjects = ets:match_object(?VERSION_TABLE, {{TemplateId, '_'}, '_', '_', '_'}),
  [{Version, Timestamp} || {{_, Version}, _, _, Timestamp} <- MatchedObjects].

%% @doc Selects which versions to keep based on recency and current version.
%% @param Versions List of {Version, Timestamp} tuples
%% @param CurrentVersion The currently active version that must be kept
%% @returns [Version] List of versions to keep
%% @private
-spec select_versions_to_keep(
    Versions :: [{Version :: version(), Timestamp :: timestamp()}],
    CurrentVersion :: version()) -> [version()].
select_versions_to_keep(Versions, CurrentVersion) ->
  MaxVersionsToKeep = application:get_env(gabarit, max_versions_to_keep, 5),
  SortedVersions = lists:sort(fun({_V1, T1}, {_V2, T2}) -> T1 > T2 end, Versions),
  [CurrentVersion |
    [V || {V, _} <- lists:sublist(SortedVersions, MaxVersionsToKeep),
          V =/= CurrentVersion]].

%% @doc Deletes versions that are not in the keep list.
%% @param TemplateId The ID of the template
%% @param AllVersions List of all {Version, _Timestamp} tuples
%% @param VersionsToKeep List of versions to keep
%% @returns ok
%% @private
-spec delete_old_versions(
    TemplateId :: template_id(),
    AllVersions :: [{Version :: version(), Timestamp :: timestamp()}],
    VersionsToKeep :: [version()]) -> ok.
delete_old_versions(TemplateId, AllVersions, VersionsToKeep) ->
  VersionsToDelete = [V || {V, _} <- AllVersions, not lists:member(V, VersionsToKeep)],
    lists:foreach(fun(Version) ->
      ets:delete(?VERSION_TABLE, {TemplateId, Version})
    end, VersionsToDelete),
  ok.

%% @doc Implementation of template update.
%% @param TemplateId The template identifier
%% @param Content The new content
%% @param State The current state
%% @returns {ok, NewVersion} | {error, Reason}
%% @private
-spec update_template_impl(
    TemplateId :: template_id(),
    Content :: content(),
    State :: #gabarit_store{}) -> {ok, version()} | {error, error_reason()}.
update_template_impl(TemplateId, Content, State) ->
  case ets:lookup(?TEMPLATE_TABLE, TemplateId) of
    [{TemplateId, Namespace, Path, CurrentVersion}] ->
      {CompressedContent, IsCompressed} =
        compress_if_needed(Content, State#?MODULE.compress_threshold),

      NewVersion = CurrentVersion + 1,

      ets:insert(?VERSION_TABLE,
                 {{TemplateId, NewVersion},
                  CompressedContent,
                  IsCompressed,
                  erlang:system_time(millisecond)}),

      ets:insert(?TEMPLATE_TABLE, {TemplateId, Namespace, Path, NewVersion}),

      {ok, NewVersion};
    [] ->
      {error, template_not_found}
  end.

%% @doc Implementation of template retrieval.
%% @param TemplateId The template identifier
%% @returns {ok, Content} | {error, Reason}
%% @private
-spec get_template_impl(TemplateId :: template_id()) ->
    {ok, content()} | {error, error_reason()}.
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
-spec get_template_version_impl(TemplateId :: template_id(), Version :: version()) ->
    {ok, content()} | {error, error_reason()}.
get_template_version_impl(TemplateId, Version) ->
  case ets:lookup(?VERSION_TABLE, {TemplateId, Version}) of
    [{{_, _}, Content, IsCompressed, _}] ->
      DecompressedContent = decompress_if_needed(Content, IsCompressed),
      {ok, DecompressedContent};
    [] ->
      {error, version_not_found}
  end.
