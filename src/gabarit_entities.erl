%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @doc 
%%%
%%% This module extract all entities from unicode.xml file and convert
%%% them into indexed maps. The first one generated creates an index
%%% of ascii/utf8 chars, the second one is doing the opposite, by
%%% indexing directly the entity. That's quite dirty, but it works for
%%% now.
%%%
%%% The next step is to directly create a module template using
%%% `merl', where all entities/chars are being stored instead of
%%% starting a process.
%%%
%%% @end
%%%===================================================================
-module(gabarit_entities).
-compile(export_all).
-behavior(gen_server).
-export([start/0, start_link/0]).
-export([entity_to_char/1, char_to_entity/1]).
-export([get_entities/0, get_chars/0]).
-export([init/1]).
-export([handle_info/2, handle_cast/2, handle_call/3]).
-include_lib("xmerl/include/xmerl.hrl").
-record(?MODULE, { chars = #{} :: map()
                 , entities = #{} :: map()
                 , options = #{} :: map()
                 }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()}.

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec entity_to_char(Entity) -> Return when
      Entity :: binary(),
      Return :: {ok, binary()}
              | {error, {binary(), not_found}}
              | timeout.

entity_to_char(<<Entity/binary>>) ->
    gen_server:call(?MODULE, {entity_to_char, Entity}, 1000).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec char_to_entity(Entity) -> Return when
      Entity :: binary(),
      Return :: {ok, binary()}
              | {error, {binary(), not_found}}
              | timeout.

char_to_entity(<<Char/binary>>) ->
    gen_server:call(?MODULE, {char_to_entity, Char}, 1000).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec get_entities() -> map().

get_entities() ->
    gen_server:call(?MODULE, entities, 1000).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec get_chars() -> map().

get_chars() ->
    gen_server:call(?MODULE, chars, 1000).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Args) ->
    {ok, {Chars, Entities}} = parse(),
    State = #?MODULE{ chars = Chars
                    , entities = Entities
                    , options = Args
                    },
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call({char_to_entity, Char}, _, #?MODULE{ chars = Chars, options = #{ default := Default }} = State) ->
    case maps:get(Char, Chars, undefined) of
        undefined -> 
            {reply, {error, {Char, not_found}}, State};
        Entity -> 
            case Entity of
                #{ Default := Result } ->
                    {reply, {ok, Result}, State};
                _ ->
                    {reply, {error, {Char, not_found, Default}}, State}
            end
    end;
handle_call({char_to_entity, Char}, _, #?MODULE{ chars = Chars } = State) ->
    case maps:get(Char, Chars, undefined) of
        undefined -> 
            {reply, {error, {Char, not_found}}, State};
        Entity -> 
            {reply, {ok, Entity}, State}
    end;
handle_call({entity_to_char, Entity}, _, #?MODULE{ entities = Entities} = State) ->
    case maps:get(Entity, Entities, undefined) of
        undefined -> 
            {reply, {error, {Entity, not_found}}, State};
        Char -> 
            {reply, {ok, Char}, State}
    end;
handle_call(entities, _, #?MODULE{ entities = Entities } = State) ->
    {reply, Entities, State};
handle_call(chars, _, #?MODULE{ chars = Chars } = State) ->
    {reply, Chars, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(_,State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(_,State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
options() ->
    [{event_fun, fun event/3}].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
parse() ->
    {ok, Cwd} = file:get_cwd(),
    Priv = code:priv_dir(gabarit),
    Filename = filename:join(Priv, "unicode.xml"),
    {ok, Content} = file:read_file(Filename),
    stream(binary_to_list(Content)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec stream(XML) -> Return when
      XML       :: binary() | string(),
      Return    :: {ok, {CharMap, EntityMap}},
      CharMap   :: map(),
      EntityMap :: map().

stream(Content) ->
    {ok, {charlist, Buffer}, _} = xmerl_sax_parser:stream(Content, options()),
    list_to_map(Buffer, #{}, #{}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
list_to_map(#{}, ByChars, ByEntities) -> {ok, {ByChars, ByEntities}};
list_to_map([], ByChars, ByEntities) -> {ok, {ByChars, ByEntities}};
list_to_map([#{ binary := Binary, entity := Entities } = Char|Rest], ByChars, ByEntities) ->
    EntitiesMap = entities_to_map(Entities, #{}),
    EntityChar = entity_to_char(Binary, Entities, #{}),
    list_to_map(Rest, ByChars#{ Binary => EntitiesMap }, maps:merge(ByEntities, EntityChar));
list_to_map([_|Rest], ByChars, ByEntities) ->
    list_to_map(Rest, ByChars, ByEntities).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
entities_to_map([], Buffer) -> Buffer;
entities_to_map([#{ id := Id, set := Set }|Rest], Buffer) ->
    entities_to_map(Rest, Buffer#{ Set => Id }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
entity_to_char(Binary, [], Buffer) -> Buffer;
entity_to_char(Binary, [#{ id := Id }|Rest], Buffer) ->
    entity_to_char(Binary, Rest, Buffer#{ Id => Binary }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
event(startDocument, Location, State) -> State;
event({startElement, [], "unicode", _, Attributes}, _Location, State) ->
    case get_attribute(Attributes, "unicode") of
        "15.1" -> State;
        Version -> throw({error, Version})
    end;
event({startElement, [], "entitygroups", _, _Attributes}, _Location, State) ->
    State;
event({startElement, [], "charlist", _, _Attributes}, _Location, State) ->
    {charlist, #{}};
event({startElement, [], "character",_,Attributes}, Location,  {charlist, Buffer} = State) ->
    Id = get_attribute(Attributes, "id"),
    Dec = get_attribute(Attributes, "dec"),
    Mode = get_attribute(Attributes, "mode"),
    Type = get_attribute(Attributes, "type"),
    case decimal_to_utf8(Dec) of
        {ok, FromDec} ->
            Char = #{ id => Id
                    , dec => Dec
                    , mode => Mode
                    , type => Type
                    , binary => FromDec
                    },
            {charlist, [Char|Buffer]};
        {error, _} ->
            {charlist, Buffer}
    end;
event({startElement, [], "entity", _, Attributes}, Location, {charlist, [Char|Buffer]}) ->
    EntityId = get_attribute(Attributes, "id"),
    Set = get_attribute(Attributes, "set"),
    BinaryEntity = list_to_binary(EntityId),
    Entity = #{ id => <<"&", BinaryEntity/binary, ";">>, set => Set },
    case Char of
        #{ entity := List } ->
            {charlist, [Char#{ entity => [Entity|List]}|Buffer]};
        _ ->
            {charlist, [Char#{ entity => [Entity]}|Buffer]}
    end;
event(Event, Location, State) ->
    State.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
hexadecimal_to_utf8([$U|Hex]) ->
    list_to_integer(Hex, 16).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decimal_to_utf8(Int) ->
    try 
        Integer = list_to_integer(Int),
        {ok, <<Integer/utf8>>}
    catch
        _:_ -> {error, Int}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
get_attribute(List, Key) -> get_attribute(List, Key, undefined).
get_attribute([], _, Default) -> Default;
get_attribute([{_,_,Key,Value}|_], Key, Default) -> Value;
get_attribute([Attribute|Rest], Key, Default) -> get_attribute(Rest, Key, Default).
