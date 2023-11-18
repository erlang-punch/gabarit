%%%===================================================================
%%%
%%%===================================================================
-module(gabarit_entities).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
options() ->
    [ % {continuation_fun, fun ?MODULE:continuation/1}
      % , {continuation_state, []}
      {event_fun, fun event/3}
    ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
parse() ->
    {ok, Cwd} = file:get_cwd(),
    Filename = filename:join(Cwd, "priv/unicode.xml"),
    {ok, Content} = file:read_file(Filename),
    stream(binary_to_list(Content)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
stream(Content) ->
    xmerl_sax_parser:stream(Content, options()).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
event(startDocument, Location, State) ->
    State;
event({startElement,[],"unicode",_,_Attributes}, _Location, State) ->    
    State;
event({startElement, [], "charlist",_,_}, _Location, State) ->
    {[], []};
event({startElement, [], "character",_,Attributes}, Location,  {CharList, _} = State) ->
    Id = get_attribute(Attributes, "id"),
    Dec = get_attribute(Attributes, "dec"),
    Mode = get_attribute(Attributes, "mode"),
    Type = get_attribute(Attributes, "type"),
    Char = {character, Id, #{ dec => Dec, mode => Mode, type => Type }, []},
    {[Char|CharList], Char};
event({startElement, [], "entity", _, Attributes}, Location, {CharList, Char} = State) ->
    {character, Id, Opts, Entities} = Char,
    EntityId = get_attribute(Attributes, "id"),
    Set = get_attribute(Attributes, "set"),
    {CharList, {character, Id, Opts, [#{ id => EntityId, set => Set }|Entities]}};
event({endElement, [], "entity", _}, Location, State) ->
    State;
event({endElement, [], "character",_}, Location, State) ->
    State;
event({endElement, [], "charlist", _}, _Location, State) ->
    State;
event(endDocument, _Location, State) ->
    State;
event(Event, Location, State) ->
    State.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
get_attribute(List, Key) -> get_attribute(List, Key, undefined).
get_attribute([], _, Default) -> Default;
get_attribute([{_,_,Key,Value}|_], Key, Default) -> Value;
get_attribute([Attribute|Rest], Key, Default) -> get_attribute(Rest, Key, Default).
