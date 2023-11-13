%%%===================================================================
%%% @doc Create html page using tuple, map and binaries.
%%% @end
%%%===================================================================
-module(gabarit_html).
-export([create/1, create/2]).
-export([join/1, join/2]).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-type tag() :: atom() | list() | binary().
-type attributes() :: #{}.
-type content() :: [].
-type options() :: #{}.
-type element() :: {tag(), attributes()}
                 | {tag(), attributes(), content()}
                 | {tag(), attributes(), content(), options()}.
-type elements() :: [element()].

%%--------------------------------------------------------------------
%% @doc create a new HTML page from elements.
%% @end
%%--------------------------------------------------------------------
-spec create(Element) -> Return when
      Element :: element() | elements(),
      Return  :: binary().

create(Element) ->
    create(Element, #{}).

%%--------------------------------------------------------------------
%% @doc create a new HTML page from elements.
%% @end
%%--------------------------------------------------------------------
-spec create(Element, Opts) -> Return when
      Element :: element() | elements(),
      Opts    :: options(),
      Return  :: binary().

create(Element, _Opts)
  when is_list(Element) ->
    tags(Element);
create(Element, _Opts) 
  when is_tuple(Element) ->
    tag(Element).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
tags([]) -> <<>>;
tags(Elements) ->
    tags(Elements, <<>>).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tags([], Buffer) -> Buffer;
tags([Element|Elements], Buffer) 
  when is_binary(Element) ->
    tags(Elements, <<Element/binary, Buffer/binary>>);
tags([Element|Elements], Buffer) ->
    Tag = tag(Element),
    tags(Elements, <<Tag/binary, Buffer/binary>>).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag({Element, Attributes}) ->
    tag({Element, Attributes, []});
tag({Element, Attributes, Content})
  when is_atom(Element) ->
    tag({atom_to_binary(Element), Attributes, Content});
tag({Element, Attributes, Content} = Tag) 
  when is_binary(Element), is_map(Attributes), is_list(Content) ->
    tag1(Tag, <<>>).

tag1({Element, Attributes, Content}, Buffer) ->
    tag1({Element, Attributes, Content, #{}}, Buffer);
tag1({Element, Attributes, Content, _Opts}, _Buffer) ->
    NewAttributes = attributes(Attributes),
    StartElement = bracket(Element, NewAttributes),
    NewContent = tags(Content),
    EndElement = bracket_end(Element),
    <<StartElement/binary, NewContent/binary, EndElement/binary>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
bracket(Element, <<>>) ->
    <<"<", Element/binary, ">">>;
bracket(Element, Attributes) ->
    <<"<", Element/binary, " ", Attributes/binary, ">">>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
bracket_end(Element) ->
    <<"</", Element/binary, ">">>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
attributes(Attributes) 
  when map_size(Attributes) =:= 0 ->
    <<>>;
attributes(Attributes) ->
    Keys = maps:keys(Attributes),
    attributes(Attributes, Keys, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
attributes(_Attributes, [], Buffer) ->
    join(lists:reverse(Buffer), <<" ">>);
attributes(Attributes, [Key|Keys], Buffer) ->
    Value = erlang:map_get(Key, Attributes),
    attributes(Attributes, Keys, [attribute(Key, Value)|Buffer]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
attribute(Key, Value) ->
    NewKey = key(Key),
    NewValue = quotes(value(Value)),
    <<NewKey/binary, "=", NewValue/binary>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
join(Binaries) -> join(Binaries, <<" ">>).

join(Binaries, Sep) -> join(Binaries, Sep, <<>>).
join([Binary], _, Buffer) ->
    <<Binary/binary, Buffer/binary>>;
join([Binary|Rest], Sep, Buffer) ->
    join(Rest, Sep, <<Sep/binary, Binary/binary, Buffer/binary>>).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
value(Value) when is_list(Value) ->
    list_to_binary(Value);
value(Value) when is_atom(Value) ->
    atom_to_binary(Value);
value(Value) when is_integer(Value) ->
    integer_to_binary(Value);
value(Value) when is_binary(Value) ->
    Value.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
key(Key) when is_list(Key) ->
    list_to_binary(Key);
key(Key) when is_atom(Key) ->
    atom_to_binary(Key);
key(Key) when is_integer(Key) ->
    integer_to_binary(Key);
key(Key) when is_binary(Key) ->
    Key.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
quotes(Value) ->
    <<$\", Value/binary, $\">>.

tag_test() ->
    ?assertEqual(<<"<html></html>">>, tag({html, #{}})).
