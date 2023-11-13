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

create(Element, Opts)  ->
    Full = tags(Element, Opts),
    doctype(Full, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tags([], _) -> <<>>;
tags(Elements, Opts) ->
    tags(Elements, <<>>, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tags([], Buffer, _Opts) -> Buffer;
tags(Element, Buffer, Opts)
  when is_tuple(Element) ->
    tags([Element], Buffer, Opts);
tags([Element|Elements], Buffer, Opts)
  when is_tuple(Element) ->
    Tag = tag(Element, Opts),
    tags(Elements, <<Buffer/binary, Tag/binary>>, Opts);
tags([Element|Elements], Buffer, Opts) ->
    Encoded = text(Element, Opts),
    tags(Elements, <<Buffer/binary, Encoded/binary>>, Opts);
tags(Element, Buffer, Opts) ->
    Encoded = text(Element, Opts),
    <<Buffer/binary, Encoded/binary>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
doctype(Buffer, #{ doctype := true }) ->
    <<"<!DOCTYPE html>", Buffer/binary>>;
doctype(Buffer, _Opts) ->
    Buffer.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
text(Content, Opts)
  when is_integer(Content) ->
    text(integer_to_binary(Content), Opts);
text(Content, Opts)
  when is_atom(Content) ->
    text(atom_to_binary(Content), Opts);
text(Content, #{ html_entities := false })
  when is_binary(Content) ->
    Content;
text(Content, _Opts)
  when is_binary(Content) ->
    gabarit_html_entities:encode(Content).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag({Element, Attributes}, Opts) ->
    tag({Element, Attributes, []}, Opts);

tag({Element, Attributes, Content}, Opts)
  when is_atom(Element) ->
    tag({atom_to_binary(Element), Attributes, Content}, Opts);
tag({Element, Attributes, _Content} = Tag, Opts)
  when is_binary(Element), is_map(Attributes) ->
    tag1(Tag, <<>>, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag1({Element, Attributes, Content}, Buffer, Opts) ->
    tag1({Element, Attributes, Content, #{}}, Buffer, Opts);

tag1({Element, _Attributes, _Content, _LocalOpts} = Item, Buffer, Opts) ->
    case Element of
        <<"base">> -> tag_without_content(Item, Buffer, Opts);
        <<"br">> -> tag_without_content(Item, Buffer, Opts);
        <<"img">> -> tag_without_content(Item, Buffer, Opts);
        <<"input">> -> tag_without_content(Item, Buffer, Opts);
        <<"link">> -> tag_without_content(Item, Buffer, Opts);
        <<"meta">> -> tag_without_content(Item, Buffer, Opts);
        <<"source">> -> tag_without_content(Item, Buffer, Opts);
        _ -> tag_with_content(Item, Buffer, Opts)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag_with_content({Element, Attributes, Content, _LocalOpts}, _Buffer, Opts) ->
    NewAttributes = attributes(Attributes),
    StartElement = bracket(Element, NewAttributes),
    NewContent = tags(Content, Opts),
    EndElement = bracket_end(Element),
    <<StartElement/binary, NewContent/binary, EndElement/binary>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag_without_content({Element, Attributes, _Content, _LocalOpts}, _Buffer, _Opts) ->
    NewAttributes = attributes(Attributes),
    Item = bracket(Element, NewAttributes),
    <<Item/binary>>.

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
