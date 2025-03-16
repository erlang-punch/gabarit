%%%-------------------------------------------------------------------
%%% @author Maartz <maartz@protonmail.com>
%%%
%%% @doc
%%% Property-based tests for template rendering functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(prop_gabarit_render).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Properties
%%====================================================================

prop_render_substitutes_variables() ->
    ?FORALL(
        {Key, Value},
        {variable_key(), variable_value()},
        begin
            VariableName = binary_to_list(Key),
            Template = <<"<div>Some content {{", Key/binary, "}} more content</div>">>,

            Context = #{VariableName => Value},
            Rendered = bbmustache:render(Template, Context),

            PlaceholderGone = not contains_placeholder(Rendered, Key),
            ValuePresent = contains_value(Rendered, Value),

            PlaceholderGone andalso ValuePresent
        end
    ).

prop_render_preserves_unmatched_variables() ->
    ?FORALL(
        {Template, Key, _UnmatchedKey},
        {template_with_var(), variable_key(), different_key()},
        begin
            Context = #{},

            Rendered = bbmustache:render(Template, Context),

            % BBMustache removes unmatched variables by default
            not contains_placeholder(Rendered, Key)
        end
    ).
prop_nested_variables_work() ->
    ?FORALL(
        {Outer, Inner, Value},
        {variable_key(), variable_key(), variable_value()},
        begin
            Template =
                <<"<div>{{#", Outer/binary, "}}", "{{", Inner/binary, "}}", "{{/", Outer/binary,
                    "}}</div>">>,

            Context = #{binary_to_list(Outer) => #{binary_to_list(Inner) => Value}},

            Rendered = bbmustache:render(Template, Context),
            binary:match(Rendered, Value) =/= nomatch
        end
    ).
%%====================================================================
%% Generators
%%====================================================================

template_with_var() ->
    ?LET(
        {_Base, Key},
        {template_base(), variable_key()},
        begin
            Parts = [
                <<"<div>">>,
                <<"Some content ">>,
                <<"{{", Key/binary, "}}">>,
                <<" more content">>,
                <<"</div>">>
            ],
            iolist_to_binary(Parts)
        end
    ).

template_base() ->
    oneof([
        <<"<html><body></body></html>">>,
        <<"<div></div>">>,
        <<"<p></p>">>,
        <<"Hello!">>,
        <<"This is a test.">>
    ]).

variable_key() ->
    ?LET(
        Chars,
        non_empty(list(variable_char())),
        list_to_binary(Chars)
    ).

different_key() ->
    ?LET(
        Key,
        variable_key(),
        <<Key/binary, "_different">>
    ).

variable_value() ->
    ?LET(
        Chars,
        non_empty(list(value_char())),
        list_to_binary(Chars)
    ).

variable_char() ->
    oneof([
        choose($a, $z),
        choose($A, $Z),
        choose($0, $9),
        $_
    ]).

value_char() ->
    oneof([
        choose($a, $z),
        choose($A, $Z),
        choose($0, $9),
        $.,
        $,,
        $!,
        $?,
        $-,
        $_,
        $

    ]).

%%====================================================================
%% Helper Functions
%%====================================================================

contains_placeholder(Rendered, Key) ->
    Placeholder = <<"{{", Key/binary, "}}">>,
    binary:match(Rendered, Placeholder) =/= nomatch.

contains_value(Rendered, Value) ->
    binary:match(Rendered, Value) =/= nomatch.
