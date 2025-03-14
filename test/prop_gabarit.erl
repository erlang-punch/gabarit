%%%-------------------------------------------------------------------
%%% @author Maartz <maartz@protonmail.com>
%%%
%%% @doc
%%% Property-based tests for the gabarit template engine.
%%% Tests verify template creation, content preservation, and variable replacement.
%%% @end
%%%-------------------------------------------------------------------
-module(prop_gabarit).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Type definitions for better code documentation
-type template() :: binary().
-type template_type() :: simple_div | paragraph | html_doc | list | plain_text.
-type template_variable() :: binary().
-type context() :: #{binary() => binary()}.
-type render_result() :: {ok, binary()} | binary() | term().

%%====================================================================
%% Properties
%%====================================================================

%% @doc Verifies that the template compiler successfully creates a module
%% for each template type. Fails if template compilation fails for any type.
prop_new_template_creates_valid_module() ->
    ?FORALL(
        TemplateType,
        template_type(),
        run_template_test(
            % Setup function
            fun() -> create_template_with_placeholder(TemplateType) end,
            % Execute function
            fun(Filename) -> gabarit:new(Filename) end,
            % Verify function
            fun is_ok_result/1
        )
    ).

%% @doc Verifies that templates with no variables render exactly as the original
%% template content. Fails if the rendered content differs from the original template.
prop_render_preserves_static_content() ->
    ?FORALL(
        {Template, Context},
        {valid_template_without_vars(), context_map()},
        run_template_test(
            % Setup function
            fun() -> Template end,
            % Execute function
            fun(Filename) ->
                {ok, Module} = gabarit:new(Filename),
                Module:render(Context)
            end,
            % Verify function
            fun(Result) ->
                Rendered = normalize_render_result(Result),
                Rendered =:= Template
            end
        )
    ).

%% @doc Verifies that all variables in a template are replaced with values
%% from the context. Fails if any variable remains unreplaced in the output.
prop_render_replaces_all_variables() ->
    ?FORALL(
        {TemplateBase, Variables, Context},
        {template_base(), non_empty(list(template_variable())), context_map_with_vars()},
        run_template_test(
            % Setup function
            fun() -> create_template_with_vars(TemplateBase, Variables) end,
            % Execute function
            fun(Filename) ->
                {ok, Module} = gabarit:new(Filename),
                FullContext = create_context_for_variables(Variables, Context),
                Module:render(FullContext)
            end,
            % Verify function
            fun(Result) ->
                Rendered = normalize_render_result(Result),
                not contains_any_variable(Rendered, Variables)
            end
        )
    ).

%% @doc Verifies the behavior of the template engine when variables are missing from the context.
%% Either the variables remain in the output or they're replaced with empty strings.
prop_render_handles_missing_variables() ->
    ?FORALL(
        {TemplateBase, Variables},
        {template_base(), non_empty(list(template_variable()))},
        run_template_test(
            fun() -> create_template_with_vars(TemplateBase, Variables) end,
            fun(Filename) ->
                {ok, Module} = gabarit:new(Filename),
                Module:render(#{})
            end,
            fun(Result) ->
                Rendered = normalize_render_result(Result),
                AllPresent = lists:all(
                    fun(Var) -> binary:match(Rendered, Var) =/= nomatch end,
                    Variables
                ),
                NonePresent = lists:all(
                    fun(Var) -> binary:match(Rendered, Var) =:= nomatch end,
                    Variables
                ),
                AllPresent orelse NonePresent
            end
        )
    ).

%%====================================================================
%% Generators
%%====================================================================

%% @doc Generator for template types
-spec template_type() -> proper_types:type().
template_type() ->
    oneof([simple_div, paragraph, html_doc, list, plain_text]).

%% @doc Generator for templates without variables
-spec valid_template_without_vars() -> proper_types:type().
valid_template_without_vars() ->
    template_base().

%% @doc Generator for base template content
-spec template_base() -> proper_types:type().
template_base() ->
    ?LET(
        Parts,
        non_empty(list(template_part())),
        list_to_binary(Parts)
    ).

%% @doc Generator for template parts
-spec template_part() -> proper_types:type().
template_part() ->
    oneof([
        "<!DOCTYPE html>",
        "<html>",
        "</html>",
        "<head>",
        "</head>",
        "<body>",
        "</body>",
        "<div>",
        "</div>",
        "<p>",
        "</p>",
        "Hello, world!",
        "This is a test.",
        "\n",
        " "
    ]).

%% @doc Generator for template variables ({{name}})
-spec template_variable() -> proper_types:type().
template_variable() ->
    ?LET(
        Name,
        non_empty(list(variable_char())),
        list_to_binary(["{{", Name, "}}"])
    ).

%% @doc Generator for characters valid in variable names
-spec variable_char() -> proper_types:type().
variable_char() ->
    oneof([
        choose($a, $z),
        choose($A, $Z),
        choose($0, $9),
        $_
    ]).

%% @doc Generator for context maps (for rendering)
-spec context_map() -> proper_types:type().
context_map() ->
    ?LET(
        KVs,
        list({template_key(), template_value()}),
        maps:from_list([{Key, Value} || {Key, Value} <- KVs])
    ).

%% @doc Generator for non-empty context maps
-spec context_map_with_vars() -> proper_types:type().
context_map_with_vars() ->
    ?LET(
        KVs,
        non_empty(list({template_key(), template_value()})),
        maps:from_list([{Key, Value} || {Key, Value} <- KVs])
    ).

%% @doc Generator for template keys
-spec template_key() -> proper_types:type().
template_key() ->
    ?LET(
        Chars,
        non_empty(list(variable_char())),
        list_to_binary(Chars)
    ).

%% @doc Generator for template values
-spec template_value() -> proper_types:type().
template_value() ->
    ?LET(
        Chars,
        list(choose($a, $z)),
        list_to_binary(Chars)
    ).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Runs a template test with setup, execution, cleanup, and verification steps
-spec run_template_test(
    fun(() -> template()),
    fun((string()) -> term()),
    fun((term()) -> boolean())
) -> boolean().
run_template_test(Setup, Execute, Verify) ->
    Template = Setup(),
    Filename = setup_template_file(Template),
    Result =
        try
            Execute(Filename)
        catch
            _:Error:Stacktrace ->
                % Check if the error occurred in the render function
                case Stacktrace of
                    [{_Module, render, _Args, _Location} | _] ->
                        {error, render_failed};
                    _ ->
                        {error, {Error, Stacktrace}}
                end
        end,
    cleanup_template_file(Filename),
    Verify(Result).

%% @doc Checks if a result is an {ok, _} tuple
-spec is_ok_result(term()) -> boolean().
is_ok_result(Result) ->
    is_tuple(Result) andalso tuple_size(Result) >= 1 andalso element(1, Result) =:= ok.

%% @doc Normalizes different possible render result formats to a binary
-spec normalize_render_result(render_result()) -> binary().
normalize_render_result({ok, Rendered}) -> Rendered;
normalize_render_result(Rendered) when is_binary(Rendered) -> Rendered;
normalize_render_result(_) -> <<"error">>.

%% @doc Creates a template with a placeholder variable based on template type
-spec create_template_with_placeholder(template_type()) -> template().
create_template_with_placeholder(simple_div) ->
    <<"<div>Hello, {{name}}!</div>">>;
create_template_with_placeholder(paragraph) ->
    <<"<p>This is a {{name}} paragraph.</p>">>;
create_template_with_placeholder(html_doc) ->
    <<"<html><body><div>Welcome, {{name}}!</div></body></html>">>;
create_template_with_placeholder(list) ->
    <<"<ul><li>{{name}}</li><li>Item 2</li></ul>">>;
create_template_with_placeholder(plain_text) ->
    <<"Plain text with {{name}} variable.">>.

%% @doc Creates a template with variables inserted at deterministic positions
-spec create_template_with_vars(template(), [template_variable()]) -> template().
create_template_with_vars(Base, []) ->
    Base;
create_template_with_vars(Base, Variables) ->
    insert_variables_deterministically(Base, Variables).

%% @doc Inserts variables at deterministic positions in the template
-spec insert_variables_deterministically(template(), [template_variable()]) -> template().
insert_variables_deterministically(Template, Variables) ->
    TemplateSize = byte_size(Template),
    % Calculate evenly spaced positions for variables
    insert_variables_deterministically(Template, Variables, TemplateSize, [], 1).

insert_variables_deterministically(Template, [], _, Positions, _) ->
    insert_at_positions(Template, Positions);
insert_variables_deterministically(Template, [Var | Rest], Size, Positions, Index) ->
    % Calculate position based on the number of variables and template size
    VarCount = length(Rest) + 1,
    Pos = min(Size, (Size * Index) div (VarCount + 1)),
    insert_variables_deterministically(Template, Rest, Size, [{Pos, Var} | Positions], Index + 1).

%% @doc Inserts variables at specified positions
-spec insert_at_positions(template(), [{non_neg_integer(), template_variable()}]) -> template().
insert_at_positions(Template, Positions) ->
    SortedPositions = lists:sort(fun({A, _}, {B, _}) -> A =< B end, Positions),
    do_insert_at_positions(Template, SortedPositions, 0, <<>>).

do_insert_at_positions(Template, [], _, Acc) ->
    <<Acc/binary, Template/binary>>;
do_insert_at_positions(Template, [{Pos, Var} | Rest], Offset, Acc) ->
    AdjustedPos = Pos - Offset,
    <<Before:AdjustedPos/binary, After/binary>> = Template,
    NewAcc = <<Acc/binary, Before/binary, Var/binary>>,
    do_insert_at_positions(After, Rest, Offset + AdjustedPos, NewAcc).

%% @doc Cleans up template file after a test
-spec cleanup_template_file(string()) -> ok.
cleanup_template_file(RelPath) ->
    TemplatePath = gabarit_compiler:path(),
    FullPath = filename:join(TemplatePath, RelPath),
    case filelib:is_file(FullPath) of
        true -> file:delete(FullPath);
        false -> ok
    end.

%% @doc Sets up a template file for testing
-spec setup_template_file(template()) -> string().
setup_template_file(Content) ->
    TemplatePath = gabarit_compiler:path(),
    ok = filelib:ensure_dir(filename:join(TemplatePath, "/")),

    % Create a unique filename based on timestamp
    TemplateFilename =
        "test_template_" ++ integer_to_list(erlang:system_time(millisecond)) ++ ".html",
    FullPath = filename:join(TemplatePath, TemplateFilename),

    ok = file:write_file(FullPath, Content),
    TemplateFilename.

%% @doc Creates a context with values for all variables in a template
-spec create_context_for_variables([template_variable()], context()) -> context().
create_context_for_variables(Variables, BaseContext) ->
    lists:foldl(
        fun(Var, Ctx) ->
            VarName = extract_var_name(Var),
            maps:put(VarName, <<"test_value">>, Ctx)
        end,
        BaseContext,
        Variables
    ).

%% @doc Extracts variable name from {{name}} format
-spec extract_var_name(template_variable()) -> binary().
extract_var_name(Var) ->
    % Match {{name}} and extract just "name"
    case binary:match(Var, <<"{{">>) of
        {Start, Len} ->
            StartPos = Start + Len,
            case binary:match(Var, <<"}}">>) of
                {End, _} ->
                    Length = End - StartPos,
                    binary:part(Var, StartPos, Length);
                _ ->
                    <<>>
            end;
        _ ->
            <<>>
    end.

%% @doc Checks if rendered content contains any of the original variables
-spec contains_any_variable(template(), [template_variable()]) -> boolean().
contains_any_variable(Rendered, Variables) ->
    lists:any(
        fun(Var) ->
            binary:match(Rendered, Var) =/= nomatch
        end,
        Variables
    ).
