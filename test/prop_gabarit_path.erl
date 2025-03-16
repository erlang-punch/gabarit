%%%-------------------------------------------------------------------
%%% @author Maartz <maartz@protonmail.com>
%%%
%%% @doc
%%% Property-based tests for Gabarit's path handling functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(prop_gabarit_path).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Properties
%%====================================================================

prop_template_file_identifier_is_valid() ->
    ?FORALL(
        {Path, Filename},
        {template_path(), template_filename()},
        begin
            Identifier = gabarit_compiler:template_file_identifier(Path, Filename),
            is_valid_identifier(Identifier, Filename)
        end
    ).

prop_safe_path_is_handled_correctly() ->
    ?FORALL(
        {BasePath, RelPath},
        {template_path(), relative_path()},
        begin
            JoinedPath = filename:join(BasePath, RelPath),
            case filelib:safe_relative_path(RelPath, BasePath) of
                unsafe ->
                    % If unsafe, the property doesn't apply
                    true;
                SafePath ->
                    is_binary(SafePath) orelse is_list(SafePath)
            end
        end
    ).

%%====================================================================
%% Generators
%%====================================================================

template_path() ->
    ?LET(
        Parts,
        list(path_part()),
        case Parts of
            [] ->
                "priv/templates";
            _ ->
                filename:join(["priv/templates" | Parts])
        end
    ).

relative_path() ->
    ?LET(Parts, non_empty(list(path_part())), filename:join(Parts)).

template_filename() ->
    ?LET(Name, filename_base(), Name ++ ".html").

filename_base() ->
    ?LET(Chars, non_empty(list(filename_char())), Chars).

path_part() ->
    ?LET(Chars, non_empty(list(path_char())), Chars).

filename_char() ->
    oneof([choose($a, $z), choose($A, $Z), choose($0, $9), $_, $-]).

path_char() ->
    oneof([choose($a, $z), choose($A, $Z), choose($0, $9), $_, $-]).

%%====================================================================
%% Helper Functions
%%====================================================================

is_valid_identifier(Identifier, Filename) ->
    is_list(Identifier) andalso
        hd(Identifier) =:= $/ andalso
        string:find(Identifier, Filename) =/= nomatch.
