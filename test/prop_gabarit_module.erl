%%%-------------------------------------------------------------------
%%% @author Maartz <maartz@protonmail.com>
%%%
%%% @doc
%%% Property-based tests for Gabarit's module generation functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(prop_gabarit_module).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Properties
%%====================================================================

prop_module_name_is_valid_atom() ->
    ?FORALL(
        {Prefix, Identifier},
        {module_prefix(), module_identifier()},
        begin
            Result = gabarit_module:name(Prefix, Identifier),
            case Result of
                {ok, ModuleName} ->
                    is_atom(ModuleName) andalso
                        atom_to_list(ModuleName) =:= Prefix ++ Identifier;
                ModuleName when is_atom(ModuleName) ->
                    atom_to_list(ModuleName) =:= Prefix ++ Identifier;
                _ ->
                    false
            end
        end
    ).

prop_module_exist_returns_correct_result() ->
    ?FORALL(
        ModuleName,
        module_name(),
        begin
            ExistingModules = [gabarit, gabarit_app, gabarit_sup, gabarit_module, gabarit_compiler],
            ShouldExist = lists:member(ModuleName, ExistingModules),
            Result = gabarit_module:exist(atom_to_list(ModuleName)),
            Result =:= ShouldExist orelse Result =:= ModuleName
        end
    ).

%%====================================================================
%% Generators
%%====================================================================

module_prefix() ->
    oneof(["gabarit@", "gabarit$"]).

module_identifier() ->
    ?LET(
        Parts,
        resize(3, list(resize(5, valid_path_part()))),
        begin
            SafeParts =
                case Parts of
                    [] -> ["default"];
                    _ -> Parts
                end,

            Path = "/" ++ filename:join(SafeParts),

            case length(Path) > 100 of
                true -> string:slice(Path, 0, 100);
                false -> Path
            en
        end
    ).

module_name() ->
    oneof([
        gabarit,
        gabarit_app,
        gabarit_sup,
        gabarit_compiler,
        gabarit_module,
        gabarit_support,
        gabarit_store,
        'gabarit@/nonexistent'
    ]).

valid_path_part() ->
    ?LET(Chars, non_empty(list(path_char())), Chars).

path_char() ->
    oneof([choose($a, $z), choose($A, $Z), choose($0, $9), $_, $-]).
