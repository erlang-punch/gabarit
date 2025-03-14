-module(gabarit_module).
-compile(export_all).
-define(DEFAULT_PREFIX_FILE, "gabarit@").
-define(DEFAULT_PREFIX_STRING, "gabarit$").

name(Prefix, Identifier) ->
    ModuleName = string:concat(Prefix, Identifier),
    try
        {ok, erlang:list_to_existing_atom(ModuleName)}
    catch
        _:_ -> erlang:list_to_atom(ModuleName)
    end.

exist(Name) ->
    try
        _Module = erlang:list_to_existing_atom(Name)
    catch
        _:_ -> false
    end.
