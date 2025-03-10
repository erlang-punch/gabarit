%%%===================================================================
%%% @doc Module naming utilities for Gabarit template system.
%%%
%%% This module provides functions for dynamically creating and managing
%%% module names used by compiled templates. It handles atom creation
%%% safely to prevent atom table exhaustion and ensures consistent
%%% naming patterns across the application.
%%% @end
%%%===================================================================
-module(gabarit_module).

-export([
    name/2,
    exist/1
]).

-define(DEFAULT_PREFIX_FILE, "gabarit@").
-define(DEFAULT_PREFIX_STRING, "gabarit$").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Creates a safe, deterministic module name from a prefix and identifier.
%%
%% This function builds module names for dynamically compiled templates by:
%% 1. Converting the identifier to a string (directly for integers, via hash for other terms)
%% 2. Truncating long identifiers to 200 characters to prevent excessively long names
%% 3. Safely creating atoms, reusing existing ones when possible
%%
%% @param Prefix A string prefix for the module name (e.g., "gabarit@")
%% @param Identifier A unique identifier for the template (integer or term)
%% @returns atom() A module name as an atom
%% @end
name(Prefix, Identifier) ->
    IdString = case is_integer(Identifier) of
        true ->
            integer_to_list(Identifier);
        false ->
            Hash = erlang:phash2(Identifier),
            integer_to_list(Hash)
    end,

    SafeId = if
        length(IdString) > 200 ->
            string:substr(IdString, 1, 200);
        true ->
            IdString
    end,

    ModuleName = string:concat(Prefix, SafeId),

    try
        erlang:list_to_existing_atom(ModuleName)
    catch
        error:badarg ->
            erlang:list_to_atom(ModuleName)
    end.

%% @doc Checks if a module with the given name exists.
%%
%% This function tests whether an atom already exists for the given module name
%% without creating a new atom if it doesn't exist.
%%
%% @param Name A string containing the module name to check
%% @returns boolean() true if the module name exists as an atom, false otherwise
%% @end
exist(Name) ->
    try
        _Module = erlang:list_to_existing_atom(Name),
        true
    catch
        error:badarg ->
            false
    end.
