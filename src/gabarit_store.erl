%%%===================================================================
%%%
%%%===================================================================
-module(gabarit_store).
-behavior(gen_statem).
-compile(export_all).
-record(state, {store :: reference()}).

callback_mode() -> state_functions.

init(_Args) ->
    Ets = ets:new(?MODULE, [private]),
    {ok, unlocked, #state{store = Ets}}.

unlocked(_, _, Data) ->
    {keep_state, Data}.

locked(_, _, Data) ->
    {keep_state, Data}.
