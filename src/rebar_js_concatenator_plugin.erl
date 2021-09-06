-module(rebar_js_concatenator_plugin).

-export([init/1]).
-export([concatenate/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar_js_concatenator_plugin_prv:init(State).

concatenate(Sources) ->
    rebar_js_concatenator_plugin_prv:concatenate(Sources).
