-module(js_fumble_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, js_fumble).
-define(DEPS, [{default, compile}, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, js_fumble},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 js_fumble"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running js_concatenator...", []),
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         _Dir = rebar_app_info:dir(AppInfo),
         Opts0 = rebar_app_info:opts(AppInfo),
         rebar_api:debug("Opts0 ~p", [Opts0]),

         Opts1 = proplists:unfold(
                      rebar_opts:get(Opts0, js_concatenator, [])),
         rebar_api:debug("Opts1 ~p", [Opts1]),
         Concatenations = rebar_opts:get(Opts1, concatenations),
         rebar_api:debug("Concatenations ~p", [Concatenations])

     end || AppInfo <- Apps],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
