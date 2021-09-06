-module(rebar_js_concatenator_plugin_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).
-export([concatenate/1]).

-define(PROVIDER, rebar_js_concatenator_plugin).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, false},
            {deps, ?DEPS},
            {example, "rebar3 rebar_js_concatenator_plugin"},
            {opts, []},
            {short_desc, "Concatenates js assets into bundle.js"},
            {desc, "A rebar3 plugins to concatenate JS files into a bundle"}
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
         AppDir = rebar_app_info:dir(AppInfo),
         AppOpts = rebar_app_info:opts(AppInfo),

         Opts = rebar_opts:get(AppOpts, js_concatenator, []),
         Concatenations = option(concatenations, Opts),
         OutDir = filename:join([AppDir, option(out_dir, Opts)]),
         DocRoot = filename:join([AppDir, option(doc_root, Opts)]),
         Targets = [{normalize_path(Destination, OutDir),
                     normalize_paths(Sources, DocRoot),
                     ConcatOptions} || {Destination, Sources, ConcatOptions} <- Concatenations],
         build_each(Targets)
     end || AppInfo <- Apps],
    {ok, State}.

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root) -> "priv/assets/javascripts";
default(out_dir)  -> "priv/assets/javascripts";
default(concatenations) -> [].

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


read(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
           Binary;
        {error, Reason} ->
           rebar_api:error("Reading asset ~s failed: ~p", [File, Reason]),
           rebar_utils:abort()
    end.

normalize_paths(Paths, Basedir) ->
    lists:foldr(fun(X, Acc) ->
                [normalize_path(X, Basedir) | Acc] end, [], Paths).
normalize_path(Path, Basedir) ->
    filename:join([Basedir, Path]).


build_each([]) ->
    ok;
build_each([{Destination, Sources, ConcatOptions} | Rest]) ->
    rebar_api:debug("Building ~s from ~p", [Destination, Sources]),
    case any_needs_concat(Sources, Destination) of
        true ->
            Contents = concatenate_files(Sources),
            case file:write_file(Destination, Contents, [write]) of
                ok ->
                    rebar_api:info("Built asset ~s", [Destination]),
                    case lists:member(uglify, ConcatOptions) of
                        true ->
                            uglify(Destination);
                        false ->
                            ok
                    end;
                {error, Reason} ->
                    rebar_api:error("Building asset ~s failed: ~p",
                           [Destination, Reason]),
                    rebar_utils:abort()
            end;
        false ->
            ok
    end,
    build_each(Rest).

any_needs_concat(Sources, Destination) ->
    lists:any(fun(X) -> needs_concat(X, Destination) end, Sources).
needs_concat(Source, Destination) ->
    filelib:last_modified(Destination) < filelib:last_modified(Source).

concatenate(Sources) ->
    iolist_to_binary([Sources]).

concatenate_files(Sources) ->
    concatenate([read(Source) || Source <- Sources]).

uglify(Source) ->
    %% Destination = uglify_destination(Source),
    rebar_api:warning("Nah, that's one plugin too much: rebar_js_uglifier_plugin:compress(Source, Destination, []).", []),
    Source.

%% uglify_destination(Source) ->
%%     Outdir = filename:dirname(Source),
%%     Basename = filename:basename(Source, ".js"),
%%     normalize_path(lists:flatten(Basename ++ ".min.js"), Outdir).


%% delete_each([]) ->
%%     ok;
%% delete_each([First | Rest]) ->
%%     case file:delete(First) of
%%         ok ->
%%             ok;
%%         {error, enoent} ->
%%             ok;
%%         {error, Reason} ->
%%             rebar_api:error("Failed to delete ~s: ~p", [First, Reason])
%%     end,
%%     delete_each(Rest).
