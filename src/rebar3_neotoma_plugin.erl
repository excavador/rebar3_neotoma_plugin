-module(rebar3_neotoma_plugin).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default}]).

-include_lib("providers/include/providers.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% Called when rebar3 first boots, before even parsing the arguments
%% or commands to be run. Purely initiates the provider, and nothing
%% else should be done here.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
Provider = providers:create([
    {name,       ?PROVIDER                },
    {module,     ?MODULE                  },
    {namespace,  neotoma                  },
    {bare,       false                    },
    {deps,       ?DEPS                    },
    {example,    "rebar3 neotoma compile" },
    {short_desc, "compile peg files."     },
    {desc,       "compile peg files."     },
    {opts,       []                       }
]),
{ok, rebar_state:add_provider(State, Provider)}.

%% Run the code for the plugin. The command line argument are parsed
%% and dependencies have been run.
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running neotoma...", []),
    _Ignore = case rebar_state:get(State, escript_main_app, undefined) of
        undefined ->
            Dir = rebar_state:dir(State),
            case rebar_app_discover:find_app(Dir, all) of
                {true, AppInfo} ->
                    AllApps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
                    case rebar_app_utils:find(rebar_app_info:name(AppInfo), AllApps) of
                        {ok, AppInfo1} ->
                            %% Use the existing app info instead of newly created one
                            run_neotoma(AppInfo1, State);
                        _ ->
                            run_neotoma(AppInfo, State)
                    end,
                        {ok, State};
                _ ->
                    ?PRV_ERROR(no_main_app)
            end;
        _Name ->
            ok
    end,
    {ok, State}.

run_neotoma(App, State) ->
    Source = rebar_app_info:source(App),
    Dir = rebar_state:dir(State),
    rebar_api:debug("source=~p dir=~p", [Source, Dir]),
    rebar_base_compiler:run(State, [], Source, "peg", Source, "erl",
                            fun(SourceF, TargetF, StateF) ->
                                compile_peg(StateF, SourceF, TargetF, [], Dir, SourceF)
                            end,
                            [{check_last_mod, false}]).

compile_peg(_State, Source, Target, _Options, _Dir, OutDir) ->
    case needs_compile(Source, Target) of
        true ->
            neotoma:file(Source, [{output, OutDir}]);
        false ->
            skipped
    end.

needs_compile(Source, Target) ->
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source).


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
