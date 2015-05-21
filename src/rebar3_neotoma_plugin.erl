-module(rebar3_neotoma_plugin).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

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
    case rebar_state:get(State, escript_main_app, undefined) of
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
        Name ->
            ok
    end,
    {ok, State}.

run_neotoma(App, State) ->
    Source = rebar_app_info:source(App),
    Dir = rebar_state:dir(State),
    rebar_api:debug("source=~p dir=~p", [Source, Dir]),
    rebar_base_compiler:run(State,
                            [],
                            Source,
                            "peg",
                            Source,
                            "erl",
                            fun(Source, Target, State) ->
                                    compile_peg(State, Source, Target, [], Dir, Source)
                            end,
                            [{check_last_mod, false},
                             {recursive, option(recursive, [])}]).

compile_peg(State, Source, Target, DtlOpts, Dir, OutDir) ->
    case needs_compile(Source, Target, DtlOpts) of
        true ->
            do_compile(State, Source, Target, DtlOpts, Dir, OutDir);
        false ->
            skipped
    end.

do_compile(State, Source, Target, DtlOpts, Dir, OutDir) ->
    eof.
%    CompilerOptions = option(compiler_options, DtlOpts),
%
%    Sorted = proplists:unfold(
%               lists:sort(
%                 [{out_dir, OutDir},
%                  {doc_root, filename:join(Dir, option(doc_root, DtlOpts))},
%                  {custom_tags_dir, option(custom_tags_dir, DtlOpts)},
%                  {compiler_options, CompilerOptions}])),
%
%    %% ensure that doc_root and out_dir are defined,
%    %% using defaults if necessary
%    Opts = lists:ukeymerge(1, DtlOpts, Sorted),
%    rebar_api:debug("Compiling \"~s\" -> \"~s\" with options:~n    ~s",
%                    [Source, Target, io_lib:format("~p", [Opts])]),
%    case erlydtl:compile_file(ec_cnv:to_list(Source),
%                              list_to_atom(module_name(Target)),
%                              Opts) of
%        {ok, _Mod} ->
%            ok;
%        {ok, _Mod, Ws} ->
%            rebar_base_compiler:ok_tuple(State, Source, Ws);
%        error ->
%            rebar_base_compiler:error_tuple(State, Source, [], [], Opts);
%        {error, Es, Ws} ->
%            rebar_base_compiler:error_tuple(State, Source, Es, Ws, Opts)
%    end.

%% When an exception is raised or a value returned as
%% `{error, {?MODULE, Reason}}` will see the `format_error(Reason)`
%% function called for them, so a string can be formatted explaining
%% the issue.
-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
