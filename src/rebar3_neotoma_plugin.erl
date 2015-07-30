-module(rebar3_neotoma_plugin).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).

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
    run_neotoma(State).

run_neotoma(State) ->
    Dir = filename:join(rebar_state:dir(State), "src"),
    rebar_base_compiler:run(State, [], Dir, ".peg", Dir, ".erl",
                            fun(SourceF, TargetF, StateF) ->
                                compile_peg(StateF, SourceF, TargetF, [], Dir, SourceF)
                            end,
                            [{check_last_mod, false}]),
    {ok, State}.

compile_peg(_State, Source, Target, _Options, Dir, _) ->
    case needs_compile(Source, Target) of
        true ->
            neotoma:file(Source, [{output, Dir}]);
        false ->
            skipped
    end.

needs_compile(Source, Target) ->
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source).


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
