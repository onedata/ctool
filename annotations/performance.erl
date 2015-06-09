%%%--------------------------------------------------------------------
%%% @author Michal Wrzeszcz
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This file contains definitions of annotations used during
%%% performance tests.
%%% @end
%%%--------------------------------------------------------------------
-module(performance).
-author("Michal Wrzeszcz").
-author("Krzysztof Trzepla").
-annotation('function').

% this file is built by parent project so include_lib must be used
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("annotations/include/types.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-export([around_advice/4]).

-type proplist() :: [{Key :: atom(), Value :: term()}].

-define(BRANCH_ENV_VARIABLE, "branch").
-define(PERFORMANCE_ENV_VARIABLE, "performance").
-define(PERFORMANCE_RESULT_FILE, "performance.json").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Function executed instead of annotated function. May start
%% annotated function inside.
%% @end
%%--------------------------------------------------------------------
-spec around_advice(Annotation :: #annotation{}, Module :: atom(),
    Function :: atom(), Args :: [term()]) -> Result :: [atom()] | term().
around_advice(#annotation{data = {test_cases, CasesNames}}, SuiteName, all, []) ->
    case os:getenv(?PERFORMANCE_ENV_VARIABLE) of
        "true" -> CasesNames;
        _ -> annotation:call_advised(SuiteName, all, [])
    end;
around_advice(#annotation{data = Data}, SuiteName, CaseName, [CaseArgs]) ->
    DefaultReps = proplists:get_value(repeats, Data, 1),
    DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
    case os:getenv(?PERFORMANCE_ENV_VARIABLE) of
        "true" ->
            CaseDescr = proplists:get_value(description, Data, ""),
            Configs = proplists:get_all_values(config, Data),
            exec_perf_configs(SuiteName, CaseName, CaseDescr, CaseArgs,
                Configs, DefaultReps, DefaultParams);
        _ ->
            exec_ct_config(SuiteName, CaseName, CaseArgs, DefaultParams)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes common test using non-performance configurations.
%% @end
%%--------------------------------------------------------------------
-spec exec_ct_config(SuiteName :: atom(), CaseName :: atom(),
    CaseArgs :: proplist(), Params :: [#parameter{}]) -> term().
exec_ct_config(SuiteName, CaseName, CaseArgs, Params) ->
    NewCaseArgs = inject_parameters(CaseArgs, Params),
    annotation:call_advised(SuiteName, CaseName, [NewCaseArgs]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes common test case using performance configurations.
%% @end
%%--------------------------------------------------------------------
-spec exec_perf_configs(SuiteName :: atom(), CaseName :: atom(),
    CaseDescr :: string(), CaseArgs :: proplist(), Config :: proplist(),
    DefaultReps :: non_neg_integer(), DefaultParams :: [#parameter{}]) -> ok.
exec_perf_configs(SuiteName, CaseName, CaseDescr, CaseArgs, Configs,
    DefaultReps, DefaultParams) ->
    ?assertEqual(ok, lists:foldl(fun(Config, Status) ->
        case exec_perf_config(SuiteName, CaseName, CaseDescr, CaseArgs,
            Config, DefaultReps, DefaultParams) of
            ok -> Status;
            _ -> error
        end
    end, ok, Configs)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes common test case using performance configuration.
%% @end
%%--------------------------------------------------------------------
-spec exec_perf_config(SuiteName :: atom(), CaseName :: atom(),
    CaseDescr :: string(), CaseArgs :: proplist(), Config :: proplist(),
    DefaultReps :: non_neg_integer(), DefaultParams :: [#parameter{}]) ->
    ok | error.
exec_perf_config(SuiteName, CaseName, CaseDescr, CaseArgs, Config,
    DefaultReps, DefaultParams) ->

    % Fetch and prepare test case configuration.
    TestRoot = proplists:get_value(ct_test_root, CaseArgs),
    ConfigName = proplists:get_value(name, Config),
    ConfigReps = proplists:get_value(repeats, Config, DefaultReps),
    ConfigDescr = proplists:get_value(description, Config, ""),
    % Merge specific configuration test case parameters with default test case
    % parameters, so that specific values overrider default ones.
    ConfigParams = merge_parameters(
        parse_parameters(proplists:get_value(parameters, Config, [])),
        DefaultParams
    ),
    % Inject configuration parameters into common test cases configuration.
    NewCaseArgs = inject_parameters(CaseArgs, ConfigParams),
    {RepsSummary, RepsDetails, FailedReps} =
        exec_test_repeats(SuiteName, CaseName, ConfigName, NewCaseArgs, ConfigReps),

    % Fetch git repository metadata.
    Repository = list_to_binary(proplists:get_value(git_repository, CaseArgs)),
    Branch = list_to_binary(proplists:get_value(git_branch, CaseArgs)),
    Commit = list_to_binary(proplists:get_value(git_commit, CaseArgs)),

    #{<<"performance">> := PerfResults} =
        case file:read_file(?PERFORMANCE_RESULT_FILE) of
            {ok, Json} ->
                jiffy:decode(Json, [return_maps]);
            _ ->
                #{
                    <<"performance">> => #{
                        <<"repository">> => Repository,
                        <<"branch">> => Branch,
                        <<"commit">> => Commit
                    }
                }
        end,

    % Create JSON description of performance configuration execution.
    BinSuiteName = atom_to_binary(SuiteName, utf8),
    BinCaseName = atom_to_binary(CaseName, utf8),
    BinConfigName = atom_to_binary(ConfigName, utf8),
    SuccessfulReps = ConfigReps - maps:size(FailedReps),
    RepsAverage = lists:map(fun(#parameter{value = Value} = Param) ->
        Param#parameter{value = round(Value / SuccessfulReps)}
    end, RepsSummary),

    SuitesMap = maps:get(<<"suites">>, PerfResults, #{}),
    SuiteMap = maps:get(BinSuiteName, SuitesMap, #{
        <<"name">> => BinSuiteName,
        <<"copyright">> => get_copyright(TestRoot, SuiteName),
        <<"authors">> => get_authors(TestRoot, SuiteName),
        <<"description">> => get_description(TestRoot, SuiteName)
    }),
    CasesMap = maps:get(<<"cases">>, SuiteMap, #{}),
    CaseMap = maps:get(BinCaseName, CasesMap, #{
        <<"name">> => BinCaseName,
        <<"description">> => list_to_binary(CaseDescr)
    }),
    ConfigsMap = maps:get(<<"configs">>, CaseMap, #{}),
    ConfigMap = #{
        <<"name">> => BinConfigName,
        <<"completed">> => get_timestamp(),
        <<"parameters">> => format_parameters(ConfigParams),
        <<"description">> => list_to_binary(ConfigDescr),
        <<"repeats_number">> => ConfigReps,
        <<"successful_repeats_number">> => SuccessfulReps,
        <<"successful_repeats_summary">> => format_parameters(RepsSummary),
        <<"successful_repeats_average">> => format_parameters(RepsAverage),
        <<"successful_repeats_details">> => format_parameters(RepsDetails),
        <<"failed_repeats_details">> => FailedReps
    },

    NewPerfResults = PerfResults#{
        <<"suites">> =>maps:put(BinSuiteName, SuiteMap#{
            <<"cases">> => maps:put(BinCaseName, CaseMap#{
                <<"configs">> => maps:put(BinConfigName, ConfigMap, ConfigsMap)
            }, CasesMap)
        }, SuitesMap)
    },

    NewJson = jiffy:encode(#{<<"performance">> => NewPerfResults}, [pretty]),
    file:write_file(?PERFORMANCE_RESULT_FILE, NewJson),

    % Check whether performance configuration execution has been successfully
    % completed.
    case maps:size(FailedReps) of
        0 -> ok;
        _ -> error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes test case multiple times.
%% @end
%%--------------------------------------------------------------------
-spec exec_test_repeats(SuiteName :: atom(), CaseName :: atom(), ConfigName :: atom(),
    CaseConfig :: proplist(), Reps :: integer()) -> {RepsSummary :: [#parameter{}],
    RepsDetails :: [#parameter{}], FailedReps :: map()}.
exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Reps) ->
    exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, 1, Reps + 1, [], [], #{}).
exec_test_repeats(_SuiteName, _CaseName, _ConfigName, _CaseConfig, Reps, Reps,
    RepsSummary, RepsDetails, FailedReps) ->
    {RepsSummary, RepsDetails, FailedReps};
exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep, Reps,
    RepsSummary, RepsDetails, FailedReps) ->
    ct:print("SUITE: ~p~nCASE: ~p~nCONFIG: ~p~nREPEAT: ~p / ~p (~p%)",
        [SuiteName, CaseName, ConfigName, Rep, Reps - 1, round(100 * Rep / (Reps - 1))]),
    BinRep = integer_to_binary(Rep),
    case exec_test_repeat(SuiteName, CaseName, CaseConfig) of
        {ok, Params} ->
            case RepsSummary of
                [] ->
                    % Initialize list of parameters for test case repeats.
                    NewRepsDetails = lists:map(fun
                        (#parameter{value = Value} = Param) ->
                            Param#parameter{value = maps:put(BinRep, Value, #{})}
                    end, Params),
                    exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
                        Reps, Params, NewRepsDetails, FailedReps);
                _ ->
                    % Merge list of test case parameters from current test case
                    % repeat with list of parameters from previous test case repeats.
                    NewRepsSummary = lists:zipwith(fun
                        (#parameter{value = Value1}, #parameter{value = Value2} = Param) ->
                            Param#parameter{value = Value1 + Value2}
                    end, Params, RepsSummary),
                    NewRepsDetails = lists:zipwith(fun
                        (#parameter{value = Value1}, #parameter{value = Value2} = Param) ->
                            Param#parameter{value = maps:put(BinRep, Value1, Value2)}
                    end, Params, RepsDetails),
                    exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
                        Reps, NewRepsSummary, NewRepsDetails, FailedReps)
            end;
        {error, Reason} ->
            NewRepsDetails = lists:map(fun(#parameter{value = Value} = Param) ->
                Param#parameter{value = maps:put(BinRep, 0, Value)}
            end, RepsDetails),
            NewFailedReps = maps:put(BinRep, Reason, FailedReps),
            exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
                Reps, RepsSummary, NewRepsDetails, NewFailedReps)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes test case once.
%% @end
%%--------------------------------------------------------------------
-spec exec_test_repeat(SuiteName :: atom(), CaseName :: atom(), CaseConfig :: proplist()) ->
    {ok, [#parameter{}]} | {error, Reason :: binary()}.
exec_test_repeat(SuiteName, CaseName, CaseConfig) ->
    try
        Timestamp1 = os:timestamp(),
        Result = annotation:call_advised(SuiteName, CaseName, [CaseConfig]),
        Timestamp2 = os:timestamp(),
        TestTime = utils:milliseconds_diff(Timestamp2, Timestamp1),
        % Return list of parameters consisting of default 'test_time' parameter
        % and parameters returned from test case.
        {ok, [#parameter{
            name = test_time,
            description = "Test execution time.",
            value = TestTime,
            unit = "ms"} | lists:filter(fun
            (#parameter{}) -> true;
            (_) -> false
        end, lists:flatten([Result]))]}
    catch
        Error:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            Message = gui_str:format("~p:~p~n~p", [Error, Reason, Stacktrace]),
            {error, list_to_binary(Message)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms list of parameters proplist into list of 'parameter' records.
%% @end
%%--------------------------------------------------------------------
-spec parse_parameters(Params :: proplist()) -> ParsedParams :: [#parameter{}].
parse_parameters(Params) ->
    lists:sort(lists:map(fun(Param) ->
        #parameter{
            name = proplists:get_value(name, Param),
            value = proplists:get_value(value, Param),
            unit = proplists:get_value(unit, Param, ""),
            description = proplists:get_value(description, Param, "")
        }
    end, Params)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms list of in/out parameters into list of mappings from parameter
%% field name to its value.
%% @end
%%--------------------------------------------------------------------
-spec format_parameters(Params :: [#parameter{}]) -> [map()].
format_parameters(Params) ->
    lists:map(fun
        (#parameter{name = Name, value = Value, unit = Unit, description = Descr}) ->
            #{
                <<"name">> => Name,
                <<"value">> => Value,
                <<"unit">> => list_to_binary(Unit),
                <<"description">> => list_to_binary(Descr)
            }
    end, Params).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Injects additional parameters into common test configuration.
%% @end
%%--------------------------------------------------------------------
-spec inject_parameters(Config :: proplist(), Params :: [#parameter{}]) ->
    NewConfig :: proplist().
inject_parameters(Config, Params) ->
    lists:foldl(fun(#parameter{name = Name, value = Value}, ConfigAcc) ->
        [{Name, Value} | ConfigAcc]
    end, Config, Params).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @equiv merge_parameters(ConfigParams, DefaultParams, [])
%% @end
%%--------------------------------------------------------------------
-spec merge_parameters(ConfigParams :: [#parameter{}], DefaultParams :: [#parameter{}]) ->
    MergedParams :: [#parameter{}].
merge_parameters(ConfigParams, DefaultParams) ->
    merge_parameters(ConfigParams, DefaultParams, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Merges list of config parameters and list of default parameters, so that
%% default parameter value is overwritten by specific one.
%% @end
%%--------------------------------------------------------------------
-spec merge_parameters(ConfigParams :: [#parameter{}], DefaultParams :: [#parameter{}],
    MergedParamsAcc :: [#parameter{}]) -> MergedParams :: [#parameter{}].
merge_parameters([], [], MergedParams) ->
    lists:reverse(MergedParams);
merge_parameters([ConfigParam | ConfigParams], [], MergedParams) ->
    merge_parameters(ConfigParams, [], [ConfigParam | MergedParams]);
merge_parameters([], [DefaultParam | DefaultParams], MergedParams) ->
    merge_parameters([], DefaultParams, [DefaultParam | MergedParams]);
merge_parameters([#parameter{name = Name} = ConfigParam | ConfigParams],
    [#parameter{name = Name} = DefaultParam | DefaultParams], MergedParams) ->
    MergedParam = merge_parameter(ConfigParam, DefaultParam),
    merge_parameters(ConfigParams, DefaultParams, [MergedParam | MergedParams]);
merge_parameters([#parameter{name = Name1} = ConfigParam | ConfigParams],
    [#parameter{name = Name2} | _] = DefaultParams, MergedParams) when Name1 < Name2 ->
    merge_parameters(ConfigParams, DefaultParams, [ConfigParam | MergedParams]);
merge_parameters(ConfigParams, [DefaultParam | DefaultParams], MergedParams) ->
    merge_parameters(ConfigParams, DefaultParams, [DefaultParam | MergedParams]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Merges config parameter with default parameter, so that if unit or description
%% of config parameter is missing corresponding value of default parameter is
%% substituted.
%% @end
%%--------------------------------------------------------------------
-spec merge_parameter(ConfigParam :: #parameter{}, DefaultParam :: #parameter{}) ->
    MergeParam :: #parameter{}.
merge_parameter(#parameter{unit = Unit1, description = Descr1} = ConfigParam,
    #parameter{unit = Unit2, description = Descr2}) ->
    ConfigParam#parameter{
        unit = case Unit1 of "" -> Unit2; _ -> Unit1 end,
        description = case Descr1 of "" -> Descr2; _ -> Descr1 end
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns current timestamp in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec get_timestamp() -> integer().
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns list of test suite authors.
%% @end
%%--------------------------------------------------------------------
-spec get_authors(TestRoot :: string(), SuiteName :: atom()) ->
    [Author :: binary()].
get_authors(TestRoot, SuiteName) ->
    SuiteFile = filename:join(TestRoot, atom_to_list(SuiteName) ++ ".erl"),
    {SuiteName, Doc} = edoc:get_doc(SuiteFile),
    lists:filtermap(fun
        (#xmlElement{name = author, attributes = Attributes}) ->
            {true, hd(lists:filtermap(fun
                (#xmlAttribute{name = name, value = Name}) ->
                    {true, list_to_binary(Name)};
                (_) -> false
            end, Attributes))};
        (_) -> false
    end, Doc#xmlElement.content).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns test suite description.
%% @end
%%--------------------------------------------------------------------
-spec get_description(TestRoot :: string(), SuiteName :: atom()) ->
    Descr :: binary().
get_description(TestRoot, SuiteName) ->
    SuiteFile = filename:join(TestRoot, atom_to_list(SuiteName) ++ ".erl"),
    {SuiteName, Doc} = edoc:get_doc(SuiteFile),
    hd(lists:filtermap(fun
        (#xmlElement{name = description, content = Descr}) ->
            {true, hd(lists:filtermap(fun
                (#xmlElement{name = fullDescription, content = [FullDescr]}) ->
                    {true, list_to_binary(FullDescr#xmlText.value)};
                (_) -> false
            end, Descr))};
        (_) -> false
    end, Doc#xmlElement.content)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns test suite copyright.
%% @end
%%--------------------------------------------------------------------
-spec get_copyright(TestRoot :: string(), SuiteName :: atom()) ->
    Copyright :: binary().
get_copyright(TestRoot, SuiteName) ->
    SuiteFile = filename:join(TestRoot, atom_to_list(SuiteName) ++ ".erl"),
    {SuiteName, Doc} = edoc:get_doc(SuiteFile),
    hd(lists:filtermap(fun
        (#xmlElement{name = copyright, content = [Copyright]}) ->
            {true, list_to_binary(Copyright#xmlText.value)};
        (_) -> false
    end, Doc#xmlElement.content)).
