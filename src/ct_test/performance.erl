%%%--------------------------------------------------------------------
%%% @author Michal Wrzeszcz
%%% @author Krzysztof Trzepla
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This file contains definitions of macros used during
%%% performance tests.
%%% @end
%%%--------------------------------------------------------------------
-module(performance).

-author("Michal Wrzeszcz").
-author("Krzysztof Trzepla").
-author("Jakub Kudzia").

% Because this module include indirectly eunit.hrl Eunit considers this as a test module by default.
-define(EUNIT_NOAUTO, 1).

% this file is built by parent project so include_lib must be used
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("common_test/include/ct.hrl").

-export([is_standard_test/0, is_stress_test/0, stress_test/2, should_clear/1,
    all/2, stress_all/3, run_stress_test/3, run_test/4]).

-type proplist() :: proplists:proplist().

-define(BRANCH_ENV_VARIABLE, "branch").
-define(PERFORMANCE_ENV_VARIABLE, "performance").
-define(PERFORMANCE_RESULT_FILE, "performance.json").
-define(STRESS_ENV_VARIABLE, "stress").
-define(STRESS_NO_CLEARING_ENV_VARIABLE, "stress_no_clearing").
-define(STRESS_TIME_ENV_VARIABLE, "stress_time").
-define(STRESS_DEFAULT_TIME, timer:hours(3) div 1000).
-define(STRESS_ERRORS_TO_STOP, 20).
-define(STRESS_ETS_NAME, stress_ets).
-define(STRESS_TIMEOUT_EXTENSION_SECONDS, 600). % extension of ct timeout to let running tests end

-define(BASE_SUFFIX, "_base").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is meant to replace all() function in ct test suite
%% It checks what kind (normal ct, performance, stress) of test is started
%% and returns list of appropriate test cases.
%% @end
%%--------------------------------------------------------------------
-spec all(CasesNames :: [atom()], PerformanceCasesNames :: [atom()]) -> [atom()].
all(CasesNames, PerformanceCasesNames) ->
    case os:getenv(?PERFORMANCE_ENV_VARIABLE) of
        "true" ->
            PerformanceCasesNames;
        _ ->
            case is_stress_test() of
                true ->
                    [];
                _ ->
                    CasesNames
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns list of stress test cases in given suite.
%% @end
%%--------------------------------------------------------------------
-spec stress_all(SuiteName :: atom(), CasesNames :: [atom()],
    NoClearingCasesNames :: [atom()]) -> any().
stress_all(SuiteName, CasesNames, NoClearingCasesNames) ->
    case is_stress_test() of
        true ->
            set_up_stress_test(
                SuiteName, CasesNames, NoClearingCasesNames
            );
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc
%% Function returns information if test is integration test (not performance or stress).
%% @end
%%--------------------------------------------------------------------
-spec is_standard_test() -> boolean().
is_standard_test() ->
    Envs = [os:getenv(?PERFORMANCE_ENV_VARIABLE),
        os:getenv(?STRESS_ENV_VARIABLE), os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE)],
    not lists:member("true", Envs).

%%--------------------------------------------------------------------
%% @doc
%% Checks if current run is stress test.
%% @end
%%--------------------------------------------------------------------
-spec is_stress_test() -> boolean().
is_stress_test() ->
    Envs = [os:getenv(?STRESS_ENV_VARIABLE), os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE)],
    lists:member("true", Envs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs given test case in given suite.
%% @end
%%--------------------------------------------------------------------
-spec run_test(atom(), atom(), term(), Data :: [term()]) -> list().
run_test(_SuiteName, _CaseName, get_params, Data) ->
    get_config_params(Data);
run_test(SuiteName, CaseName, CaseArgs, Data) ->
    case is_stress_test() of
        true ->
            ConfigParams = get_config_params(Data),
            NewCaseArgs = inject_parameters(CaseArgs, ConfigParams),
            Configs = case os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE) of
                          "true" ->
                              [{clearing, false} | NewCaseArgs];
                          _ ->
                              NewCaseArgs
                      end,
            exec_test_repeat(SuiteName, CaseName, Configs);
        _ ->
            run_testcase(SuiteName, CaseName, CaseArgs, Data)
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function runs stress tests in given suite.
%% @end
%%--------------------------------------------------------------------
-spec run_stress_test(SuiteName :: atom(), CaseArgs :: term(),
    Data :: [term()]) -> any().
run_stress_test(SuiteName, CaseArgs, Data) ->
    CaseDescr = proplists:get_value(description, Data, ""),
    Configs = proplists:get_all_values(config, Data),
    DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
    DefaultSuccessRate = proplists:get_value(success_rate, Data, 100),
    Ans = exec_perf_configs(SuiteName, stress_test, CaseArgs, CaseDescr,
        Configs, 1, DefaultSuccessRate, DefaultParams),
    Ans.

%%--------------------------------------------------------------------
%% @doc
%% Checks if function should clear or leave changes (e.g. docs in DB).
%% @end
%%--------------------------------------------------------------------
-spec should_clear(Config :: list()) -> boolean().
should_clear(Config) ->
    ?config(clearing, Config) =/= false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Basic function for stress test.
%% @end
%%--------------------------------------------------------------------
-spec stress_test(Config :: list(), Suite :: atom()) -> list() | no_return().
stress_test(Config, Suite) ->
    [{{cases, Suite},  Cases}] = ets:lookup(?STRESS_ETS_NAME, {cases, Suite}),
    [{timeout, Timeout}] = ets:lookup(?STRESS_ETS_NAME, timeout),
    ct:timetrap({seconds, Timeout + ?STRESS_TIMEOUT_EXTENSION_SECONDS}), % add 10 minutes to let running tests end

    AnsList = lists:foldl(fun(Case, Ans) ->
        [{{history, Suite, Case}, {Rep, FailedReps, LastFails}}] = ets:lookup(?STRESS_ETS_NAME, {history, Suite, Case}),
        case LastFails of
            permanent_error ->
                SkipMessage = str_utils:format("Case: ~p, skipped (too many fails)", [Case]),
                [{error, list_to_binary(SkipMessage)} | Ans];
            _ ->
                CaseAns = try
                              ExtendedConfig =
                                  [{rep_num, Rep}, {failed_num, FailedReps}, {last_fails, LastFails}] ++ Config,
                              NewConfig = try
                                              apply(Suite, init_per_testcase, [Case, ExtendedConfig])
                                          catch
                                              error:undef ->
                                                  ExtendedConfig
                                          end,
                              case apply(Suite, Case, [NewConfig]) of
                                  {ok, TmpAns} ->
%%                           TmpAns2 = lists:map(fun(Param) ->
%%                               PName = Param#parameter.name,
%%                               Param#parameter{name = concat_atoms(Case, PName)}
%%                           end, TmpAns),
%%                           [{ok, TmpAns2} | Ans];
                                      [{ok, TmpAns} | Ans];
                                  {error, E} ->
                                      Message = str_utils:format("Case: ~p, error: ~p", [Case, E]),
                                      [{error, list_to_binary(Message)} | Ans]
                              end
                          catch
                              E1:E2 ->
                                  % only init_per_testcase can throw (case has catch inside)
                                  ct:print("Case: ~p, init_per_testcase error: ~p:~p", [Case, E1, E2]),
                                  Message2 = str_utils:format("Case: ~p, init_per_testcase error: ~p:~p", [Case, E1, E2]),
                                  [{error, list_to_binary(Message2)} | Ans]
                          end,
                try
                    apply(Suite, end_per_testcase, [Case, Config])
                catch
                    error:undef ->
                        ok;
                    E1_2:E2_2 ->
                        ct:print("Case: ~p, end_per_testcase error: ~p:~p", [Case, E1_2, E2_2])
                end,
                CaseAns
        end
    end, [], Cases),
    lists:reverse(AnsList).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of test's parameters or empty list if test is
%% standard or performance.
%% @end
%%--------------------------------------------------------------------
-spec get_stress_test_params(Suite :: atom()) -> list().
get_stress_test_params(Suite) ->
    case is_stress_test() of
        true ->
            [{{cases, Suite}, Cases}] = ets:lookup(?STRESS_ETS_NAME, {cases, Suite}),

            lists:foldl(fun(Case, Ans) ->
                Params = apply(Suite, Case, [get_params]),
                Params2 = lists:map(fun(Param) ->
                    PName = Param#parameter.name,
                    Param#parameter{name = concat_atoms(Case, PName)}
                end, Params),
                Params2 ++ Ans
            end, [], Cases);
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs given ct or performance test case in given suite.
%% @end
%%--------------------------------------------------------------------
-spec run_testcase(SuiteName :: atom(), CaseName :: atom(), CaseArgs :: list(),
    Data :: list()) -> term().
run_testcase(SuiteName, CaseName, CaseArgs, Data) ->
    DefaultReps = proplists:get_value(repeats, Data, 1),
    DefaultSuccessRate = proplists:get_value(success_rate, Data, 100),
    DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
    case is_standard_test() of
        false ->
            CaseDescr = proplists:get_value(description, Data, ""),
            Configs = proplists:get_all_values(config, Data),
            exec_perf_configs(SuiteName, CaseName, CaseArgs, CaseDescr,
                Configs, DefaultReps, DefaultSuccessRate, DefaultParams);
        _ ->
            exec_ct_config(SuiteName, CaseName, CaseArgs, DefaultParams)
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function saves suite and cases names for stress test.
%% @end
%%--------------------------------------------------------------------
-spec set_up_stress_test(SuiteName :: atom(), CasesNames :: [atom()],
    NoClearing :: [atom()]) -> any().
set_up_stress_test(SuiteName, CasesNames, NoClearingCases) ->
    case {os:getenv(?STRESS_ENV_VARIABLE), os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE)} of
        {"true", _} ->
            save_suite_and_cases(SuiteName, CasesNames);
        {_, "true"} ->
            save_suite_and_cases(SuiteName, NoClearingCases)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Saves information about tests to be done during stress test.
%% @end
%%--------------------------------------------------------------------
-spec save_suite_and_cases(Suite :: atom(), Cases :: list()) -> list().
save_suite_and_cases(Suite, Cases) ->
    case Cases of
        [] ->
            [];
        _ ->
            Pid = self(),
            case ets:info(?STRESS_ETS_NAME) of
                undefined ->
                    spawn(fun() ->
                        try
                            ets:new(?STRESS_ETS_NAME, [set, public, named_table]),
                            Pid ! ets_created,
                            receive
                                kill_ets_owner -> ok
                            end
                        catch
                            _:_ -> ok % ct may call all for single suite more than once
                        end
                    end);
                _ ->
                    Pid ! ets_created
            end,
            receive
                ets_created ->
                    ets:insert(?STRESS_ETS_NAME, {{cases, Suite}, Cases})
            end,
            [stress_test]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gets parameters for stress test from annotation data.
%% @end
%%--------------------------------------------------------------------
-spec get_config_params(Data :: list()) -> list().
get_config_params(Data) ->
    DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
    Config = case proplists:get_all_values(config, Data) of
                 [] ->
                     [];
                 [C1 | _] ->
                     C1
             end,
    merge_parameters(
        parse_parameters(proplists:get_value(parameters, Config, [])),
        DefaultParams
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Concatenates two atoms.
%% @end
%%--------------------------------------------------------------------
-spec concat_atoms(A1 :: atom(), A2 :: atom()) -> atom().
concat_atoms(A1, A2) ->
    list_to_atom(atom_to_list(A1) ++ "_" ++ atom_to_list(A2)).

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
    apply(SuiteName, base_case(CaseName), [NewCaseArgs]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes common test case using performance configurations.
%% @end
%%--------------------------------------------------------------------
-spec exec_perf_configs(SuiteName :: atom(), CaseName :: atom(),
    CaseArgs :: proplist(), CaseDescr :: string(), Config :: proplist(),
    DefaultReps :: non_neg_integer(), DefaultSuccessRate :: number(),
    DefaultParams :: [#parameter{}]) -> ok.
exec_perf_configs(SuiteName, CaseName, CaseArgs, CaseDescr, Configs,
    DefaultReps, DefaultSuccessRate, DefaultParams) ->
    {Time, _Scale} = ct:get_timetrap_info(),
    Multiplier = lists:foldl(fun(Config, Sum) ->
        Sum + proplists:get_value(repeats, Config, DefaultReps)
    end, 0, Configs),
    ct:timetrap(Time * Multiplier),
    ?assertEqual(ok, lists:foldl(
        fun(Config, Status) ->
            case exec_perf_config(SuiteName, CaseName, CaseArgs, CaseDescr,
                Config, DefaultReps, DefaultSuccessRate, DefaultParams) of
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
    CaseArgs :: proplist(), CaseDescr :: string(), Config :: proplist(),
    DefaultReps :: non_neg_integer(), DefaultSuccessRate :: number(),
    DefaultParams :: [#parameter{}]) -> ok | error.
exec_perf_config(SuiteName, CaseName, CaseArgs, CaseDescr, Config, DefaultReps,
    DefaultSuccessRate, DefaultParams) ->

    % Fetch and prepare test case configuration.
    TestRoot = proplists:get_value(ct_test_root, CaseArgs),
    ConfigName = proplists:get_value(name, Config),
    ConfigDescr = proplists:get_value(description, Config, ""),
    % Merge specific configuration test case parameters with default test case
    % parameters, so that specific values overrider default ones.
    ConfigParams = merge_parameters(
        parse_parameters(proplists:get_value(parameters, Config, [])),
        DefaultParams
    ),
    % Inject configuration parameters into common test cases configuration.
    NewCaseArgs = inject_parameters(CaseArgs, ConfigParams),
    ConfigParamsToJSON = ConfigParams ++ get_stress_test_params(SuiteName),

    ConfigReps = case is_stress_test() of
                     true ->
                         Time = case os:getenv(?STRESS_TIME_ENV_VARIABLE) of
                                    false -> ?STRESS_DEFAULT_TIME;
                                    V -> list_to_integer(V)
                                end,
                         ets:insert(?STRESS_ETS_NAME, {timeout, Time}),
                         {timeout, Time};
                     _ ->
                         proplists:get_value(repeats, Config, DefaultReps)
                 end,

    {RepeatsDone, RepsSummary0, RepsDetails0, FailedReps0} =
        exec_test_repeats(SuiteName, CaseName, ConfigName, NewCaseArgs, ConfigReps),

    {RepsSummary, RepsDetails, FailedReps, SuccessfulReps, RepsAverage} =
        case is_stress_test() of
            true ->
                [{{cases, SuiteName},  Cases}] = ets:lookup(?STRESS_ETS_NAME, {cases, SuiteName}),
                ToMap = fun(InputList) ->
                    ZippedList = lists:zip(InputList, Cases),
                    lists:foldl(fun({Element, Case}, Acc) ->
                        maps:put(atom_to_binary(Case, utf8), Element, Acc)
                    end, #{}, ZippedList)
                end,

%%                 FRs = maps:fold(fun(Case, FR, Acc) ->
%%                     maps:fold(fun(K, V, Acc2) ->
%%                         CaseBin = atom_to_binary(Case, utf8),
%%                         K2 = <<CaseBin/binary, <<"_">>/binary, K/binary>>,
%%                         maps:put(K2, V, Acc2)
%%                     end, Acc, FR)
%%                 end, #{}, ToMap(FailedReps0)),

                SRs = lists:map(fun(FR) ->
                    RepeatsDone - maps:size(FR)
                end, FailedReps0),

                RAs = lists:map(fun({RS, SR}) ->
                    lists:map(fun(#parameter{value = Value} = Param) ->
                        Param#parameter{value = Value / SR}
                    end, RS)
                end, lists:zip(RepsSummary0, SRs)),

                {ToMap(RepsSummary0), ToMap(RepsDetails0), ToMap(FailedReps0), ToMap(SRs), ToMap(RAs)};
            _ ->
                SuccessfulReps0 = RepeatsDone - maps:size(FailedReps0),
                RepsAverage0 = lists:map(fun(#parameter{value = Value} = Param) ->
                    Param#parameter{value = Value / SuccessfulReps0}
                end, RepsSummary0),
                {RepsSummary0, RepsDetails0, FailedReps0, SuccessfulReps0, RepsAverage0}
        end,

    % Fetch git repository metadata.
    Repository = list_to_binary(proplists:get_value(git_repository, CaseArgs)),
    BranchBeg = proplists:get_value(git_branch, CaseArgs),
    Branch = case {os:getenv(?STRESS_ENV_VARIABLE), os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE)} of
                 {"true", _} ->
                     list_to_binary(BranchBeg ++ "/" ++ ?STRESS_ENV_VARIABLE);
                 {_, "true"} ->
                     list_to_binary(BranchBeg ++ "/" ++ ?STRESS_NO_CLEARING_ENV_VARIABLE);
                 _ ->
                     list_to_binary(BranchBeg)
             end,
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
        <<"completed">> => global_clock:timestamp_millis(),
        <<"parameters">> => format_parameters(ConfigParamsToJSON),
        <<"description">> => list_to_binary(ConfigDescr),
        <<"repeats_number">> => RepeatsDone,
        <<"successful_repeats">> => format_parameters(SuccessfulReps),
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

    % Check whether performance/stress configuration execution has been successfully
    % completed.
    SuccessRate = proplists:get_value(success_rate, Config, DefaultSuccessRate),
    case is_stress_test() of
        true ->
            StressStatus = maps:fold(fun(Case, SReps, Acc) ->
                CaseRate = 100.0 * SReps / RepeatsDone,
                ct:print("Done CASE: ~p~nOk percent: ~p, Required ok percent ~p",
                    [Case, CaseRate, SuccessRate]),
                case CaseRate >= SuccessRate of
                    true -> Acc;
                    _ -> error
                end
            end, ok, SuccessfulReps),
            ct:print("Stress test: SUITE: ~p~nCASE: ~p~nCONFIG: ~p~nStatus: ~p",
                [SuiteName, CaseName, ConfigName, StressStatus]),
            StressStatus;
        _ ->
            CaseRate = 100.0 * SuccessfulReps / RepeatsDone,
            ct:print("Done:~nSUITE: ~p~nCASE: ~p~nCONFIG: ~p~nOk percent: ~p, Required ok percent ~p",
                [SuiteName, CaseName, ConfigName, CaseRate, SuccessRate]),
            case CaseRate >= SuccessRate of
                true -> ok;
                _ -> error
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes test case multiple times.
%% @end
%%--------------------------------------------------------------------
-spec exec_test_repeats(SuiteName :: atom(), CaseName :: atom(), ConfigName :: atom(),
    CaseConfig :: proplist(), Reps :: integer() | {test_time, integer()}) -> {RepsDone :: integer(),
    RepsSummary :: [#parameter{}] | [[#parameter{}]], RepsDetails :: [#parameter{}] | [[#parameter{}]],
    FailedReps :: map() | [map()]}.
exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, {timeout, TimeLimit}) ->
    [{{cases, SuiteName},  Cases}] = ets:lookup(?STRESS_ETS_NAME, {cases, SuiteName}),
    exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, 1, {timeout, stopwatch:start(), TimeLimit},
        lists:map(fun(_) -> [] end, Cases), lists:map(fun(_) -> [] end, Cases),
        lists:map(fun(_) -> #{} end, Cases));
exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Reps) ->
    exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, 1, Reps + 1, [], [], #{}).
exec_test_repeats(_SuiteName, _CaseName, _ConfigName, _CaseConfig, Reps, Reps,
    RepsSummary, RepsDetails, FailedReps) ->
    {Reps - 1, RepsSummary, RepsDetails, FailedReps};
exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep, Reps,
    RepsSummary, RepsDetails, FailedReps) ->
    TimeStop = case Reps of
                   {timeout, Stopwatch, TimeLimit} ->
                       TestTime = stopwatch:read_seconds(Stopwatch),
                       TimeLeft = TimeLimit - TestTime,

                       [{{cases, SuiteName},  Cases}] = ets:lookup(?STRESS_ETS_NAME, {cases, SuiteName}),
                       ZippedFR = lists:zip(FailedReps, Cases),
                       ErrorsHistoryOK = lists:foldl(fun({FR, Case}, Acc) ->
                           CheckAns = check_error(Rep, FR),
                           ets:insert(?STRESS_ETS_NAME, {{history, SuiteName, Case}, {Rep, maps:size(FR), CheckAns}}),
                           case Acc of
                               permanent_error ->
                                   CheckAns;
                               _ ->
                                   Acc
                           end
                       end, permanent_error, ZippedFR),

                       case (TimeLeft > 0) and (ErrorsHistoryOK =/= permanent_error) of
                           true ->
                               ct:print("SUITE: ~p~nCASE: ~p~nCONFIG: ~p~nREPEAT: ~p, TEST TIME ~p sek, TIME LEFT ~p sek",
                                   [SuiteName, CaseName, ConfigName, Rep, TestTime, TimeLeft]),
                               ok;
                           _ ->
                               stop
                       end;
                   _ ->
                       ct:print("SUITE: ~p~nCASE: ~p~nCONFIG: ~p~nREPEAT: ~p / ~p (~p%)",
                           [SuiteName, CaseName, ConfigName, Rep, Reps - 1, (100 * Rep div (Reps - 1))]),
                       ok
               end,
    case TimeStop of
        stop ->
            {Rep - 1, RepsSummary, RepsDetails, FailedReps};
        _ ->
            case exec_test_repeat(SuiteName, CaseName, CaseConfig) of
                List0 when is_list(List0) ->
                    {List, Proceed} = case List0 of
                        [{ok, L2}] ->
                            case lists:member(stop, L2) of
                                true ->
                                    {[{ok, L2 -- [stop]}], false};
                                _ ->
                                    {List0, true}
                            end;
                        _ ->
                            {List0, true}
                    end,
                    List2 = lists:zip(List, lists:zip3(RepsSummary, RepsDetails, FailedReps)),
                    {R1, R2, R3} = lists:foldl(fun({R, {RS, RD, FR}}, {A1, A2, A3}) ->
                        {NewRepsSummary, NewRepsDetails, NewFailedReps} = proccess_repeat_result(R, Rep, RS, RD, FR),
                        {[NewRepsSummary | A1], [NewRepsDetails | A2], [NewFailedReps | A3]}
                    end, {[], [], []}, List2),

                    case Proceed of
                        true ->
                            exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
                                Reps, lists:reverse(R1), lists:reverse(R2), lists:reverse(R3));
                        _ ->
                            {Rep, lists:reverse(R1), lists:reverse(R2), lists:reverse(R3)}
                    end;
                RepeatResult ->
                    {NewRepsSummary, NewRepsDetails, NewFailedReps} =
                        proccess_repeat_result(RepeatResult, Rep, RepsSummary, RepsDetails, FailedReps),
                    exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
                        Reps, NewRepsSummary, NewRepsDetails, NewFailedReps)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if stress test should be stopped because of error.
%% @end
%%--------------------------------------------------------------------
-spec check_error(Rep :: integer(), FailedReps :: map()) -> integer() | permanent_error.
check_error(Rep, FailedReps) ->
    check_error(Rep - 1, Rep, FailedReps).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if stress test should be stopped because of error.
%% @end
%%--------------------------------------------------------------------
-spec check_error(Rep :: integer(), Start :: integer(), FailedReps :: map()) -> integer() | permanent_error.
check_error(Check, Start, _FailedReps) when Start - ?STRESS_ERRORS_TO_STOP > Check ->
    permanent_error;
check_error(0, Start, _FailedReps) ->
    Start - 1;
check_error(Check, Start, FailedReps) ->
    case maps:is_key(integer_to_binary(Check), FailedReps) of
        false ->
            Start - Check - 1;
        _ ->
            check_error(Check - 1, Start, FailedReps)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Processes test's repeat result.
%% @end
%%--------------------------------------------------------------------
-spec proccess_repeat_result(RepeatResult :: {ok, [#parameter{}]} | {error, Reason :: binary()},
    Rep :: integer(), RepsSummary :: [#parameter{}], RepsDetails :: [#parameter{}], FailedReps :: map()) ->
    {NewRepsSummary :: [#parameter{}], NewRepsDetails :: [#parameter{}], NewFailedReps :: map()}.
proccess_repeat_result(RepeatResult, Rep, RepsSummary, RepsDetails, FailedReps) ->
    BinRep = integer_to_binary(Rep),
    case RepeatResult of
        {ok, Params} ->
            case RepsSummary of
                [] ->
                    % Initialize list of parameters for test case repeats.
                    NewRepsDetails = lists:map(fun
                        (#parameter{value = Value} = Param) ->
                            Param#parameter{value = maps:put(BinRep, Value, #{})}
                    end, Params),
                    {Params, NewRepsDetails, FailedReps};
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
                    {NewRepsSummary, NewRepsDetails, FailedReps}
            end;
        {error, Reason} ->
            NewFailedReps = maps:put(BinRep, Reason, FailedReps),
            {RepsSummary, RepsDetails, NewFailedReps}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes test case once.
%% @end
%%--------------------------------------------------------------------
-spec exec_test_repeat(SuiteName :: atom(), CaseName :: atom(), CaseConfig :: proplist()) ->
    {ok, [#parameter{}]} | {error, Reason :: binary()} | list().
exec_test_repeat(SuiteName, CaseName, CaseConfig) ->
    try
        Stopwatch = stopwatch:start(),
        Result = apply(SuiteName, base_case(CaseName), [CaseConfig]),
        TestTime = stopwatch:read_millis(Stopwatch),
        % Return list of parameters consisting of default 'test_time' parameter
        % and parameters returned from test case.
        case is_stress_test() and (CaseName =:= stress_test) of
            true ->
                Result;
            _ ->
                {ok, [#parameter{
                    name = test_time,
                    description = "Test execution time.",
                    value = TestTime,
                    unit = "ms"} | lists:filter(fun
                    (#parameter{}) -> true;
                    (stop) -> true;
                    (_) -> false
                end, lists:flatten([Result]))]}
        end
    catch
        Error:Reason:Stacktrace ->
            Message = str_utils:format("~p:~p~n~p", [Error, Reason, Stacktrace]),
            ct:print("~p:~p failed due to: ~w:~w~n"
            "Stacktrace: ~p", [SuiteName, CaseName, Error, Reason, Stacktrace]),
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
-spec format_parameters(Params :: term()) -> term().
format_parameters(Params) when is_list(Params)->
    lists:map(fun
        (#parameter{name = Name, value = Value, unit = Unit, description = Descr}) ->
            #{
                <<"name">> => Name,
                <<"value">> => maybe_round(Value),
                <<"unit">> => list_to_binary(Unit),
                <<"description">> => list_to_binary(Descr)
            }
    end, Params);
format_parameters(Params) when is_map (Params)->
    maps:map(fun(_, V) ->
        format_parameters(V)
    end, Params);
format_parameters(Params) ->
    Params.

%%--------------------------------------------------------------------
%% @doc
%% If value is numeric returns number rounded to three decimal digits, otherwise
%% returns unmodified value.
%% @end
%%--------------------------------------------------------------------
-spec maybe_round(Value :: term()) -> NewValue :: term().
maybe_round(Value) when is_number(Value) ->
    case Value == round(Value) of
        true -> Value;
        false -> round(Value * 1000) / 1000
    end;
maybe_round(Value) ->
    Value.

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
    hd(lists:filtermap(fun(#xmlElement{name = copyright, content = [Copyright]}) ->
        {true, list_to_binary(Copyright#xmlText.value)};
        (_) -> false
    end, Doc#xmlElement.content)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns base function name of a testcase as atom.
%% @end
%%--------------------------------------------------------------------
-spec base_case(CaseName :: atom()) -> atom().
base_case(CaseName) ->
    list_to_atom(atom_to_list(CaseName) ++ ?BASE_SUFFIX).
