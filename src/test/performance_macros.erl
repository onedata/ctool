%%%--------------------------------------------------------------------
%%% @author Michal Wrzeszcz
%%% @author Krzysztof Trzepla
%%% @author Jakub Kudzia
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This file contains definitions of annotations used during
%%% performance tests.
%%% @end
%%%--------------------------------------------------------------------
-module(performance_macros).

-author("Michal Wrzeszcz").
-author("Krzysztof Trzepla").
-author("Jakub Kudzia").

% this file is built by parent project so include_lib must be used
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("annotations/include/types.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("common_test/include/ct.hrl").

-export([%around_advice/4,
  is_standard_test/0, is_stress_test/0, stress_test/1,
  should_clear/1, inject_parameters/2, get_config_params/1, %run_annotated/4,
  run_testcase/5, set_up_stress_test/3, run_stress_test/4, run_test/5]).

-type proplist() :: [{Key :: atom(), Value :: term()}].

%%-define(BRANCH_ENV_VARIABLE, "branch").
%%-define(PERFORMANCE_ENV_VARIABLE, "performance").
%%-define(PERFORMANCE_RESULT_FILE, "performance.json").
%%-define(STRESS_ENV_VARIABLE, "stress").
%%-define(STRESS_NO_CLEARING_ENV_VARIABLE, "stress_no_clearing").
%%-define(STRESS_TIME_ENV_VARIABLE, "stress_time").
%%-define(STRESS_DEFAULT_TIME, timer:hours(3) div 1000).
%%-define(STRESS_ERRORS_TO_STOP, 100).
%%-define(STRESS_ETS_NAME, stress_ets).
%%-define(STRESS_TIMEOUT_EXTENSION_SECONDS, 600). % extension of ct timeout to let running tests end

%%%===================================================================
%%% API
%%%===================================================================

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
%% TODO add doc
%% @end
%%--------------------------------------------------------------------
run_test(_SuiteName, _CaseName, [get_params], Data, _TestFun) ->
  get_config_params(Data);
run_test(SuiteName, CaseName, CaseArgs, Data, TestFun) ->
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
      exec_test_repeat(TestFun, SuiteName, CaseName, Configs);
    _ ->
      run_testcase(SuiteName, CaseName, CaseArgs, Data, TestFun)
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO add doc
%% @end
%%--------------------------------------------------------------------
%% TODO update spec
-spec run_testcase(SuiteName :: list(), CaseName :: list(), CaseArgs :: list(),
    Data :: list(), TestFun :: function()) -> term().
run_testcase(SuiteName, CaseName, CaseArgs, Data, TestFun) ->
  DefaultReps = proplists:get_value(repeats, Data, 1),
  DefaultSuccessRate = proplists:get_value(success_rate, Data, 100),
  DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
  case is_standard_test() of
    false ->
      CaseDescr = proplists:get_value(description, Data, ""),
      Configs = proplists:get_all_values(config, Data),
      exec_perf_configs(TestFun, CaseDescr, SuiteName, CaseName, CaseArgs,
        Configs, DefaultReps, DefaultSuccessRate, DefaultParams);
    _ ->
      exec_ct_config(TestFun, CaseArgs, DefaultParams)
  end.

%%--------------------------------------------------------------------
%% @doc
%%TODO add doc
%% @end
%%--------------------------------------------------------------------
%%TODO add spec
set_up_stress_test(SuiteName, CasesNames, NoClearingCases) ->
  case {os:getenv(?STRESS_ENV_VARIABLE), os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE)} of
    {"true", _} ->
      save_suite_and_cases(SuiteName, CasesNames),
      [stress_test];
    {_, "true"} ->
      save_suite_and_cases(SuiteName, NoClearingCases),
      [stress_test]
  end.

%%--------------------------------------------------------------------
%% @doc
%%TODO add doc
%% @end
%%--------------------------------------------------------------------
%%TODO add spec
run_stress_test(SuiteName, CaseArgs, Data, TestFun) ->

  CaseDescr = proplists:get_value(description, Data, ""),
  Configs = proplists:get_all_values(config, Data),
  DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
  DefaultSuccessRate = proplists:get_value(success_rate, Data, 100),
  Ans = exec_perf_configs(TestFun, CaseDescr, SuiteName, stress_test, CaseArgs,
    Configs, 1, DefaultSuccessRate, DefaultParams),
  EtsOwner = ets:info(?STRESS_ETS_NAME, owner),
  ets:delete(?STRESS_ETS_NAME),
  EtsOwner ! kill_ets_owner,
  Ans.


%%TODO DELETE
%%%%--------------------------------------------------------------------
%%%% @doc
%%%% Function executed instead of annotated function. May start
%%%% annotated function inside.
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec around_advice(Annotation :: #annotation{}, Module :: atom(),
%%    Function :: atom(), Args :: [term()]) -> Result :: [atom()] | term().
%%around_advice(#annotation{data = {test_cases, CasesNames}}, SuiteName, all, []) ->
%%  case os:getenv(?PERFORMANCE_ENV_VARIABLE) of
%%    "true" -> CasesNames;
%%    _ ->
%%      case is_stress_test() of
%%        true ->
%%          [];
%%        _ ->
%%          call_testcase(SuiteName, all, [])
%%      end
%%  end;
%%around_advice(#annotation{data = CasesNames}, SuiteName, all, []) ->
%%  case {os:getenv(?STRESS_ENV_VARIABLE), os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE)} of
%%    {"true", _} ->
%%      save_suite_and_cases(SuiteName, proplists:get_value(stress, CasesNames, [])),
%%      [stress_test];
%%    {_, "true"} ->
%%      save_suite_and_cases(SuiteName, proplists:get_value(stress_no_clearing, CasesNames, [])),
%%      [stress_test];
%%    _ ->
%%        call_testcase(SuiteName, all, [])
%%  end;
%%around_advice(#annotation{data = Data}, SuiteName, stress_test, [CaseArgs]) ->
%%  case is_stress_test() of
%%    true ->
%%      CaseDescr = proplists:get_value(description, Data, ""),
%%      Configs = proplists:get_all_values(config, Data),
%%      DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
%%      DefaultSuccessRate = proplists:get_value(success_rate, Data, 100),
%%      Ans = exec_perf_configs(SuiteName, stress_test, CaseDescr, CaseArgs,
%%        Configs, 1, DefaultSuccessRate, DefaultParams),
%%      EtsOwner = ets:info(?STRESS_ETS_NAME, owner),
%%      ets:delete(?STRESS_ETS_NAME),
%%      EtsOwner ! kill_ets_owner,
%%      Ans;
%%    _ ->
%%      run_annotated(Data, SuiteName, stress_test, CaseArgs)
%%  end;
%%around_advice(#annotation{data = Data}, _SuiteName, _CaseName, [get_params]) ->
%%  get_config_params(Data);
%%around_advice(#annotation{data = Data}, SuiteName, CaseName, [CaseArgs]) ->
%%  case is_stress_test() of
%%    true ->
%%      ConfigParams = get_config_params(Data),
%%      NewCaseArgs = inject_parameters(CaseArgs, ConfigParams),
%%      Configs = case os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE) of
%%                  "true" ->
%%                    [{clearing, false} | NewCaseArgs];
%%                  _ ->
%%                    NewCaseArgs
%%                end,
%%      exec_test_repeat(SuiteName, CaseName, Configs);
%%    _ ->
%%      run_annotated(Data, SuiteName, CaseName, CaseArgs)
%%  end.

%%--------------------------------------------------------------------
%% @doc
%% Basic function for stress test.
%% @end
%%--------------------------------------------------------------------
-spec stress_test(Config :: list()) -> list() | no_return().
stress_test(Config) ->
  [{suite, Suite}] = ets:lookup(?STRESS_ETS_NAME, suite),
  [{cases, Cases}] = ets:lookup(?STRESS_ETS_NAME, cases),
  [{timeout, Timeout}] = ets:lookup(?STRESS_ETS_NAME, timeout),
  ct:timetrap({seconds, Timeout + ?STRESS_TIMEOUT_EXTENSION_SECONDS}), % add 10 minutes to let running tests end

  AnsList = lists:foldl(fun(Case, Ans) ->
    NewConfig = try
                  apply(Suite, init_per_testcase, [Case, Config])
                catch
                  error:undef ->
                    Config;
                  E1:E2 ->
                    ct:print("Case: ~p, init_per_testcase error: ~p:~p", [Case, E1, E2]),
                    Config
                end,
    CaseAns = case apply(Suite, Case, [NewConfig]) of
                {ok, TmpAns} ->
%%                           TmpAns2 = lists:map(fun(Param) ->
%%                               PName = Param#parameter.name,
%%                               Param#parameter{name = concat_atoms(Case, PName)}
%%                           end, TmpAns),
%%                           [{ok, TmpAns2} | Ans];
                  [{ok, TmpAns} | Ans];
                {error, E} ->
                  Message = str_utils:format("Case: ~p, error: ~p", [Case, E]),
                  [{error, Message} | Ans]
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
                        end, [], Cases),
  lists:reverse(AnsList).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of test's parameters or empty list if test is
%% standard or performance.
%% @end
%%--------------------------------------------------------------------
-spec get_stress_test_params() -> list().
get_stress_test_params() ->
  case is_stress_test() of
    true ->
      [{suite, Suite}] = ets:lookup(?STRESS_ETS_NAME, suite),
      [{cases, Cases}] = ets:lookup(?STRESS_ETS_NAME, cases),

      lists:foldl(fun(Case, Ans) ->
        Params =  apply(Suite, Case, [get_params]),
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


%% TODO delete
%%%%--------------------------------------------------------------------
%%%% @private
%%%% @doc
%%%% Runs annotated function with apropriate parameters.
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec run_annotated(Data :: list(), SuiteName :: atom(), CaseName :: atom(),
%%    CaseArgs :: list()) -> term().
%%run_annotated(Data, SuiteName, CaseName, CaseArgs) ->
%%  DefaultReps = proplists:get_value(repeats, Data, 1),
%%  DefaultSuccessRate = proplists:get_value(success_rate, Data, 100),
%%  DefaultParams = parse_parameters(proplists:get_value(parameters, Data, [])),
%%  case is_standard_test() of
%%    false ->
%%      CaseDescr = proplists:get_value(description, Data, ""),
%%      Configs = proplists:get_all_values(config, Data),
%%      exec_perf_configs(SuiteName, CaseName, CaseDescr, CaseArgs,
%%        Configs, DefaultReps, DefaultSuccessRate, DefaultParams);
%%    _ ->
%%      exec_ct_config(SuiteName, CaseName, CaseArgs, DefaultParams)
%%  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Saves information about tests to be done during stress test.
%% @end
%%--------------------------------------------------------------------
-spec save_suite_and_cases(Suite :: atom(), Cases :: list()) -> ok.
save_suite_and_cases(Suite, Cases) ->
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
      ets:insert(?STRESS_ETS_NAME, {suite, Suite}),
      ets:insert(?STRESS_ETS_NAME, {cases, Cases})
  end,
  ok.

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
-spec exec_ct_config(TestFun :: function(), CaseArgs :: proplist(), Params :: [#parameter{}]) -> term().
exec_ct_config(TestFun, CaseArgs, Params) ->
  NewCaseArgs = inject_parameters(CaseArgs, Params),
  TestFun(NewCaseArgs).

%%TODO delete below function
%%%%--------------------------------------------------------------------
%%%% @private
%%%% @doc
%%%% Executes common test using non-performance configurations.
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec exec_ct_config(SuiteName :: atom(), CaseName :: atom(),
%%    CaseArgs :: proplist(), Params :: [#parameter{}]) -> term().
%%exec_ct_config(SuiteName, CaseName, CaseArgs, Params) ->
%%  NewCaseArgs = inject_parameters(CaseArgs, Params),
%%  call_testcase(SuiteName, CaseName, [NewCaseArgs]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes common test case using performance configurations.
%% @end
%%--------------------------------------------------------------------
-spec exec_perf_configs(TestFun :: function(), CaseDescr :: string(),
    SuiteName :: list(), CaseName :: list(), CaseArgs :: proplist(),
    Config :: proplist(), DefaultReps :: non_neg_integer(),
    DefaultSuccessRate :: number(), DefaultParams :: [#parameter{}]) -> ok.
exec_perf_configs(TestFun, CaseDescr, SuiteName, CaseName, CaseArgs, Configs,
    DefaultReps, DefaultSuccessRate, DefaultParams) ->
  ?assertEqual(ok, lists:foldl(
    fun(Config, Status) ->
      case exec_perf_config(TestFun, CaseDescr, SuiteName, CaseName, CaseArgs,
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
-spec exec_perf_config(TestFun :: function(), CaseDescr :: string(),
    SuiteName :: list(), CaseName :: list(), CaseArgs :: proplist(),
    Config :: proplist(), DefaultReps :: non_neg_integer(),
    DefaultSuccessRate :: number(), DefaultParams :: [#parameter{}]) -> ok | error.
exec_perf_config(TestFun, CaseDescr, SuiteName, CaseName, CaseArgs, Config,
    DefaultReps, DefaultSuccessRate, DefaultParams) ->

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
  ConfigParamsToJSON = ConfigParams ++ get_stress_test_params(),

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
    exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, NewCaseArgs, ConfigReps),

  {RepsSummary, RepsDetails, FailedReps, SuccessfulReps, RepsAverage} =
    case is_stress_test() of
      true ->
        [{cases, Cases}] = ets:lookup(?STRESS_ETS_NAME, cases),
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
    <<"completed">> => get_timestamp(),
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



%%TODO delete
%%%%--------------------------------------------------------------------
%%%% @private
%%%% @doc
%%%% Executes common test case using performance configurations.
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec exec_perf_configs(SuiteName :: atom(), CaseName :: atom(),
%%    CaseDescr :: string(), CaseArgs :: proplist(), Config :: proplist(),
%%    DefaultReps :: non_neg_integer(), DefaultSuccessRate :: number(), DefaultParams :: [#parameter{}]) -> ok.
%%exec_perf_configs(SuiteName, CaseName, CaseDescr, CaseArgs, Configs,
%%    DefaultReps, DefaultSuccessRate, DefaultParams) ->
%%  ?assertEqual(ok, lists:foldl(
%%        fun(Config, Status) ->
%%    case exec_perf_config(SuiteName, CaseName, CaseDescr, CaseArgs,
%%      Config, DefaultReps, DefaultSuccessRate, DefaultParams) of
%%      ok -> Status;
%%      _ -> error
%%    end
%%                               end, ok, Configs)).
%%TODO delete
%%%%--------------------------------------------------------------------
%%%% @private
%%%% @doc
%%%% Executes common test case using performance configuration.
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec exec_perf_config(SuiteName :: atom(), CaseName :: atom(),
%%    CaseDescr :: string(), CaseArgs :: proplist(), Config :: proplist(),
%%    DefaultReps :: non_neg_integer(), DefaultSuccessRate :: number(), DefaultParams :: [#parameter{}]) ->
%%  ok | error.
%%exec_perf_config(SuiteName, CaseName, CaseDescr, CaseArgs, Config,
%%    DefaultReps, DefaultSuccessRate, DefaultParams) ->
%%
%%  % Fetch and prepare test case configuration.
%%  TestRoot = proplists:get_value(ct_test_root, CaseArgs),
%%  ConfigName = proplists:get_value(name, Config),
%%  ConfigDescr = proplists:get_value(description, Config, ""),
%%  % Merge specific configuration test case parameters with default test case
%%  % parameters, so that specific values overrider default ones.
%%  ConfigParams = merge_parameters(
%%    parse_parameters(proplists:get_value(parameters, Config, [])),
%%    DefaultParams
%%  ),
%%  % Inject configuration parameters into common test cases configuration.
%%  NewCaseArgs = inject_parameters(CaseArgs, ConfigParams),
%%  ConfigParamsToJSON = ConfigParams ++ get_stress_test_params(),
%%
%%  ConfigReps = case is_stress_test() of
%%                 true ->
%%                   Time = case os:getenv(?STRESS_TIME_ENV_VARIABLE) of
%%                            false -> ?STRESS_DEFAULT_TIME;
%%                            V -> list_to_integer(V)
%%                          end,
%%                   ets:insert(?STRESS_ETS_NAME, {timeout, Time}),
%%                   {timeout, Time};
%%                 _ ->
%%                   proplists:get_value(repeats, Config, DefaultReps)
%%               end,
%%
%%  {RepeatsDone, RepsSummary0, RepsDetails0, FailedReps0} =
%%    exec_test_repeats(SuiteName, CaseName, ConfigName, NewCaseArgs, ConfigReps),
%%
%%  {RepsSummary, RepsDetails, FailedReps, SuccessfulReps, RepsAverage} =
%%    case is_stress_test() of
%%      true ->
%%        [{cases, Cases}] = ets:lookup(?STRESS_ETS_NAME, cases),
%%        ToMap = fun(InputList) ->
%%          ZippedList = lists:zip(InputList, Cases),
%%          lists:foldl(fun({Element, Case}, Acc) ->
%%            maps:put(atom_to_binary(Case, utf8), Element, Acc)
%%                      end, #{}, ZippedList)
%%                end,
%%
%%%%                 FRs = maps:fold(fun(Case, FR, Acc) ->
%%%%                     maps:fold(fun(K, V, Acc2) ->
%%%%                         CaseBin = atom_to_binary(Case, utf8),
%%%%                         K2 = <<CaseBin/binary, <<"_">>/binary, K/binary>>,
%%%%                         maps:put(K2, V, Acc2)
%%%%                     end, Acc, FR)
%%%%                 end, #{}, ToMap(FailedReps0)),
%%
%%        SRs = lists:map(fun(FR) ->
%%          RepeatsDone - maps:size(FR)
%%                        end, FailedReps0),
%%
%%        RAs = lists:map(fun({RS, SR}) ->
%%          lists:map(fun(#parameter{value = Value} = Param) ->
%%            Param#parameter{value = Value / SR}
%%                    end, RS)
%%                        end, lists:zip(RepsSummary0, SRs)),
%%
%%        {ToMap(RepsSummary0), ToMap(RepsDetails0), ToMap(FailedReps0), ToMap(SRs), ToMap(RAs)};
%%      _ ->
%%        SuccessfulReps0 = RepeatsDone - maps:size(FailedReps0),
%%        RepsAverage0 = lists:map(fun(#parameter{value = Value} = Param) ->
%%          Param#parameter{value = Value / SuccessfulReps0}
%%                                 end, RepsSummary0),
%%        {RepsSummary0, RepsDetails0, FailedReps0, SuccessfulReps0, RepsAverage0}
%%    end,
%%
%%  % Fetch git repository metadata.
%%  Repository = list_to_binary(proplists:get_value(git_repository, CaseArgs)),
%%  BranchBeg = proplists:get_value(git_branch, CaseArgs),
%%  Branch = case {os:getenv(?STRESS_ENV_VARIABLE), os:getenv(?STRESS_NO_CLEARING_ENV_VARIABLE)} of
%%             {"true", _} ->
%%               list_to_binary(BranchBeg ++ "/" ++ ?STRESS_ENV_VARIABLE);
%%             {_, "true"} ->
%%               list_to_binary(BranchBeg ++ "/" ++ ?STRESS_NO_CLEARING_ENV_VARIABLE);
%%             _ ->
%%               list_to_binary(BranchBeg)
%%           end,
%%  Commit = list_to_binary(proplists:get_value(git_commit, CaseArgs)),
%%
%%  #{<<"performance">> := PerfResults} =
%%    case file:read_file(?PERFORMANCE_RESULT_FILE) of
%%      {ok, Json} ->
%%        jiffy:decode(Json, [return_maps]);
%%      _ ->
%%        #{
%%          <<"performance">> => #{
%%            <<"repository">> => Repository,
%%            <<"branch">> => Branch,
%%            <<"commit">> => Commit
%%          }
%%        }
%%    end,
%%
%%  % Create JSON description of performance configuration execution.
%%  BinSuiteName = atom_to_binary(SuiteName, utf8),
%%  BinCaseName = atom_to_binary(CaseName, utf8),
%%  BinConfigName = atom_to_binary(ConfigName, utf8),
%%
%%  SuitesMap = maps:get(<<"suites">>, PerfResults, #{}),
%%  SuiteMap = maps:get(BinSuiteName, SuitesMap, #{
%%    <<"name">> => BinSuiteName,
%%    <<"copyright">> => get_copyright(TestRoot, SuiteName),
%%    <<"authors">> => get_authors(TestRoot, SuiteName),
%%    <<"description">> => get_description(TestRoot, SuiteName)
%%  }),
%%  CasesMap = maps:get(<<"cases">>, SuiteMap, #{}),
%%  CaseMap = maps:get(BinCaseName, CasesMap, #{
%%    <<"name">> => BinCaseName,
%%    <<"description">> => list_to_binary(CaseDescr)
%%  }),
%%  ConfigsMap = maps:get(<<"configs">>, CaseMap, #{}),
%%  ConfigMap = #{
%%    <<"name">> => BinConfigName,
%%    <<"completed">> => get_timestamp(),
%%    <<"parameters">> => format_parameters(ConfigParamsToJSON),
%%    <<"description">> => list_to_binary(ConfigDescr),
%%    <<"repeats_number">> => RepeatsDone,
%%    <<"successful_repeats">> => format_parameters(SuccessfulReps),
%%    <<"successful_repeats_summary">> => format_parameters(RepsSummary),
%%    <<"successful_repeats_average">> => format_parameters(RepsAverage),
%%    <<"successful_repeats_details">> => format_parameters(RepsDetails),
%%    <<"failed_repeats_details">> => FailedReps
%%  },
%%
%%  NewPerfResults = PerfResults#{
%%    <<"suites">> =>maps:put(BinSuiteName, SuiteMap#{
%%      <<"cases">> => maps:put(BinCaseName, CaseMap#{
%%        <<"configs">> => maps:put(BinConfigName, ConfigMap, ConfigsMap)
%%      }, CasesMap)
%%    }, SuitesMap)
%%  },
%%
%%  NewJson = jiffy:encode(#{<<"performance">> => NewPerfResults}, [pretty]),
%%  file:write_file(?PERFORMANCE_RESULT_FILE, NewJson),
%%
%%  % Check whether performance/stress configuration execution has been successfully
%%  % completed.
%%  SuccessRate = proplists:get_value(success_rate, Config, DefaultSuccessRate),
%%  case is_stress_test() of
%%    true ->
%%      StressStatus = maps:fold(fun(Case, SReps, Acc) ->
%%        CaseRate = 100.0 * SReps / RepeatsDone,
%%        ct:print("Done CASE: ~p~nOk percent: ~p, Required ok percent ~p",
%%          [Case, CaseRate, SuccessRate]),
%%        case CaseRate >= SuccessRate of
%%          true -> Acc;
%%          _ -> error
%%        end
%%                               end, ok, SuccessfulReps),
%%      ct:print("Stress test: SUITE: ~p~nCASE: ~p~nCONFIG: ~p~nStatus: ~p",
%%        [SuiteName, CaseName, ConfigName, StressStatus]),
%%      StressStatus;
%%    _ ->
%%      CaseRate = 100.0 * SuccessfulReps / RepeatsDone,
%%      ct:print("Done:~nSUITE: ~p~nCASE: ~p~nCONFIG: ~p~nOk percent: ~p, Required ok percent ~p",
%%        [SuiteName, CaseName, ConfigName, CaseRate, SuccessRate]),
%%      case CaseRate >= SuccessRate of
%%        true -> ok;
%%        _ -> error
%%      end
%%  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes test case multiple times.
%% @end
%%--------------------------------------------------------------------
-spec exec_test_repeats(TestFun :: function(), SuiteName :: atom(), CaseName :: atom(), ConfigName :: atom(),
    CaseConfig :: proplist(), Reps :: integer() | {test_time, integer()}) -> {RepsDone :: integer(),
  RepsSummary :: [#parameter{}] | [[#parameter{}]], RepsDetails :: [#parameter{}] | [[#parameter{}]],
  FailedReps :: map() | [map()]}.
exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, CaseConfig, {timeout, TimeLimit}) ->
  [{cases, Cases}] = ets:lookup(?STRESS_ETS_NAME, cases),
  exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, CaseConfig, 1, {timeout, os:timestamp(), TimeLimit},
    lists:map(fun(_) -> [] end, Cases), lists:map(fun(_) -> [] end, Cases),
    lists:map(fun(_) -> #{} end, Cases));
exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, CaseConfig, Reps) ->
  exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, CaseConfig, 1, Reps + 1, [], [], #{}).
exec_test_repeats(_TestFun, _SuiteName, _CaseName, _ConfigName, _CaseConfig, Reps, Reps,
    RepsSummary, RepsDetails, FailedReps) ->
  {Reps - 1, RepsSummary, RepsDetails, FailedReps};
exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, CaseConfig, Rep, Reps,
    RepsSummary, RepsDetails, FailedReps) ->
  TimeStop = case Reps of
               {timeout, StartTime, TimeLimit} ->
                 Now = os:timestamp(),
                 TestTime = timer:now_diff(Now, StartTime) div 1000000,
                 TimeLeft = TimeLimit - TestTime,

                 ErrorsHistoryOK = lists:foldl(fun(FR, Acc) ->
                   case Acc of
                     error ->
                       error;
                     _ ->
                       check_error(Rep, FR)
                   end
                                               end, ok, FailedReps),

                 case (TimeLeft > 0) and (ErrorsHistoryOK =:= ok) of
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
      case exec_test_repeat(TestFun, SuiteName, CaseName, CaseConfig) of
        List when is_list(List) ->
          List2 = lists:zip(List, lists:zip3(RepsSummary, RepsDetails, FailedReps)),
          {R1, R2, R3} = lists:foldl(fun({R, {RS, RD, FR}}, {A1, A2, A3}) ->
            {NewRepsSummary, NewRepsDetails, NewFailedReps} = proccess_repeat_result(R, Rep, RS, RD, FR),
            {[NewRepsSummary | A1], [NewRepsDetails | A2], [NewFailedReps | A3]}
                                     end, {[], [], []}, List2),
          exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
            Reps, lists:reverse(R1), lists:reverse(R2), lists:reverse(R3));
        RepeatResult ->
          {NewRepsSummary, NewRepsDetails, NewFailedReps} =
            proccess_repeat_result(RepeatResult, Rep, RepsSummary, RepsDetails, FailedReps),
          exec_test_repeats(TestFun, SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
            Reps, NewRepsSummary, NewRepsDetails, NewFailedReps)
      end
  end.

%%TODO delete
%%%%--------------------------------------------------------------------
%%%% @private
%%%% @doc
%%%% Executes test case multiple times.
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec exec_test_repeats(SuiteName :: atom(), CaseName :: atom(), ConfigName :: atom(),
%%    CaseConfig :: proplist(), Reps :: integer() | {test_time, integer()}) -> {RepsDone :: integer(),
%%  RepsSummary :: [#parameter{}] | [[#parameter{}]], RepsDetails :: [#parameter{}] | [[#parameter{}]],
%%  FailedReps :: map() | [map()]}.
%%exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, {timeout, TimeLimit}) ->
%%  [{cases, Cases}] = ets:lookup(?STRESS_ETS_NAME, cases),
%%  exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, 1, {timeout, os:timestamp(), TimeLimit},
%%    lists:map(fun(_) -> [] end, Cases), lists:map(fun(_) -> [] end, Cases),
%%    lists:map(fun(_) -> #{} end, Cases));
%%exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Reps) ->
%%  exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, 1, Reps + 1, [], [], #{}).
%%exec_test_repeats(_SuiteName, _CaseName, _ConfigName, _CaseConfig, Reps, Reps,
%%    RepsSummary, RepsDetails, FailedReps) ->
%%  {Reps - 1, RepsSummary, RepsDetails, FailedReps};
%%exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep, Reps,
%%    RepsSummary, RepsDetails, FailedReps) ->
%%  TimeStop = case Reps of
%%               {timeout, StartTime, TimeLimit} ->
%%                 Now = os:timestamp(),
%%                 TestTime = timer:now_diff(Now, StartTime) div 1000000,
%%                 TimeLeft = TimeLimit - TestTime,
%%
%%                 ErrorsHistoryOK = lists:foldl(fun(FR, Acc) ->
%%                   case Acc of
%%                     error ->
%%                       error;
%%                     _ ->
%%                       check_error(Rep, FR)
%%                   end
%%                                               end, ok, FailedReps),
%%
%%                 case (TimeLeft > 0) and (ErrorsHistoryOK =:= ok) of
%%                   true ->
%%                     ct:print("SUITE: ~p~nCASE: ~p~nCONFIG: ~p~nREPEAT: ~p, TEST TIME ~p sek, TIME LEFT ~p sek",
%%                       [SuiteName, CaseName, ConfigName, Rep, TestTime, TimeLeft]),
%%                     ok;
%%                   _ ->
%%                     stop
%%                 end;
%%               _ ->
%%                 ct:print("SUITE: ~p~nCASE: ~p~nCONFIG: ~p~nREPEAT: ~p / ~p (~p%)",
%%                   [SuiteName, CaseName, ConfigName, Rep, Reps - 1, (100 * Rep div (Reps - 1))]),
%%                 ok
%%             end,
%%  case TimeStop of
%%    stop ->
%%      {Rep - 1, RepsSummary, RepsDetails, FailedReps};
%%    _ ->
%%      case exec_test_repeat(SuiteName, CaseName, CaseConfig) of
%%        List when is_list(List) ->
%%          List2 = lists:zip(List, lists:zip3(RepsSummary, RepsDetails, FailedReps)),
%%          {R1, R2, R3} = lists:foldl(fun({R, {RS, RD, FR}}, {A1, A2, A3}) ->
%%            {NewRepsSummary, NewRepsDetails, NewFailedReps} = proccess_repeat_result(R, Rep, RS, RD, FR),
%%            {[NewRepsSummary | A1], [NewRepsDetails | A2], [NewFailedReps | A3]}
%%                                     end, {[], [], []}, List2),
%%          exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
%%            Reps, lists:reverse(R1), lists:reverse(R2), lists:reverse(R3));
%%        RepeatResult ->
%%          {NewRepsSummary, NewRepsDetails, NewFailedReps} =
%%            proccess_repeat_result(RepeatResult, Rep, RepsSummary, RepsDetails, FailedReps),
%%          exec_test_repeats(SuiteName, CaseName, ConfigName, CaseConfig, Rep + 1,
%%            Reps, NewRepsSummary, NewRepsDetails, NewFailedReps)
%%      end
%%  end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if stress test should be stopped because of error.
%% @end
%%--------------------------------------------------------------------
-spec check_error(Rep :: integer(), FailedReps :: map()) -> ok | error.
check_error(Rep, _FailedReps) when Rep < ?STRESS_ERRORS_TO_STOP ->
  ok;
check_error(Rep, FailedReps) ->
  check_error(Rep - 1, Rep, FailedReps).

check_error(Check, Start, _FailedReps) when Start - ?STRESS_ERRORS_TO_STOP > Check ->
  error;
check_error(Check, Start, FailedReps) ->
  case maps:is_key(integer_to_binary(Check), FailedReps) of
    false ->
      ok;
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
-spec exec_test_repeat(TestFun :: function(), SuiteName :: atom(), CaseName :: atom(), CaseConfig :: proplist()) ->
  {ok, [#parameter{}]} | {error, Reason :: binary()} | list().
exec_test_repeat(TestFun, SuiteName, CaseName, CaseConfig) ->
  try
    Timestamp1 = os:timestamp(),
    Result = TestFun(CaseConfig),
    Timestamp2 = os:timestamp(),
    TestTime = utils:milliseconds_diff(Timestamp2, Timestamp1),
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
                                        (_) -> false
                                      end, lists:flatten([Result]))]}
    end
  catch
    Error:Reason ->
      Stacktrace = erlang:get_stacktrace(),
      Message = str_utils:format("~p:~p~n~p", [Error, Reason, Stacktrace]),
      {error, list_to_binary(Message)}
  end.

%% TODO delete
%%%%--------------------------------------------------------------------
%%%% @private
%%%% @doc
%%%% Executes test case once.
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec exec_test_repeat(SuiteName :: atom(), CaseName :: atom(), CaseConfig :: proplist()) ->
%%  {ok, [#parameter{}]} | {error, Reason :: binary()} | list().
%%exec_test_repeat(SuiteName, CaseName, CaseConfig) ->
%%  try
%%    Timestamp1 = os:timestamp(),
%%    Result = call_testcase(SuiteName, CaseName, [CaseConfig]),
%%    Timestamp2 = os:timestamp(),
%%    TestTime = utils:milliseconds_diff(Timestamp2, Timestamp1),
%%    % Return list of parameters consisting of default 'test_time' parameter
%%    % and parameters returned from test case.
%%    case is_stress_test() and (CaseName =:= stress_test) of
%%      true ->
%%        Result;
%%      _ ->
%%        {ok, [#parameter{
%%          name = test_time,
%%          description = "Test execution time.",
%%          value = TestTime,
%%          unit = "ms"} | lists:filter(fun
%%                                        (#parameter{}) -> true;
%%                                        (_) -> false
%%                                      end, lists:flatten([Result]))]}
%%    end
%%  catch
%%    Error:Reason ->
%%      Stacktrace = erlang:get_stacktrace(),
%%      Message = str_utils:format("~p:~p~n~p", [Error, Reason, Stacktrace]),
%%      {error, list_to_binary(Message)}
%%  end.

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
%% Returns current timestamp in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec get_timestamp() -> integer().
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + (Micro div 1000).

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
                       (#xmlElement{name = copyright, content  = [Copyright]}) ->
                         {true, list_to_binary(Copyright#xmlText.value)};
                       (_) -> false
                     end, Doc#xmlElement.content)).
