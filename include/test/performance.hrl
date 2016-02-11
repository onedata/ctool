%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains configuration records for performance tests.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(PERFORMANCE_HRL).
-define(PERFORMANCE_HRL, 1).

-define(BRANCH_ENV_VARIABLE, "branch").
-define(PERFORMANCE_ENV_VARIABLE, "performance").
-define(PERFORMANCE_RESULT_FILE, "performance.json").
-define(STRESS_ENV_VARIABLE, "stress").
-define(STRESS_NO_CLEARING_ENV_VARIABLE, "stress_no_clearing").
-define(STRESS_TIME_ENV_VARIABLE, "stress_time").
-define(STRESS_DEFAULT_TIME, timer:hours(3) div 1000).
-define(STRESS_ERRORS_TO_STOP, 100).
-define(STRESS_ETS_NAME, stress_ets).
-define(STRESS_TIMEOUT_EXTENSION_SECONDS, 600). % extension of ct timeout to let running tests end

%% output test parameter and also internal representation of input test
%% parameter used by performance test module
%% * name        - name of parameter
%% * description - human-readable description of parameter
%% * value       - value of parameter (IMPORTANT! this value must be numeric)
%% * unit        - unit of parameter
-record(parameter, {
    name :: atom(),
    description = "" :: string(),
    value :: term(),
    unit = "" :: string()
}).

-define(FUNCTION,
    element(2, element(2, process_info(self(), current_function)))
).

-define(ALL(CasesNames), ?ALL(CasesNames, [])).
-define(ALL(CasesNames, PerformanceCasesNames),
    performance:all(CasesNames, PerformanceCasesNames)
).

-define(STRESS_ALL(CasesNames, NoClearingCasesNames),
    performance:stress_all(?MODULE, CasesNames, NoClearingCasesNames)
).

-define(PERFORMANCE(Config, PerformanceConfig),
    performance:run_test(?MODULE, ?FUNCTION, Config, PerformanceConfig)
).

-define(STRESS(Config, StressConfig),
    performance:run_stress_test(?MODULE, Config, StressConfig)
).

-endif.