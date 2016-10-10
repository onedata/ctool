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

%% This macro returns name of a current function as an atom.
-define(FUNCTION,
    element(2, element(2, process_info(self(), current_function)))
).

%% This macro is used to determine which test cases will be started.
%% It depends on type of test (ct or performance).
%% For stress test it returns an empty list.
%% If you want to start stress test you should use macro STRESS_ALL.
%% Example usage:
%% all() -> ?ALL([t1, t2, t3], [t2, t4]).
-define(ALL(CasesNames), ?ALL(CasesNames, [])).
-define(ALL(CasesNames, PerformanceCasesNames),
    performance:all(CasesNames, PerformanceCasesNames)
).

%% This macro is used in stress tests to determine which test cases will
%% be started normally and which in no-clearing mode.
%% Example usage:
%% all() -> ?STRESS_ALL([t1, t2, t3], [t2, t4]).
-define(STRESS_ALL(CasesNames, NoClearingCasesNames),
    performance:stress_all(?MODULE, CasesNames, NoClearingCasesNames)
).

%% This macro is used to define performance tests parameters.
%% It should be used as definition of test function.
%% Code of your test must be defined in function with the same name with
%% suffix "_base"
%% Both functions must be exported.
%% Example usage:
%%
%%-export([t1, t1_base]).
%% t1(Config) ->
%%    ?PERFORMANCE(Config, [
%%        %performance_parameters...
%%    ]).
%%
%% t1_base(Config) ->
%%    %code of your test ...
%%
-define(PERFORMANCE(Config, PerformanceConfig),
    performance:run_test(?MODULE, ?FUNCTION, Config, PerformanceConfig)
).

%% This macro is used to define stress test parameters.
%% You should use in definition of stress_test function
%% Example usage:
%%
%%-export([stress_test, stress_test_base]).
%% stress_test(Config) ->
%%    ?STRESS(Config, [
%%        %stress_parameters...
%%    ]).
%%
%% stress_test_base(Config) ->
%%    performance:stress_test(Config).
%%
-define(STRESS(Config, StressConfig),
    performance:run_stress_test(?MODULE, Config, StressConfig)
).

-define(STRESS_TEST_BASE(Config),
    performance:stress_test(Config, ?MODULE)).

-endif.