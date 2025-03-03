%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon, Lukasz Opiola
%%% @copyright (C) 2014-2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Assertion macros used in ct tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Tomasz Lichon").
-author("Lukasz Opiola").

-ifndef(ASSERTIONS_HRL).
-define(ASSERTIONS_HRL, 1).

-undef(TEST).
-define(TEST, true).
-include_lib("eunit/include/eunit.hrl").

% required, otherwise there will be warnings when two macros are used within
% the same block of code
-compile(nowarn_shadow_vars).

-record(failure_summary, {
    module :: atom(),
    line :: integer(),
    expected_expression :: term(),
    expected_value = undefined :: term(),
    actual_expression :: term(),
    actual_value :: term()
}).

-undef(assertMatch).
-define(assertMatch(Guard, ExpressionToCheck),
    ?assertMatch(Guard, ExpressionToCheck, 1)).
-define(assertMatch(Guard, ExpressionToCheck, Attempts),
    ?assertMatch(Guard, ExpressionToCheck, Attempts, timer:seconds(1))).
-define(assertMatch(Guard, ExpressionToCheck, Attempts, Interval), begin
    ((fun() ->
        lists_utils:foldl_while(fun(AttemptsLeft__Local, ActualValue__Local) ->
            case ActualValue__Local of
                Guard ->
                    {halt, ActualValue__Local};
                _ ->
                    case AttemptsLeft__Local of
                        1 ->
                            test_utils:ct_pal_failure_summary(
                                "assertMatch", #failure_summary{
                                    module = ?MODULE,
                                    line = ?LINE,
                                    expected_expression = (??Guard),
                                    actual_expression = (??ExpressionToCheck),
                                    actual_value = ActualValue__Local
                                }, annotate_diff
                            ),
                            erlang:error(assertMatch_failed);
                        _ ->
                            timer:sleep(Interval),
                            {cont, ExpressionToCheck}
                    end
            end
        end, ExpressionToCheck, lists:seq(max(Attempts, 1), 1, -1))
    end)())
end).

-undef(assertNotMatch).
-define(assertNotMatch(Guard, ExpressionToCheck),
    ?assertNotMatch(Guard, ExpressionToCheck, 1)).
-define(assertNotMatch(Guard, ExpressionToCheck, Attempts),
    ?assertNotMatch(Guard, ExpressionToCheck, Attempts, timer:seconds(1))).
-define(assertNotMatch(Guard, ExpressionToCheck, Attempts, Interval), begin
    ((fun() ->
        lists_utils:foldl_while(fun(AttemptsLeft__Local, ActualValue__Local) ->
            case ActualValue__Local of
                Guard ->
                    test_utils:ct_pal_failure_summary(
                        "assertNotMatch", #failure_summary{
                            module = ?MODULE,
                            line = ?LINE,
                            expected_expression = (??Guard),
                            actual_expression = (??ExpressionToCheck),
                            actual_value = ActualValue__Local
                        }, skip_diff_annotation
                    ),
                    erlang:error(assertNotMatch_failed);
                _ ->
                    case AttemptsLeft__Local of
                        1 ->
                            {halt, ActualValue__Local};
                        _ ->
                            timer:sleep(Interval),
                            {cont, ExpressionToCheck}
                    end
            end
        end, ExpressionToCheck, lists:seq(max(Attempts, 1), 1, -1))
    end)())
end).


-undef(assertEqual).
-define(assertEqual(Expectation, ExpressionToCheck),
    ?assertEqual(Expectation, ExpressionToCheck, 1)).
-define(assertEqual(Expectation, ExpressionToCheck, Attempts),
    ?assertEqual(Expectation, ExpressionToCheck, Attempts, timer:seconds(1))).
-define(assertEqual(Expectation, ExpressionToCheck, Attempts, Interval), begin
    ((fun() ->
        lists_utils:foldl_while(fun(AttemptsLeft__Local, ExpectedValue__Local) ->
            case eunit_utils:erase_ctx_if_error(ExpressionToCheck) of
                ExpectedValue__Local ->
                    {halt, ok};
                ActualValue__Local ->
                    case AttemptsLeft__Local of
                        1 ->
                            test_utils:ct_pal_failure_summary(
                                "assertEqual", #failure_summary{
                                    module = ?MODULE,
                                    line = ?LINE,
                                    expected_expression = (??Expectation),
                                    expected_value = ExpectedValue__Local,
                                    actual_expression = (??ExpressionToCheck),
                                    actual_value = ActualValue__Local
                                }, annotate_diff
                            ),
                            erlang:error(assertEqual_failed);
                        _ ->
                            timer:sleep(Interval),
                            {cont, Expectation}
                    end
            end
        end, eunit_utils:erase_ctx_if_error(Expectation), lists:seq(max(Attempts, 1), 1, -1))
    end)())
end).


-undef(assertNotEqual).
-define(assertNotEqual(Expectation, ExpressionToCheck),
    ?assertNotEqual(Expectation, ExpressionToCheck, 1)).
-define(assertNotEqual(Expectation, ExpressionToCheck, Attempts),
    ?assertNotEqual(Expectation, ExpressionToCheck, Attempts, timer:seconds(1))).
-define(assertNotEqual(Expectation, ExpressionToCheck, Attempts, Interval), begin
    ((fun() ->
        lists_utils:foldl_while(fun(AttemptsLeft__Local, ExpectedValue__Local) ->
            case (ExpressionToCheck) of
                ExpectedValue__Local when AttemptsLeft__Local == 1 ->
                    test_utils:ct_pal_failure_summary(
                        "assertNotEqual", #failure_summary{
                            module = ?MODULE,
                            line = ?LINE,
                            expected_expression = (??Expectation),
                            expected_value = ExpectedValue__Local,
                            actual_expression = (??ExpressionToCheck),
                            actual_value = ExpectedValue__Local
                        }, skip_diff_annotation
                    ),
                    erlang:error(assertNotEqual_failed);
                ExpectedValue__Local when AttemptsLeft__Local > 1 ->
                    timer:sleep(Interval),
                    {cont, Expectation};
                _ ->
                    {halt, ok}
            end
        end, Expectation, lists:seq(max(Attempts, 1), 1, -1))
    end)())
end).


-undef(assert).
-define(assert(ExpressionToCheck), ?assert(ExpressionToCheck, 1)).
-define(assert(ExpressionToCheck, Attempts), ?assert(ExpressionToCheck, Attempts, timer:seconds(1))).
% do not use literal 'true' atom to avoid warnings for clauses that cannot match,
% even if the expression is a constant or is known to be boolean-only.
-define(assert(ExpressionToCheck, Attempts, Interval),
    ?assertEqual(is_process_alive(self()), ExpressionToCheck, Attempts, Interval)).


-undef(assertNot).
-define(assertNot(ExpressionToCheck), ?assertNot(ExpressionToCheck, 1)).
-define(assertNot(ExpressionToCheck, Attempts), ?assertNot(ExpressionToCheck, Attempts, timer:seconds(1))).
% do not use literal 'false' atom to avoid warnings for clauses that cannot match,
% even if the expression is a constant or is known to be boolean-only.
-define(assertNot(ExpressionToCheck, Attempts, Interval),
    ?assertEqual(not is_process_alive(self()), ExpressionToCheck, Attempts, Interval)).


-define(assertReceivedMatch(Guard),
    ?assertReceivedMatch(Guard, 0)).
-define(assertReceivedMatch(Guard, Timeout), begin
    ((fun() ->
        receive
            Guard = Result__Local ->
                Result__Local
        after
            Timeout ->
                ActualValue__Local = receive
                    Result__Local -> Result__Local
                after
                    0 -> timeout
                end,
                test_utils:ct_pal_failure_summary(
                    "assertReceivedMatch", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = (??Guard),
                        actual_expression = (??ActualValue__Local),
                        actual_value = ActualValue__Local
                    }, annotate_diff
                ),
                erlang:error(assertReceivedMatch_failed)
        end
    end)())
end).


-define(assertReceivedNextMatch(Guard),
    ?assertReceivedNextMatch(Guard, 0)).
-define(assertReceivedNextMatch(Guard, Timeout), begin
    ((fun() ->
        receive
            Guard = Result__Local ->
                Result__Local;
            ActualValue__Local ->
                test_utils:ct_pal_failure_summary(
                    "assertReceivedNextMatch", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = (??Guard),
                        actual_expression = (??ActualValue__Local),
                        actual_value = ActualValue__Local
                    }, annotate_diff
                ),
                erlang:error(assertReceivedNextMatch_failed)
        after
            Timeout ->
                test_utils:ct_pal_failure_summary(
                    "assertReceivedNextMatch", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = (??Guard),
                        actual_expression = timeout,
                        actual_value = timeout
                    }, annotate_diff
                ),
                erlang:error(assertReceivedNextMatch_failed)
        end
    end)())
end).


-define(assertNotReceivedMatch(Guard),
    ?assertNotReceivedMatch(Guard, 0)).
-define(assertNotReceivedMatch(Guard, Timeout), begin
    ((fun() ->
        receive
            Guard = Result__Local ->
                test_utils:ct_pal_failure_summary(
                    "assertNotReceivedMatch", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = (??Guard),
                        expected_value = timeout,
                        actual_expression = (??Result__Local),
                        actual_value = Result__Local
                    }, skip_diff_annotation
                ),
                erlang:error(assertNotReceivedMatch_failed)
        after
            Timeout ->
                ok
        end
    end)())
end).


-define(assertReceivedEqual(Expectation),
    ?assertReceivedEqual(Expectation, 0)).
-define(assertReceivedEqual(Expectation, Timeout), begin
    ((fun(ExpectedValue__Local) ->
        receive
            ExpectedValue__Local ->
                ExpectedValue__Local
        after
            Timeout ->
                test_utils:ct_pal_failure_summary(
                    "assertReceivedEqual", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = (??Expectation),
                        expected_value = ExpectedValue__Local,
                        actual_expression = timeout,
                        actual_value = timeout
                    }, annotate_diff
                ),
                erlang:error(assertReceivedEqual_failed)
        end
    end)(Expectation))
end).


-define(assertReceivedNextEqual(Expectation),
    ?assertReceivedNextEqual(Expectation, 0)).
-define(assertReceivedNextEqual(Expectation, Timeout), begin
    ((fun(ExpectedValue__Local) ->
        receive
            ExpectedValue__Local ->
                ExpectedValue__Local;
            ActualValue__Local ->
                test_utils:ct_pal_failure_summary(
                    "assertReceivedNextEqual", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = (??Expectation),
                        expected_value = ExpectedValue__Local,
                        actual_expression = (??ActualValue__Local),
                        actual_value = ActualValue__Local
                    }, annotate_diff
                ),
                erlang:error(assertReceivedNextEqual_failed)
        after
            Timeout ->
                test_utils:ct_pal_failure_summary(
                    "assertReceivedNextEqual", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = (??Expectation),
                        expected_value = ExpectedValue__Local,
                        actual_expression = timeout,
                        actual_value = timeout
                    }, annotate_diff
                ),
                erlang:error(assertReceivedNextEqual_failed)
        end
    end)(Expectation))
end).


-define(assertNotReceivedEqual(Expectation),
    ?assertNotReceivedEqual(Expectation, 0)).
-define(assertNotReceivedEqual(Expectation, Timeout), begin
    ((fun(ExpectedValue__Local) ->
        receive
            ExpectedValue__Local ->
                test_utils:ct_pal_failure_summary(
                    "assertNotReceivedEqual", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = timeout,
                        expected_value = timeout,
                        actual_expression = (??Expectation),
                        actual_value = ExpectedValue__Local
                    }, skip_diff_annotation
                ),
                erlang:error(assertNotReceivedEqual_failed)
        after
            Timeout ->
                ok
        end
    end)(Expectation))
end).


-undef(assertException).
-define(assertException(Class, Term, ExpressionToCheck), begin
    ((fun() ->
        try (ExpressionToCheck) of
            ActualValue__Local ->
                test_utils:ct_pal_failure_summary(
                    "assertException", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = "{ " ++ (??Class) ++ " , " ++ (??Term) ++ " , [...] }",
                        actual_expression = (??ExpressionToCheck),
                        actual_value = ActualValue__Local
                    }, annotate_diff
                ),
                erlang:error(assertException_failed)
        catch
            Class:Term ->
                ok;
            ActualClass__Local:ActualTerm__Local:Stacktrace__Local ->
                test_utils:ct_pal_failure_summary(
                    "assertException", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = "{ " ++ (??Class) ++ " , " ++ (??Term) ++ " , [...] }",
                        actual_expression = (??ExpressionToCheck),
                        actual_value = {ActualClass__Local, ActualTerm__Local, Stacktrace__Local}
                    }, annotate_diff
                ),
                erlang:error(assertException_failed)
        end
    end)())
end).


-undef(assertNotException).
-define(assertNotException(ExpressionToCheck), begin
    ((fun() ->
        try (ExpressionToCheck) of
            _ ->
                ok
        catch
            ActualClass__Local:ActualTerm__Local:Stacktrace__Local ->
                test_utils:ct_pal_failure_summary(
                    "assertNotException", #failure_summary{
                        module = ?MODULE,
                        line = ?LINE,
                        expected_expression = "none",
                        actual_expression = (??ExpressionToCheck),
                        actual_value = {ActualClass__Local, ActualTerm__Local, Stacktrace__Local}
                    }, skip_diff_annotation
                ),
                erlang:error(assertNotException_failed)
        end
    end)())
end).


-endif.
