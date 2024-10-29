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
    module = ?MODULE :: atom(),
    line = ?LINE :: integer(),
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
        lists_utils:foldl_while(fun(AttemptsLeft, ActualValue) ->
            case ActualValue of
                Guard ->
                    {halt, ActualValue};
                _ ->
                    case AttemptsLeft of
                        1 ->
                            {Format, Args} = test_utils:format_failure_summary(
                                "assertMatch failed:", #failure_summary{
                                    expected_expression = (??Guard),
                                    actual_expression = (??ExpressionToCheck),
                                    actual_value = ActualValue
                                }
                            ),
                            ct:pal(str_utils:format(Format, Args)),
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
        lists_utils:foldl_while(fun(AttemptsLeft, ActualValue) ->
            case ActualValue of
                Guard ->
                    {Format, Args} = test_utils:format_failure_summary(
                        "assertNotMatch failed:", #failure_summary{
                            expected_expression = (??Guard),
                            actual_expression = (??ExpressionToCheck),
                            actual_value = ActualValue
                        }, negative
                    ),
                    ct:pal(str_utils:format(Format, Args)),
                    erlang:error(assertNotMatch_failed);
                _ ->
                    case AttemptsLeft of
                        1 ->
                            {halt, ActualValue};
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
        lists_utils:foldl_while(fun(AttemptsLeft, ExpectedValue) ->
            case (ExpressionToCheck) of
                ExpectedValue ->
                    {halt, ok};
                ActualValue ->
                    case AttemptsLeft of
                        1 ->
                            {Format, Args} = test_utils:format_failure_summary(
                                "assertEqual failed:", #failure_summary{
                                    expected_expression = (??Expectation),
                                    expected_value = ExpectedValue,
                                    actual_expression = (??ExpressionToCheck),
                                    actual_value = ActualValue
                                }
                            ),
                            ct:pal(str_utils:format(Format, Args)),
                            erlang:error(assertEqual_failed);
                        _ ->
                            timer:sleep(Interval),
                            {cont, Expectation}
                    end
            end
        end, Expectation, lists:seq(max(Attempts, 1), 1, -1))
    end)())
end).


-undef(assertNotEqual).
-define(assertNotEqual(Expectation, ExpressionToCheck),
    ?assertNotEqual(Expectation, ExpressionToCheck, 1)).
-define(assertNotEqual(Expectation, ExpressionToCheck, Attempts),
    ?assertNotEqual(Expectation, ExpressionToCheck, Attempts, timer:seconds(1))).
-define(assertNotEqual(Expectation, ExpressionToCheck, Attempts, Interval), begin
    ((fun() ->
        lists_utils:foldl_while(fun(AttemptsLeft, ExpectedValue) ->
            case (ExpressionToCheck) of
                ExpectedValue ->
                    {Format, Args} = test_utils:format_failure_summary(
                        "assertNotEqual failed:", #failure_summary{
                            expected_expression = (??Expectation),
                            expected_value = ExpectedValue,
                            actual_expression = (??ExpressionToCheck),
                            actual_value = ExpectedValue
                        }, negative
                    ),
                    ct:pal(str_utils:format(Format, Args)),
                    erlang:error(assertNotEqual_failed);
                _ ->
                    case AttemptsLeft of
                        1 ->
                            {halt, ok};
                        _ ->
                            timer:sleep(Interval),
                            {cont, Expectation}
                    end
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
            Guard = Result ->
                Result
        after
            Timeout ->
                ActualValue = receive
                    Result -> Result
                after
                    0 -> timeout
                end,
                {Format, Args} = test_utils:format_failure_summary(
                    "assertReceivedMatch failed:", #failure_summary{
                        expected_expression = (??Guard),
                        actual_expression = (??ActualValue),
                        actual_value = ActualValue
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertReceivedMatch_failed)
        end
    end)())
end).


-define(assertReceivedNextMatch(Guard),
    ?assertReceivedNextMatch(Guard, 0)).
-define(assertReceivedNextMatch(Guard, Timeout), begin
    ((fun() ->
        receive
            Guard = Result ->
                Result;
            ActualValue ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertReceivedNextMatch failed:", #failure_summary{
                        expected_expression = (??Guard),
                        actual_expression = (??ActualValue),
                        actual_value = ActualValue
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertReceivedNextMatch_failed)
        after
            Timeout ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertReceivedNextMatch failed:", #failure_summary{
                        expected_expression = (??Guard),
                        actual_expression = timeout,
                        actual_value = timeout
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertReceivedNextMatch_failed)
        end
    end)())
end).


-define(assertNotReceivedMatch(Guard),
    ?assertNotReceivedMatch(Guard, 0)).
-define(assertNotReceivedMatch(Guard, Timeout), begin
    ((fun() ->
        receive
            Guard = Result ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertNotReceivedMatch failed:", #failure_summary{
                        expected_expression = (??Guard),
                        expected_value = timeout,
                        actual_expression = (??Result),
                        actual_value = Result
                    }, negative
                ),
                ct:pal(str_utils:format(Format, Args)),
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
    ((fun(ExpectedValue) ->
        receive
            ExpectedValue ->
                ExpectedValue
        after
            Timeout ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertReceivedEqual failed:", #failure_summary{
                        expected_expression = (??Expectation),
                        expected_value = ExpectedValue,
                        actual_expression = timeout,
                        actual_value = timeout
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertReceivedEqual_failed)
        end
    end)(Expectation))
end).


-define(assertReceivedNextEqual(Expectation),
    ?assertReceivedNextEqual(Expectation, 0)).
-define(assertReceivedNextEqual(Expectation, Timeout), begin
    ((fun(ExpectedValue) ->
        receive
            ExpectedValue ->
                ExpectedValue;
            ActualValue ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertReceivedNextEqual failed:", #failure_summary{
                        expected_expression = (??Expectation),
                        expected_value = ExpectedValue,
                        actual_expression = (??ActualValue),
                        actual_value = ActualValue
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertReceivedNextEqual_failed)
        after
            Timeout ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertReceivedNextEqual failed:", #failure_summary{
                        expected_expression = (??Expectation),
                        expected_value = ExpectedValue,
                        actual_expression = timeout,
                        actual_value = timeout
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertReceivedNextEqual_failed)
        end
    end)(Expectation))
end).


-define(assertNotReceivedEqual(Expectation),
    ?assertNotReceivedEqual(Expectation, 0)).
-define(assertNotReceivedEqual(Expectation, Timeout), begin
    ((fun(ExpectedValue) ->
        receive
            ExpectedValue ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertNotReceivedEqual failed:", #failure_summary{
                        expected_expression = timeout,
                        expected_value = timeout,
                        actual_expression = (??Expectation),
                        actual_value = ExpectedValue
                    }, negative
                ),
                ct:pal(str_utils:format(Format, Args)),
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
            ActualValue ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertException failed:", #failure_summary{
                        expected_expression = "{ " ++ (??Class) ++ " , " ++ (??Term) ++ " , [...] }",
                        actual_expression = (??ExpressionToCheck),
                        actual_value = ActualValue
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertException_failed)
        catch
            Class:Term ->
                ok;
            ActualClass:ActualTerm:Stacktrace ->
                {Format, Args} = test_utils:format_failure_summary(
                    "assertException failed:", #failure_summary{
                        expected_expression = "{ " ++ (??Class) ++ " , " ++ (??Term) ++ " , [...] }",
                        actual_expression = (??ExpressionToCheck),
                        actual_value = {ActualClass, ActualTerm, Stacktrace}
                    }
                ),
                ct:pal(str_utils:format(Format, Args)),
                erlang:error(assertException_failed)
        end
    end)())
end).

-endif.
