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
                            FailureSummary = [
                                {module, ?MODULE},
                                {line, ?LINE},
                                {expression, (??ExpressionToCheck)},
                                {expected, (??Guard)},
                                {value, ActualValue}
                            ],
                            ct:print("assertMatch failed: ~p~n", [FailureSummary]),
                            erlang:error({assertMatch_failed, FailureSummary});
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
                            FailureSummary = [
                                {module, ?MODULE},
                                {line, ?LINE},
                                {expression, (??ExpressionToCheck)},
                                {expected, ExpectedValue},
                                {value, ActualValue}
                            ],
                            ct:print("assertEqual failed: ~p", [FailureSummary]),
                            erlang:error({assertEqual_failed, FailureSummary});
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
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Guard)},
                    {value, ActualValue}
                ],
                ct:print("assertReceivedMatch failed: ~p", [FailureSummary]),
                erlang:error({assertReceivedMatch_failed, FailureSummary})
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
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Guard)},
                    {value, ActualValue}
                ],
                ct:print("assertReceivedNextMatch failed: ~p", [FailureSummary]),
                erlang:error({assertReceivedNextMatch_failed, FailureSummary})
        after
            Timeout ->
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Guard)},
                    {value, timeout}
                ],
                ct:print("assertReceivedNextMatch failed: ~p", [FailureSummary]),
                erlang:error({assertReceivedNextMatch_failed, FailureSummary})
        end
    end)())
end).


-define(assertNotReceivedMatch(Guard),
    ?assertNotReceivedMatch(Guard, 0)).
-define(assertNotReceivedMatch(Guard, Timeout), begin
    ((fun() ->
        receive
            Guard = Result ->
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Guard)},
                    {expected, timeout},
                    {value, (??Result)}
                ],
                ct:print("assertNotReceivedMatch failed: ~p", [FailureSummary]),
                erlang:error({assertNotReceivedMatch_failed, FailureSummary})
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
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??ExpectedValue)},
                    {value, timeout}
                ],
                ct:print("assertReceivedEqual failed: ~p", [FailureSummary]),
                erlang:error({assertReceived_failed, FailureSummary})
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
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected, ExpectedValue},
                    {value, ActualValue}
                ],
                ct:print("assertReceivedNextEqual failed: ~p", [FailureSummary]),
                erlang:error({assertReceivedNextEqual_failed, FailureSummary})
        after
            Timeout ->
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??ExpectedValue)},
                    {value, timeout}
                ],
                ct:print("assertReceivedNextEqual failed: ~p", [FailureSummary]),
                erlang:error({assertReceivedNextEqual_failed, FailureSummary})
        end
    end)(Expectation))
end).


-define(assertNotReceivedEqual(Expectation),
    ?assertNotReceivedEqual(Expectation, 0)).
-define(assertNotReceivedEqual(Expectation, Timeout), begin
    ((fun(ExpectedValue) ->
        receive
            ExpectedValue ->
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected, timeout},
                    {value, ExpectedValue}
                ],
                ct:print("assertNotReceivedEqual failed: ~p", [FailureSummary]),
                erlang:error({assertNotReceivedEqual_failed, FailureSummary})
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
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??ExpressionToCheck)},
                    {expected, "{ " ++ (??Class) ++ " , " ++ (??Term) ++ " , [...] }"},
                    {unexpected_success, ActualValue}
                ],
                ct:print("assertException failed: ~p", [FailureSummary]),
                erlang:error({assertException_failed, FailureSummary})
        catch
            Class:Term ->
                ok;
            ActualClass:ActualTerm ->
                FailureSummary = [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??ExpressionToCheck)},
                    {expected, "{ " ++ (??Class) ++ " , " ++ (??Term) ++ " , [...] }"},
                    {unexpected_exception, {ActualClass, ActualTerm, erlang:get_stacktrace()}}
                ],
                ct:print("assertException failed: ~p", [FailureSummary]),
                erlang:error({assertException_failed, FailureSummary})
        end
    end)())
end).

-endif.
