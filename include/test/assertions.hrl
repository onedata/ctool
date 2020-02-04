%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Assertion macros used in ct tests.
%%% @end
%%% Created : 04. May 2014 9:09 PM
%%%-------------------------------------------------------------------
-author("Tomasz Lichon").

-ifndef(ASSERTIONS_HRL).
-define(ASSERTIONS_HRL, 1).

-undef(TEST).
-define(TEST, true).
-include_lib("eunit/include/eunit.hrl").

-define(assertMatchFun(Guard, Expr),
    fun(__Print__) ->
        case (Expr) of
            __Result__ = Guard -> __Result__;
            __Value__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected, (??Guard)},
                    {value, __Value__}],
                if
                    __Print__ -> ct:print("assertMatch_failed: ~p~n", [__Args__]);
                    true -> ok
                end,
                erlang:error({assertMatch_failed, __Args__})
        end
    end).

-undef(assertMatch).
-define(assertMatch(Guard, Expr),
    ((?assertMatchFun(Guard, Expr))(true))).

-define(assertMatch(Expect, Expr, Attempts),
    ?assertMatch(Expect, Expr, Attempts, timer:seconds(1))).

-define(assertMatch(Expect, Expr, Attempts, Timeout),
    ((fun() ->
        __Matcher__ = lists:foldl(fun
            (_, {true, __Value__}) -> {true, __Value__};
            (__Fun__, {false, __Value__}) ->
                try
                    {true, __Fun__(false)}
                catch
                    error:{assertMatch_failed, _} ->
                        timer:sleep(Timeout),
                        {false, __Value__}
                end
        end, {false, undefined}, lists:duplicate(Attempts - 1,
            ?assertMatchFun(Expect, Expr))),
        case __Matcher__ of
            {true, __Value__} -> __Value__;
            {false, _} ->
                ?assertMatch(Expect, Expr)
        end
    end)())).

-define(assertEqualFun(Expect, Expr),
    fun(__Expect__, __Print__) ->
        case (Expr) of
            __Expect__ -> ok;
            __Value__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected, __Expect__},
                    {value, __Value__}],
                if
                    __Print__ -> ct:print("assertEqual_failed: ~p~n", [__Args__]);
                    true -> ok
                end,
                erlang:error({assertEqual_failed, __Args__})
        end
    end).

-undef(assertEqual).
-define(assertEqual(Expect, Expr),
    ((?assertEqualFun(Expect, Expr))(Expect, true))).

-define(assertEqual(Expect, Expr, Attempts),
    ?assertEqual(Expect, Expr, Attempts, timer:seconds(1))).

-define(assertEqual(Expect, Expr, Attempts, Timeout),
    ((fun() ->
        __E = lists:foldl(fun
            (_, true) -> true;
            (__Fun__, false) ->
                try
                    __Fun__(Expect, false),
                    true
                catch
                    error:{assertEqual_failed, _} ->
                        timer:sleep(Timeout),
                        false
                end
        end, false, lists:duplicate(Attempts - 1, ?assertEqualFun(Expect, Expr))),
        case __E of
            true -> ok;
            false ->
                ?assertEqual(Expect, Expr)
        end
    end)())).

-undef(assert).
-define(assert(Expr), ?assertEqual(true, Expr)).

-define(assertReceivedMatch(Expect), ?assertReceivedMatch(Expect, 0)).

-define(assertReceivedMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = __Result__ -> __Result__
        after
            Timeout ->
                __Reason__ = receive
                    __Result__ -> __Result__
                after
                    0 -> timeout
                end,

                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, __Reason__}],
                ct:print("assertReceivedMatch_failed: ~p~n", [__Args__]),
                erlang:error({assertReceivedMatch_failed, __Args__})
        end
    end)())).

-define(assertReceivedNextMatch(Expect), ?assertReceivedNextMatch(Expect, 0)).

-define(assertReceivedNextMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = __Result__ -> __Result__;
            __Value__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, __Value__}],
                ct:print("assertReceivedNextMatch_failed: ~p~n", [__Args__]),
                erlang:error({assertReceivedNextMatch_failed, __Args__})
        after
            Timeout ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceivedNextMatch_failed: ~p~n", [__Args__]),
                erlang:error({assertReceivedNextMatch_failed, __Args__})
        end
    end)())).

-define(assertNotReceivedMatch(Expect), ?assertNotReceivedMatch(Expect, 0)).

-define(assertNotReceivedMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = __Result__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expect)},
                    {expected, timeout},
                    {value, (??__Result__)}],
                ct:print("assertNotReceivedMatch_failed: ~p~n", [__Args__]),
                erlang:error({assertNotReceivedMatch_failed, __Args__})
        after
            Timeout -> ok
        end
    end)())).

-define(assertReceivedEqual(Expect), ?assertReceivedEqual(Expect, 0)).

-define(assertReceivedEqual(Expect, Timeout),
    ((fun(__Expect__) ->
        receive
            __Expect__ -> __Expect__
        after
            Timeout ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceivedEqual_failed: ~p~n", [__Args__]),
                erlang:error({assertReceived_failed, __Args__})
        end
    end)(Expect))).

-define(assertReceivedNextEqual(Expect), ?assertReceivedNextEqual(Expect, 0)).

-define(assertReceivedNextEqual(Expect, Timeout),
    ((fun(__Expect__) ->
        receive
            __Expect__ -> __Expect__;
            __Value__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, __Expect__},
                    {value, __Value__}],
                ct:print("assertReceivedNextEqual_failed: ~p~n", [__Args__]),
                erlang:error({assertReceivedNextEqual_failed, __Args__})
        after
            Timeout ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceivedNextEqual_failed: ~p~n", [__Args__]),
                erlang:error({assertReceivedNextEqual_failed, __Args__})
        end
    end)(Expect))).

-define(assertNotReceivedEqual(Expect), ?assertNotReceivedEqual(Expect, 0)).

-define(assertNotReceivedEqual(Expect, Timeout),
    ((fun(__Expect__) ->
        receive
            __Expect__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, timeout},
                    {value, __Expect__}],
                ct:print("assertNotReceivedEqual_failed: ~p~n", [__Args__]),
                erlang:error({assertNotReceivedEqual_failed, __Args__})
        after
            Timeout -> ok
        end
    end)(Expect))).

-undef(assertException).
-define(assertException(Class, Term, Expr),
    ((fun() ->
        try (Expr) of
            __Value__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected,
                            "{ " ++ (??Class) ++ " , " ++ (??Term)
                            ++ " , [...] }"},
                    {unexpected_success, __Value__}],
                ct:print("assertException_failed: ~p~n", [__Args__]),
                erlang:error({assertException_failed, __Args__})
        catch
            Class:Term -> ok;
            __Class__:__Term__ ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected,
                            "{ " ++ (??Class) ++ " , " ++ (??Term)
                            ++ " , [...] }"},
                    {unexpected_exception,
                        {__Class__, __Term__, erlang:get_stacktrace()}}],
                ct:print("assertException_failed: ~p~n", [__Args__]),
                erlang:error({assertException_failed, __Args__})
        end
    end)())).

-undef(cmdStatus).
-define(cmdStatus(Code, Cmd),
    ((fun() ->
        case ?_cmd_(Cmd) of
            {(Code), __Out} -> __Out;
            {__Code__, _} ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {command, (Cmd)},
                    {expected_status, (Code)},
                    {status, __Code__}],
                ct:print("command_failed: ~p~n", [__Args__]),
                erlang:error({command_failed, __Args__})
        end
    end)())).

-undef(assertCmdStatus).
-define(assertCmdStatus(Code, Cmd),
    ((fun() ->
        case ?_cmd_(Cmd) of
            {(Code), _} -> ok;
            {__Code__, _} ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {command, (Cmd)},
                    {expected_status, (Code)},
                    {status, __Code__}],
                ct:print("assertCmd_failed: ~p~n", [__Args__]),
                erlang:error({assertCmd_failed, __Args__})
        end
    end)())).

-undef(assertCmdOutput).
-define(assertCmdOutput(Output, Cmd),
    ((fun() ->
        case ?_cmd_(Cmd) of
            {_, (Output)} -> ok;
            {_, __Output__} ->
                __Args__ = [{module, ?MODULE},
                    {line, ?LINE},
                    {command, (Cmd)},
                    {expected_output, (Output)},
                    {output, __Output__}],
                ct:print("assertCmdOutput_failed: ~p~n", [__Args__]),
                erlang:error({assertCmdOutput_failed, __Args__})
        end
    end)())).

-endif.
