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

-undef(assertMatch).
-define(assertMatch(Guard, Expr),
    ((fun() ->
        case (Expr) of
            Guard = Result -> Result;
            __V ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected, (??Guard)},
                    {value, __V}],
                ct:print("assertMatch_failed: ~p~n", [__Args]),
                erlang:error({assertMatch_failed, __Args})
        end
    end)())).

-undef(assertEqual).
-define(assertEqual(Expect, Expr),
    ((fun(__X) ->
        case (Expr) of
            __X -> ok;
            __V ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected, __X},
                    {value, __V}],
                ct:print("assertEqual_failed: ~p~n", [__Args]),
                erlang:error({assertEqual_failed, __Args})
        end
    end)(Expect))).

-undef(assert).
-define(assert(Expr), ?assertEqual(true, Expr)).

-define(assertReceivedMatch(Expect), ?assertReceivedMatch(Expect, 0)).

-define(assertReceivedMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = Result -> Result
        after
            Timeout ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceivedMatch_failed: ~p~n", [__Args]),
                erlang:error({assertReceivedMatch_failed, __Args})
        end
    end)())).

-define(assertReceivedNextMatch(Expect), ?assertReceivedNextMatch(Expect, 0)).

-define(assertReceivedNextMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = Result -> Result;
            __V ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, __V}],
                ct:print("assertReceivedNextMatch_failed: ~p~n", [__Args]),
                erlang:error({assertReceivedNextMatch_failed, __Args})
        after
            Timeout ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceivedNextMatch_failed: ~p~n", [__Args]),
                erlang:error({assertReceivedNextMatch_failed, __Args})
        end
    end)())).

-define(assertNotReceivedMatch(Expect), ?assertNotReceivedMatch(Expect, 0)).

-define(assertNotReceivedMatch(Expect, Timeout),
    ((fun() ->
        receive
            Expect = Result ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expect)},
                    {expected, timeout},
                    {value, (??Result)}],
                ct:print("assertNotReceivedMatch_failed: ~p~n", [__Args]),
                erlang:error({assertNotReceivedMatch_failed, __Args})
        after
            Timeout -> ok
        end
    end)())).

-define(assertReceivedEqual(Expect), ?assertReceivedEqual(Expect, 0)).

-define(assertReceivedEqual(Expect, Timeout),
    ((fun(__X) ->
        receive
            __X -> __X
        after
            Timeout ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceivedEqual_failed: ~p~n", [__Args]),
                erlang:error({assertReceived_failed, __Args})
        end
    end)(Expect))).

-define(assertReceivedNextEqual(Expect), ?assertReceivedNextEqual(Expect, 0)).

-define(assertReceivedNextEqual(Expect, Timeout),
    ((fun(__X) ->
        receive
            __X -> __X;
            __V ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, __X},
                    {value, __V}],
                ct:print("assertReceivedNextEqual_failed: ~p~n", [__Args]),
                erlang:error({assertReceivedNextEqual_failed, __Args})
        after
            Timeout ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceivedNextEqual_failed: ~p~n", [__Args]),
                erlang:error({assertReceivedNextEqual_failed, __Args})
        end
    end)(Expect))).

-define(assertNotReceivedEqual(Expect), ?assertNotReceivedEqual(Expect, 0)).

-define(assertNotReceivedEqual(Expect, Timeout),
    ((fun(__X) ->
        receive
            __X ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, timeout},
                    {value, __X}],
                ct:print("assertNotReceivedEqual_failed: ~p~n", [__Args]),
                erlang:error({assertNotReceivedEqual_failed, __Args})
        after
            Timeout -> ok
        end
    end)(Expect))).

-undef(assertException).
-define(assertException(Class, Term, Expr),
    ((fun() ->
        try (Expr) of
            __V ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected,
                            "{ " ++ (??Class) ++ " , " ++ (??Term)
                            ++ " , [...] }"},
                    {unexpected_success, __V}],
                ct:print("assertException_failed: ~p~n", [__Args]),
                erlang:error({assertException_failed, __Args})
        catch
            Class:Term -> ok;
            __C:__T ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {expected,
                            "{ " ++ (??Class) ++ " , " ++ (??Term)
                            ++ " , [...] }"},
                    {unexpected_exception,
                        {__C, __T, erlang:get_stacktrace()}}],
                ct:print("assertException_failed: ~p~n", [__Args]),
                erlang:error({assertException_failed, __Args})
        end
    end)())).

-undef(cmdStatus).
-define(cmdStatus(N, Cmd),
    ((fun() ->
        case ?_cmd_(Cmd) of
            {(N), __Out} -> __Out;
            {__N, _} ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {command, (Cmd)},
                    {expected_status, (N)},
                    {status, __N}],
                ct:print("command_failed: ~p~n", [__Args]),
                erlang:error({command_failed, __Args})
        end
    end)())).

-undef(assertCmdStatus).
-define(assertCmdStatus(N, Cmd),
    ((fun() ->
        case ?_cmd_(Cmd) of
            {(N), _} -> ok;
            {__N, _} ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {command, (Cmd)},
                    {expected_status, (N)},
                    {status, __N}],
                ct:print("assertCmd_failed: ~p~n", [__Args]),
                erlang:error({assertCmd_failed, __Args})
        end
    end)())).

-undef(assertCmdOutput).
-define(assertCmdOutput(T, Cmd),
    ((fun() ->
        case ?_cmd_(Cmd) of
            {_, (T)} -> ok;
            {_, __T} ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {command, (Cmd)},
                    {expected_output, (T)},
                    {output, __T}],
                ct:print("assertCmdOutput_failed: ~p~n", [__Args]),
                erlang:error({assertCmdOutput_failed, __Args})
        end
    end)())).

-endif.