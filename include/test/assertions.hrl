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

-undef(assert).
-define(assert(BoolExpr),
    ((fun() ->
        case (BoolExpr) of
            true -> ok;
            __V ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??BoolExpr)},
                    {expected, true},
                    {value, __V}],
                ct:print("assertion_failed: ~p~n", [__Args]),
                erlang:error({assertion_failed, __Args})
        end
    end)())).

-undef(assertMatch).
-define(assertMatch(Guard, Expr),
    ((fun() ->
        case (Expr) of
            Guard -> Expr;
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

-define(assertReceived(Expect), ?assertReceived(Expect, 0)).

-define(assertReceived(Expect, Timeout),
    ((fun() ->
        receive
            Expect = Result -> Result
        after
            Timeout ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expected, (??Expect)},
                    {value, timeout}],
                ct:print("assertReceived_failed: ~p~n", [__Args]),
                erlang:error({assertReceived_failed, __Args})
        end
    end)())).

-define(assertNotReceived(Expect), ?assertNotReceived(Expect, 0)).

-define(assertNotReceived(Expect, Timeout),
    ((fun() ->
        receive
            Expect = Result ->
                __Args = [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expect)},
                    {expected, timeout},
                    {value, (??Result)}],
                ct:print("assertNotReceived_failed: ~p~n", [__Args]),
                erlang:error({assertNotReceived_failed, __Args})
        after
            Timeout -> ok
        end
    end)())).

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
