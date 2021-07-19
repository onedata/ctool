%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of onedata_logger module, using eunit tests.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_logger_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("logging.hrl").

main_test_() ->
    {setup,
        fun() ->
            ctool:set_env(current_loglevel, 2),
            ctool:set_env(default_loglevel, 0)
        end,
        fun(_) ->
            ok
        end,
        [
            {"should_log, set/get_loglevel, set/get_default_loglevel",
                fun() ->
                    ?assertEqual(onedata_logger:get_current_loglevel(), 2),
                    ?assert(not onedata_logger:should_log(1)),
                    ?assert(not onedata_logger:should_log(0)),
                    ?assert(onedata_logger:should_log(5)),
                    ?assert(onedata_logger:should_log(2)),
                    ?assert(onedata_logger:should_log(7)),
                    onedata_logger:set_loglevel(error),
                    ?assertEqual(onedata_logger:get_current_loglevel(), 4),
                    ?assert(not onedata_logger:should_log(1)),
                    ?assert(not onedata_logger:should_log(0)),
                    ?assert(not onedata_logger:should_log(2)),
                    ?assert(onedata_logger:should_log(6)),
                    ?assert(onedata_logger:should_log(7)),
                    onedata_logger:set_loglevel(default),
                    ?assertEqual(onedata_logger:get_default_loglevel(), onedata_logger:get_current_loglevel()),
                    ?assert(onedata_logger:should_log(1)),
                    ?assert(onedata_logger:should_log(4)),
                    ?assert(onedata_logger:should_log(5)),
                    ?assert(onedata_logger:should_log(6)),
                    ?assert(onedata_logger:should_log(3))
                end
            },

            {"parse_process_info",
                fun() ->
                    Proplist = onedata_logger:parse_process_info({pid, {some_module, some_fun, some_arity}}),
                    ?assertEqual(Proplist, [{module, some_module}, {function, some_fun}, {arity, some_arity}])
                end
            },

            {"loglevel conversion",
                fun() ->
                    ?assertEqual(debug, onedata_logger:loglevel_int_to_atom(onedata_logger:loglevel_atom_to_int(debug))),
                    ?assertEqual(notice, onedata_logger:loglevel_int_to_atom(onedata_logger:loglevel_atom_to_int(notice))),
                    ?assertEqual(5, onedata_logger:loglevel_atom_to_int(onedata_logger:loglevel_int_to_atom(5))),
                    ?assertEqual(1, onedata_logger:loglevel_atom_to_int(onedata_logger:loglevel_int_to_atom(1)))
                end
            }
        ]
    }.


lager_interfacing_test_() ->
    {setup,
        fun() ->
            ctool:set_env(current_loglevel, 0),
            meck:new(lager)
        end,
        fun(_) ->
            ok = meck:unload(lager)
        end,
        [
            {"dispatch_log, set/get_include_stacktrace, compute_message, logging macros",
                fun() ->
                    meck:expect(lager, log,
                        fun(debug, _, _, ["debug message"]) -> ok;
                            (info, _, _, ["info message"]) -> ok;
                            (warning, _, _, ["warning message" ++ _Stacktrace]) ->
                                ok;
                            (error, _, _, ["error message"]) -> ok;
                            (emergency, _, _, ["emergency message"]) -> ok
                        end),

                    onedata_logger:set_include_stacktrace(false),
                    onedata_logger:dispatch_log(0, [], "debug ~s", ["message"], false),
                    onedata_logger:dispatch_log(1, [], "info message", [], false),
                    onedata_logger:dispatch_log(3, [], "warning message", [], false),
                    onedata_logger:dispatch_log(4, [], "error ~s", ["message"], false),
                    onedata_logger:set_include_stacktrace(false),
                    onedata_logger:dispatch_log(7, [], "emergency ~s", ["message"], false),
                    ?debug("debug message"),
                    ?debug("debug ~s", ["message"]),
                    ?info("info message"),
                    ?warning_stacktrace(
                        "warning message",
                        try throw(test) catch _:_:Stacktrace -> Stacktrace end
                    ),
                    ?error("error message"),
                    ?emergency("emergency message"),
                    ?assert(meck:validate(lager))
                end
            }
        ]
    }.

-endif.