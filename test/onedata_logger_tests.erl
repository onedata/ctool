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
-include("test/test_utils.hrl").

main_test_() ->
    {setup,
        fun() ->
            onedata_logger:set_loglevel(notice),
            ctool:set_env(default_loglevel, debug)
        end,
        fun(_) ->
            ok
        end,
        [
            {"set/get_loglevel, set/get_default_loglevel",
                fun() ->
                    ?assertEqual(notice, onedata_logger:get_current_loglevel()),
                    ?assert(not onedata_logger:should_log(debug)),
                    ?assert(not onedata_logger:should_log(info)),
                    ?assert(onedata_logger:should_log(notice)),
                    ?assert(onedata_logger:should_log(critical)),
                    ?assert(onedata_logger:should_log(emergency)),
                    
                    onedata_logger:set_loglevel(error),
                    ?assert(not onedata_logger:should_log(debug)),
                    ?assert(not onedata_logger:should_log(info)),
                    ?assert(not onedata_logger:should_log(notice)),
                    ?assert(not onedata_logger:should_log(warning)),
                    ?assert(onedata_logger:should_log(error)),
                    ?assert(onedata_logger:should_log(critical)),
                    ?assert(onedata_logger:should_log(alert)),
                    ?assert(onedata_logger:should_log(emergency)),
                    ?assertEqual(error, onedata_logger:get_current_loglevel()),
                    
                    onedata_logger:set_loglevel(default),
                    ?assertEqual(onedata_logger:get_default_loglevel(), onedata_logger:get_current_loglevel()),
                    ?assert(onedata_logger:should_log(debug)),
                    ?assert(onedata_logger:should_log(info)),
                    ?assert(onedata_logger:should_log(notice)),
                    ?assert(onedata_logger:should_log(warning)),
                    ?assert(onedata_logger:should_log(error)),
                    ?assert(onedata_logger:should_log(critical)),
                    ?assert(onedata_logger:should_log(alert)),
                    ?assert(onedata_logger:should_log(emergency))
                end
            }
        ]
    }.


logger_interfacing_test_() ->
    {setup,
        fun() ->
            onedata_logger:set_loglevel(debug)
        end,
        [
            {"log, set/get_include_stacktrace, compute_message, logging macros",
                fun() ->
                    onedata_logger:log(debug, #{}, "debug message"),
                    onedata_logger:log(info, #{}, "info message"),
                    onedata_logger:log(warning, #{}, "warning message"),
                    onedata_logger:log(error, #{}, "error message"),
                    onedata_logger:log(emergency, #{}, "emergency message"),
                    ?debug("debug message"),
                    ?debug("debug ~ts", ["message"]),
                    ?info("info message"),
                    try throw(test) catch Class:Reason:Stacktrace ->
                        ?warning_exception("warning message", Class, Reason, Stacktrace),
                        ?critical_exception("critical message ~tp", [?MODULE], Class, Reason, Stacktrace)
                    end,
                    ?error("error message"),
                    ?emergency("emergency message")
                end
            }
        ]
    }.


% makes sure the ?autoformat macro works with different kinds of inputs;
% checks if the code compiles and does not crash in runtime, the visual
% examination of the formatted output must be done manually
autoformatter_test() ->
    eunit_utils:dump(io:printable_range()),
    Integer = 17,
    Float = -13.75,
    Atom = atom,
    Bool = true,
    List = [1,2,3] ++ str_utils:binary_to_unicode_list(?RAND_UNICODE_STR()),
    PrintableString = str_utils:binary_to_unicode_list(?RAND_UNICODE_STR()),
    MultilineString = "mul-\n\tti-\n\tline\n\t" ++ str_utils:binary_to_unicode_list(?RAND_UNICODE_STR(5)),
    RawBinary = <<<<C>> || C <- lists:seq(1, 255)>>,
    PrintableBinary = ?RAND_UNICODE_STR(),
    MultilineBinary = <<"mul-\n\tti-\n\tline\n\t", (?RAND_UNICODE_STR())/binary>>,
    Map = #{<<"key">> => value},
    Term = {tuple_with, {complex_terms, [<<"1">>, #{2 => [true, dont_know, false]}, lists:seq(1, 20)]}},

    AutoformatAll = ?autoformat(
        Integer,
        Float,
        Atom,
        Bool,
        List,
        PrintableString,
        MultilineString,
        RawBinary,
        PrintableBinary,
        MultilineBinary,
        Map,
        Term
    ),
    ?assertEqual(str_utils:format(
        "\n"
        "    Integer = ~tp\n"
        "    Float = ~tp\n"
        "    Atom = ~tp\n"
        "    Bool = ~tp\n"
        "    List = ~tp\n"
        "    PrintableString = ~ts\n"
        "    MultilineString = ~ts\n"
        "    RawBinary = ~tp\n"
        "    PrintableBinary = ~ts\n"
        "    MultilineBinary = ~ts\n"
        "    Map = ~tp\n"
        "    Term = ~tp", [
            Integer,
            Float,
            Atom,
            Bool,
            List,
            PrintableString,
            MultilineString,
            RawBinary,
            PrintableBinary,
            MultilineBinary,
            Map,
            Term
        ]
    ), onedata_logger:format_generic_log(AutoformatAll, [])),
    ?error(AutoformatAll),

    ?assertEqual(str_utils:format(
        "Test message with test arg: test arg 1\n"
        "    Integer = ~tp\n"
        "    Float = ~tp\n"
        "    Atom = ~tp\n"
        "    Bool = ~tp\n"
        "    List = ~tp\n"
        "    PrintableString = ~ts\n"
        "    MultilineString = ~ts\n"
        "    RawBinary = ~tp\n"
        "    PrintableBinary = ~ts\n"
        "    MultilineBinary = ~ts\n"
        "    Map = ~tp\n"
        "    Term = ~tp", [
            Integer,
            Float,
            Atom,
            Bool,
            List,
            PrintableString,
            MultilineString,
            RawBinary,
            PrintableBinary,
            MultilineBinary,
            Map,
            Term
        ]),
        onedata_logger:format_generic_log(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1],
            Integer,
            Float,
            Atom,
            Bool,
            List,
            PrintableString,
            MultilineString,
            RawBinary,
            PrintableBinary,
            MultilineBinary,
            Map,
            Term
        ), [])
    ),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1],
        Integer,
        Float,
        Atom,
        Bool,
        List,
        PrintableString,
        MultilineString,
        RawBinary,
        PrintableBinary,
        MultilineBinary,
        Map,
        Term
    )),

    ?assertEqual(
        "Test message, no args.\n"
        "    Atom = atom\n"
        "    Bool = true\n"
        "    Integer = 17\n"
        "    Float = -13.75",
        onedata_logger:format_generic_log(
            ?autoformat_with_msg("Test message, no args.", [Atom, Bool, Integer, Float]), []
        )
    ),
    ?error(?autoformat_with_msg("Test message, no args.", [Atom, Bool, Integer, Float])).

-endif.
