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
%%
%%main_test_() ->
%%    {setup,
%%        fun() ->
%%            ctool:set_env(current_loglevel, 5),
%%            ctool:set_env(default_loglevel, 7)
%%        end,
%%        fun(_) ->
%%            ok
%%        end,
%%        [
%%            {"should_log, set/get_loglevel, set/get_default_loglevel",
%%                fun() ->
%%                    ?assertEqual(onedata_logger:get_current_loglevel(), 5),
%%                    ?assert(not onedata_logger:should_log(6)),
%%                    ?assert(not onedata_logger:should_log(7)),
%%                    ?assert(onedata_logger:should_log(2)),
%%                    ?assert(onedata_logger:should_log(5)),
%%                    ?assert(onedata_logger:should_log(0)),
%%                    onedata_logger:set_loglevel(error),
%%                    ?assertEqual(onedata_logger:get_current_loglevel(), 3),
%%                    ?assert(not onedata_logger:should_log(6)),
%%                    ?assert(not onedata_logger:should_log(7)),
%%                    ?assert(not onedata_logger:should_log(5)),
%%                    ?assert(not onedata_logger:should_log(4)),
%%                    ?assert(onedata_logger:should_log(1)),
%%                    ?assert(onedata_logger:should_log(0)),
%%                    onedata_logger:set_loglevel(default),
%%                    ?assertEqual(onedata_logger:get_default_loglevel(), onedata_logger:get_current_loglevel()),
%%                    ?assert(onedata_logger:should_log(1)),
%%                    ?assert(onedata_logger:should_log(4)),
%%                    ?assert(onedata_logger:should_log(5)),
%%                    ?assert(onedata_logger:should_log(6)),
%%                    ?assert(onedata_logger:should_log(3))
%%                end
%%            },
%%
%%            {"parse_process_info",
%%                fun() ->
%%                    Proplist = onedata_logger:parse_process_info({pid, {some_module, some_fun, some_arity}}),
%%                    ?assertEqual(Proplist, [{module, some_module}, {function, some_fun}, {arity, some_arity}])
%%                end
%%            },
%%
%%            {"loglevel conversion",
%%                fun() ->
%%                    ?assertEqual(debug, onedata_logger:loglevel_int_to_atom(onedata_logger:loglevel_atom_to_int(debug))),
%%                    ?assertEqual(notice, onedata_logger:loglevel_int_to_atom(onedata_logger:loglevel_atom_to_int(notice))),
%%                    ?assertEqual(5, onedata_logger:loglevel_atom_to_int(onedata_logger:loglevel_int_to_atom(5))),
%%                    ?assertEqual(1, onedata_logger:loglevel_atom_to_int(onedata_logger:loglevel_int_to_atom(1)))
%%                end
%%            }
%%        ]
%%    }.
%%
%%
%%lager_interfacing_test_() ->
%%    {setup,
%%        fun() ->
%%            ctool:set_env(current_loglevel, 7),
%%            meck:new(lager, [passthrough])
%%        end,
%%        fun(_) ->
%%            ok = meck:unload(lager)
%%        end,
%%        [
%%            {"log, set/get_include_stacktrace, compute_message, logging macros",
%%                fun() ->
%%                    meck:expect(lager, log,
%%                        fun(debug, _, _, ["debug message"]) -> ok;
%%                            (info, _, _, ["info message"]) -> ok;
%%                            (warning, _, _, ["warning message"]) -> ok;
%%                            (warning, _, _, ["An unexpected exception" ++ _]) -> ok;
%%                            (critical, _, _, ["An unexpected exception" ++ _]) -> ok;
%%                            (error, _, _, ["error message"]) -> ok;
%%                            (emergency, _, _, ["emergency message"]) -> ok
%%                        end),
%%
%%                    onedata_logger:log(7, [], "debug message"),
%%                    onedata_logger:log(6, [], "info message"),
%%                    onedata_logger:log(4, [], "warning message"),
%%                    onedata_logger:log(3, [], "error message"),
%%                    onedata_logger:log(0, [], "emergency message"),
%%                    ?debug("debug message"),
%%                    ?debug("debug ~ts", ["message"]),
%%                    ?info("info message"),
%%                    try throw(test) catch Class:Reason:Stacktrace ->
%%                        ?warning_exception("warning message", Class, Reason, Stacktrace),
%%                        ?critical_exception("critical message ~tp", [?MODULE], Class, Reason, Stacktrace)
%%                    end,
%%                    ?error("error message"),
%%                    ?emergency("emergency message"),
%%                    ?assert(meck:validate(lager))
%%                end
%%            }
%%        ]
%%    }.


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

    ?assertEqual("\n    Integer = 17", onedata_logger:format_generic_log(?autoformat(Integer))),
    ?dump(?autoformat(Integer)),
    ?error(?autoformat(Integer)),

    ?assertEqual("\n    Float = -13.75", onedata_logger:format_generic_log(?autoformat([Float]))),
    ?dump(?autoformat([Float])),
    ?error(?autoformat([Float])),

    ?assertEqual("\n    PrintableString = " ++ PrintableString,
        onedata_logger:format_generic_log(?autoformat(PrintableString))),
    ?dump(?autoformat(PrintableString)),
    ?error(?autoformat(PrintableString)),

    ?assertEqual(str_utils:format("\n    PrintableString = ~ts\n    RawBinary = ~tp", [PrintableString, RawBinary]),
        onedata_logger:format_generic_log(?autoformat([PrintableString, RawBinary]))),
    ?dump(?autoformat([PrintableString, RawBinary])),
    ?error(?autoformat([PrintableString, RawBinary])),

    ?assertEqual(str_utils:format("\n    MultilineString = ~ts\n    MultilineBinary = ~ts",
        [MultilineString, MultilineBinary]),
        onedata_logger:format_generic_log(?autoformat(MultilineString, MultilineBinary))),
    ?dump(?autoformat(MultilineString, MultilineBinary)),
    ?error(?autoformat(MultilineString, MultilineBinary)),

    ?assertEqual(str_utils:format("\n    Map = ~tp\n    Term = ~tp\n    List = ~tp", [Map, Term, List]),
        onedata_logger:format_generic_log(?autoformat(Map, Term, List))),
    ?dump(?autoformat(Map, Term, List)),
    ?error(?autoformat(Map, Term, List)),

    ?assertEqual("\n    Atom = atom\n    Bool = true\n    Bool = true",
        onedata_logger:format_generic_log(?autoformat([Atom, Bool, Bool]))),
    ?dump(?autoformat([Atom, Bool, Bool])),
    ?error(?autoformat([Atom, Bool, Bool])),

    AutoformatAll = ?autoformat(
        Integer,
        Float,
        Atom,
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
        "\n    Integer = ~tp\n    Float = ~tp\n    Atom = ~tp\n    List = ~tp" ++
        "\n    PrintableString = ~ts\n    MultilineString = ~ts\n    RawBinary = ~tp" ++
        "\n    PrintableBinary = ~ts\n    MultilineBinary = ~ts\n    Map = ~tp\n    Term = ~tp",
        [Integer, Float, Atom, List, PrintableString, MultilineString, RawBinary,
            PrintableBinary, MultilineBinary, Map, Term]),
    onedata_logger:format_generic_log(AutoformatAll)),
    ?dump(AutoformatAll),
    ?error(AutoformatAll),

    ?assertEqual("Test message with test arg: test arg 1\n    Integer = 17",
        onedata_logger:format_generic_log(?autoformat_with_msg("Test message with test arg: ~ts ~tp",
            ["test arg", 1], Integer))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], Integer)),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], Integer)),

    ?assertEqual("Test message with test arg: test arg 1\n    Float = -13.75",
        onedata_logger:format_generic_log(?autoformat_with_msg("Test message with test arg: ~ts ~tp",
            ["test arg", 1], [Float]))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [Float])),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [Float])),

    ?assertEqual("Test message with test arg: test arg 1\n    PrintableString = " ++ PrintableString,
        onedata_logger:format_generic_log(?autoformat_with_msg("Test message with test arg: ~ts ~tp",
            ["test arg", 1], PrintableString))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], PrintableString)),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], PrintableString)),

    ?assertEqual(str_utils:format("Test message with test arg: test arg 1\n    PrintableString = ~ts\n    RawBinary = ~tp",
        [PrintableString, RawBinary]), onedata_logger:format_generic_log(
        ?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [PrintableString, RawBinary]))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [PrintableString, RawBinary])),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [PrintableString, RawBinary])),

    ?assertEqual(str_utils:format("Test message with test arg: test arg 1\n    MultilineString = ~ts\n    MultilineBinary = ~ts",
        [MultilineString, MultilineBinary]), onedata_logger:format_generic_log(
        ?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], MultilineString, MultilineBinary))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], MultilineString, MultilineBinary)),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], MultilineString, MultilineBinary)),

    ?assertEqual(str_utils:format("Test message with test arg: test arg 1\n    Map = ~tp\n    Term = ~tp\n    List = ~tp", [Map, Term, List]),
        onedata_logger:format_generic_log(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], Map, Term, List))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], Map, Term, List)),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], Map, Term, List)),

    ?assertEqual("Test message with test arg: test arg 1\n    Atom = atom\n    Bool = true\n    Bool = true",
        onedata_logger:format_generic_log(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [Atom, Bool, Bool]))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [Atom, Bool, Bool])),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1], [Atom, Bool, Bool])),


    ?assertEqual(str_utils:format(
        "Test message with test arg: test arg 1\n    Integer = ~tp\n    Float = ~tp\n    Atom = ~tp\n    List = ~tp" ++
        "\n    PrintableString = ~ts\n    MultilineString = ~ts\n    RawBinary = ~tp" ++
        "\n    PrintableBinary = ~ts\n    MultilineBinary = ~ts\n    Map = ~tp\n    Term = ~tp",
        [Integer, Float, Atom, List, PrintableString, MultilineString, RawBinary,
            PrintableBinary, MultilineBinary, Map, Term]),
    onedata_logger:format_generic_log(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1],
        Integer,
        Float,
        Atom,
        List,
        PrintableString,
        MultilineString,
        RawBinary,
        PrintableBinary,
        MultilineBinary,
        Map,
        Term
    ))),
    ?dump(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1],
        Integer,
        Float,
        Atom,
        List,
        PrintableString,
        MultilineString,
        RawBinary,
        PrintableBinary,
        MultilineBinary,
        Map,
        Term
    )),
    ?error(?autoformat_with_msg("Test message with test arg: ~ts ~tp", ["test arg", 1],
        Integer,
        Float,
        Atom,
        List,
        PrintableString,
        MultilineString,
        RawBinary,
        PrintableBinary,
        MultilineBinary,
        Map,
        Term
    )),

    ?assertEqual("Test message, no args.\n    Atom = atom\n    Integer = 17\n    Float = -13.75",
        onedata_logger:format_generic_log(?autoformat_with_msg("Test message, no args.", [Atom, Integer, Float]))),
    ?dump(?autoformat_with_msg("Test message, no args.", [Atom, Integer, Float])),
    ?error(?autoformat_with_msg("Test message, no args.", [Atom, Integer, Float])).

-endif.
