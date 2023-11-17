%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This file contains ct tests helper macros and definitions.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(TEST_UTILS_HRL).
-define(TEST_UTILS_HRL, 1).

-include("assertions.hrl").
-include("performance.hrl").
-include("cth_common.hrl").
-include("../logging.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(eunit_dump(Arg), eunit_utils:dump(??Arg, Arg)).
-define(ct_dump(Arg), ct:print("~s = ~p", [??Arg, Arg])).

-define(ct_pal_exception(DetailsStr, Class, Reason, Stacktrace),
    ?ct_pal_exception(DetailsStr, "", Class, Reason, Stacktrace)
).
-define(ct_pal_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace),
    ct:pal(onedata_logger:format_exception_log(
        ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
        DetailsFormat, DetailsArgs, undefined,
        Class, Reason, Stacktrace
    ))
).

-define(ct_catch_exceptions(Expr), begin
    ((fun() ->
        try
            Expr
        catch
            Class:Reason:Stacktrace ->
                ?ct_pal_exception(
                    "Test crashed!",
                    Class, Reason, Stacktrace
                ),
                error(test_crashed)
        end
    end)())
end).

-define(eunit_print_exception(DetailsStr, Class, Reason, Stacktrace),
    ?eunit_print_exception(DetailsStr, "", Class, Reason, Stacktrace)
).
-define(eunit_print_exception(DetailsFormat, DetailsArgs, Class, Reason, Stacktrace),
    eunit_utils:debug_log(onedata_logger:format_exception_log(
        ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE,
        DetailsFormat, DetailsArgs, undefined,
        Class, Reason, Stacktrace
    ), [])
).

-define(eunit_catch_exceptions(Expr), begin
    ((fun() ->
        try
            Expr
        catch
            Class:Reason:Stacktrace ->
                ?eunit_print_exception(
                    "Test crashed!",
                    Class, Reason, Stacktrace
                ),
                error(test_crashed)
        end
    end)())
end).


%% temporary directory for test files
-define(TEMP_DIR, "/tmp").

%% Returns absolute path to given file in the test data directory
-define(TEST_FILE(Config, Path), filename:join(test_utils:data_dir(Config), Path)).

-define(TEST_RELEASE_ETC_DIR(Path), filename:join("/root/bin/node/etc", Path)).

-define(DEFAULT_ENV_DESCRIPTION, "env_desc.json").

-define(MOCK_MANAGER_NAME, mock_manager).

%% keys that can be added to Config to pass arguments to CT hooks
-define(ENV_DESCRIPTION, env_description).
-define(CTH_ENV_UP, cth_env_up).
-define(LOAD_MODULES, load_modules).
-define(ENV_UP_POSTHOOK, env_up_posthook).

%% values that can be added to Config to pass arguments to CT hooks
-define(DISABLE, disable). % used to disable cth_env_up


%% Macro used to generate case name for default init/end_per_testcase
-define(DEFAULT_CASE(Case), list_to_atom(atom_to_list(Case) ++ "_default")).

%% Utility macros
-define(CURRENT_HOST, ?GET_HOSTNAME(node())).
-define(NODE(NodeHost, NodeName), list_to_atom(atom_to_list(NodeName) ++ "@" ++ atom_to_list(NodeHost))).
-define(GET_NODE_NAME(FullName), list_to_atom(hd(string:tokens(atom_to_list(FullName), "@")))).
-define(GET_HOSTNAME(FullName), list_to_atom(lists:last(string:tokens(atom_to_list(FullName), "@")))).
-define(GET_DOMAIN(FullName), list_to_atom(string:join(lists:sublist(string:tokens(atom_to_list(?GET_HOSTNAME(FullName)), "."), 2, 10), "."))).
-define(GET_DOMAIN_BIN(FullName), list_to_binary(string:join(lists:sublist(string:tokens(atom_to_list(?GET_HOSTNAME(FullName)), "."), 2, 10), "."))).

-ifdef(config).
-undef(config).
-endif.

-define(config(Key, Config), proplists:get_value(Key, Config)).
-define(config(Key, Config, Default), proplists:get_value(Key, Config, Default)).

%% Types used in tests
-type mock_opt() :: passthrough | non_strict | unstick | no_link | no_history.

-define(EXAMPLE_UNICODE_CHARS,
    "1234567890qwertyuiop[]asdfghjkl;'\\<zxcvbnm,./əłżź.ćńµóπœę©ß←↓→óþł…ə’ŋæðśążźć„”ńµ"
    "_πœę ßþą_śðæπœęßパル-µńćźżąśð日本を- 旅す. d'ŋ-ジ(ャパル)パスで 日本を- 旅す.る()"
).

-define(RAND_ELEMENT(List), lists_utils:random_element(List)).
-define(RAND_CHOICE(A, B), ?RAND_ELEMENT([A, B])).
-define(RAND_CHOICE(A, B, C), ?RAND_ELEMENT([A, B, C])).
-define(RAND_CHOICE(A, B, C, D), ?RAND_ELEMENT([A, B, C, D])).
-define(RAND_STR(), ?RAND_STR(16)).
-define(RAND_STR(Size), string:slice(str_utils:rand_hex(Size), 0, Size)).
-define(RAND_UNICODE_STR(), ?RAND_UNICODE_STR(30)).
-define(RAND_UNICODE_STR(Size), str_utils:unicode_list_to_binary(string:slice(lists:flatten(
    lists:duplicate(ceil(Size / string:length(?EXAMPLE_UNICODE_CHARS)), ?SHUFFLED(?EXAMPLE_UNICODE_CHARS))
), 0, Size))).
-define(RAND_BOOL(), ?RAND_CHOICE(true, false)).
-define(RAND_INT(To), ?RAND_INT(0, To)).
-define(RAND_INT(From, To), From + rand:uniform(To - From + 1) - 1).
-define(RAND_FLOAT(From, To), From + rand:uniform() * (To - From)).
-define(RAND_JSON_TERM(), ?RAND_ELEMENT([
    ?RAND_BOOL(), ?RAND_STR(), ?RAND_INT(-1000, 1000), -27.8, 17.75,
    [1, 2, 3], #{<<"a">> => <<"B">>}, [#{<<"a">> => <<"B">>}, #{<<"c">> => <<"D">>}]
])).
-define(RAND_SUBLIST(List), lists_utils:random_sublist(List)).
-define(RAND_SUBLIST(List, Length), ?RAND_SUBLIST(List, Length, Length)).
-define(RAND_SUBLIST(List, MinLength, MaxLength), lists_utils:random_sublist(List, MinLength, MaxLength)).
-define(SHUFFLED(List), lists_utils:shuffle(List)).
-define(RAND_SUBMAP(Map), maps:with(lists_utils:random_sublist(maps:keys(Map)), Map)).
-define(RAND_EMAIL_ADDRESS(), str_utils:format_bin("~s@example.com", [?RAND_STR(20)])).
-define(RAND_OBJECTID, ?RAND_OBJECTID(?RAND_STR(16))).
-define(RAND_OBJECTID(SpaceId), ?check(file_id:guid_to_objectid(file_id:pack_guid(str_utils:rand_hex(4), SpaceId)))).
-define(RAND_CANONICAL_PATH(SpaceId),
    filename:join([<<"/">>, SpaceId] ++ lists_utils:generate(fun() -> ?RAND_STR() end, ?RAND_INT(0, 5)))
).


-define(TOO_LONG_NAME, <<
    "very_very_very_long_name_with_at_least_128_characters_yes_indeed_more_"
    "than_one_hundred_twenty_eight_characters_that_should_not_be_allowed"
>>).

-endif.