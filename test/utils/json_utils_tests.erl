%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of json utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(json_utils_tests).
-author("Tomasz Lichon").

-ifdef(TEST).

-include("errors.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SAMPLE_JSON, #{
    <<"name">> => <<"examplecorp-mymodule">>,
    <<"version">> => 1,
    <<"author">> => <<"Pat">>,
    <<"license">> => <<"Apache-2.0">>,
    <<"summary">> => <<"A module for a thing">>,
    <<"source">> => <<"https://github.com/examplecorp/examplecorp-mymodule">>,
    <<"project_page">> => <<"https://forge.puppetlabs.com/examplecorp/mymodule">>,
    <<"issues_url">> => <<"https://github.com/examplecorp/examplecorp-mymodule/issues">>,
    <<"tags">> => [<<"things">>, <<"stuff">>],
    <<"operatingsystem_support">> => #{
        <<"operatingsystem">> => <<"RedHat">>,
        <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>]
    },
    <<"dependencies">> => [
        #{
            <<"name">> => <<"puppetlabs/stdlib">>,
            <<"version_requirement">> => <<">= 3.2.0 < 5.0.0">>
        },
        #{
            <<"name">> => <<"puppetlabs/firewall">>,
            <<"version_requirement">> => <<">= 0.0.4 < 2.0.0">>
        }
    ],
    <<"data_provider">> => <<"hiera">>
}).


encode_map_test() ->
    Map = #{first => <<"first">>, second => <<"second">>},
    Json = json_utils:encode(Map),
    ?assertEqual(<<"{\"second\":\"second\",\"first\":\"first\"}">>, Json).


decode_map_test() ->
    Json = <<"{\"first\":\"first\",\"second\":\"second\"}">>,
    Map = json_utils:decode(Json),
    ?assertEqual(#{<<"second">> => <<"second">>, <<"first">> => <<"first">>}, Map).


change_map_to_proplist_test() ->
    Map = #{first => <<"first">>, second => <<"second">>},
    Proplist = json_utils:map_to_list(Map),
    ?assertEqual([{first, <<"first">>}, {second, <<"second">>}], Proplist).


change_proplist_to_map_test() ->
    Proplist = [{first, <<"first">>}, {second, <<"second">>}],
    Map = json_utils:list_to_map(Proplist),
    ?assertEqual(#{second => <<"second">>, first => <<"first">>}, Map).


change_nested_map_to_proplist_test() ->
    Map = #{key1 => <<"value1">>, key2 => #{key3 => <<"value3">>, key4 => []}},
    Proplist = json_utils:map_to_list(Map),
    ?assertEqual([{key1, <<"value1">>}, {key2, [{key3, <<"value3">>}, {key4, []}]}], Proplist).


change_nested_proplist_to_map_test() ->
    Proplist = [{key1, <<"value1">>}, {key2, [{key3, <<"value3">>}, {key4, []}]}],
    Map = json_utils:list_to_map(Proplist),
    ?assertEqual(#{key2 => #{key3 => <<"value3">>, key4 => []}, key1 => <<"value1">>}, Map).


encode_proplist_test() ->
    Proplist = [{first, <<"first">>}, {second, <<"second">>}],
    Json = json_utils:encode_deprecated(Proplist),
    ?assertEqual(<<"{\"second\":\"second\",\"first\":\"first\"}">>, Json).


decode_proplist_test() ->
    Json = <<"{\"first\":\"first\",\"second\":\"second\"}">>,
    Proplist = json_utils:decode_deprecated(Json),
    ?assertEqual([{<<"first">>, <<"first">>}, {<<"second">>, <<"second">>}], Proplist).


query_test_() ->
    [
        ?_assertEqual({ok, ?SAMPLE_JSON}, json_utils:query(?SAMPLE_JSON, [])),

        % GET LEVEL 0 ITEMS
        ?_assertEqual({ok, 1}, json_utils:query(?SAMPLE_JSON, [<<"version">>])),
        ?_assertEqual({ok, <<"Pat">>}, json_utils:query(?SAMPLE_JSON, [<<"author">>])),
        ?_assertEqual(
            {ok, [<<"things">>, <<"stuff">>]},
            json_utils:query(?SAMPLE_JSON, [<<"tags">>])
        ),
        ?_assertEqual(
            {ok, #{<<"operatingsystem">> => <<"RedHat">>, <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>]}},
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>])
        ),
        ?_assertEqual(
            error,
            json_utils:query(?SAMPLE_JSON, [<<"missing">>])
        ),

        % GET LEVEL 1 ITEMS
        ?_assertEqual(
            {ok, <<"RedHat">>},
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>, <<"operatingsystem">>])
        ),
        ?_assertEqual(
            {ok, [<<"5.0">>, <<"6.0">>]},
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>])
        ),
        ?_assertEqual(
            error,
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>, <<"missing">>])
        ),
        ?_assertEqual(
            error,
            json_utils:query(?SAMPLE_JSON, [<<"version">>, <<"missing">>])
        ),

        % GET LEVEL 2 ITEMS
        ?_assertEqual(
            {ok, <<"5.0">>},
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[0]">>])
        ),
        ?_assertEqual(
            {ok, <<"6.0">>},
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[1]">>])
        ),
        ?_assertEqual(
            error,
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[2]">>])
        ),
        ?_assertEqual(
            error,
            json_utils:query(?SAMPLE_JSON, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[NaN]">>])
        )
    ].


insert_test_() ->
    Json = ?SAMPLE_JSON,

    [
        % ADD LEVEL 0 ITEMS
        ?_assertEqual(
            {ok, Json#{<<"second_author">> => <<"Tom">>}},
            json_utils:insert(Json, <<"Tom">>, [<<"second_author">>])
        ),
        ?_assertEqual(
            {ok, Json#{<<"new_author">> => #{<<"first">> => <<"Tom">>, <<"second">> => <<"Pat">>}}},
            json_utils:insert(Json, #{<<"first">> => <<"Tom">>, <<"second">> => <<"Pat">>}, [<<"new_author">>])
        ),
        ?_assertEqual(
            {ok, Json#{<<"new_author">> => [<<"Tom">>, <<"Pat">>]}},
            json_utils:insert(Json, [<<"Tom">>, <<"Pat">>], [<<"new_author">>])
        ),

        % OVERRIDE LEVEL 0 ITEMS
        ?_assertEqual(
            {ok, Json#{<<"author">> => [<<"Tom">>, <<"Pat">>]}},
            json_utils:insert(Json, [<<"Tom">>, <<"Pat">>], [<<"author">>])
        ),

        % ADD LEVEL 1 ITEMS
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"repo_type">> => <<"yum">>,
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>]}
            }},
            json_utils:insert(Json, <<"yum">>, [<<"operatingsystem_support">>, <<"repo_type">>])
        ),
        ?_assertEqual(
            error,
            json_utils:insert(Json, <<"val">>, [<<"version">>, <<"attr">>])
        ),

        % OVERRIDE LEVEL 1 ITEMS
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"Ubuntu">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>]}
            }},
            json_utils:insert(Json, <<"Ubuntu">>, [<<"operatingsystem_support">>, <<"operatingsystem">>])
        ),

        % OVERRIDE LEVEL 2 ITEMS
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"4.0">>, <<"6.0">>]}
            }},
            json_utils:insert(Json, <<"4.0">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[0]">>])
        ),
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"7.0">>]}
            }},
            json_utils:insert(Json, <<"7.0">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[1]">>])
        ),

        % ADD LEVEL 2 ITEMS
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>, <<"7.0">>]}
            }},
            json_utils:insert(Json, <<"7.0">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[2]">>])
        ),
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>, null, null, <<"7.0">>]}
            }},
            json_utils:insert(Json, <<"7.0">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[4]">>])
        ),
        ?_assertEqual(
            error,
            json_utils:insert(Json, <<"7.0">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[NaN]">>])
        ),

        % ADD LEVEL 3 ITEMS
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>, [null, <<"7.1">>]]}
            }},
            json_utils:insert(Json, <<"7.1">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[2]">>, <<"[1]">>])
        ),
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>, #{<<"[NaN]">> => <<"7.1">>}]}
            }},
            json_utils:insert(Json, <<"7.1">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[2]">>, <<"[NaN]">>])
        ),
        ?_assertEqual(
            {ok, Json#{<<"operatingsystem_support">> => #{
                <<"operatingsystem">> => <<"RedHat">>,
                <<"operatingsystemrelease">> => [<<"5.0">>, <<"6.0">>, #{<<"attr">> => <<"7.1">>}]}
            }},
            json_utils:insert(Json, <<"7.1">>, [<<"operatingsystem_support">>, <<"operatingsystemrelease">>, <<"[2]">>, <<"attr">>])
        )
    ].


merge_single_json_test() ->
    Json1 = #{<<"a">> => <<"b">>},
    ?assertEqual(Json1, json_utils:merge([Json1])).


merge_empty_jsons_test() ->
    ?assertEqual(#{}, json_utils:merge([#{}, #{}])).


merge_empty_json_and_primitive_test() ->
    ?assertEqual(<<"string">>, json_utils:merge([#{}, <<"string">>])).


merge_primitive_and_empty_json_test() ->
    ?assertEqual(#{}, json_utils:merge([<<"string">>, #{}])).


merge_empty_json_and_non_empty_json_test() ->
    Json1 = #{},
    Json2 = #{<<"a">> => <<"b">>, <<"c">> => []},

    ?assertEqual(#{<<"a">> => <<"b">>, <<"c">> => []}, json_utils:merge([Json1, Json2])).


merge_non_empty_json_and_empty_json_test() ->
    Json1 = #{<<"a">> => <<"b">>, <<"c">> => []},
    Json2 = #{},

    ?assertEqual(#{<<"a">> => <<"b">>, <<"c">> => []}, json_utils:merge([Json1, Json2])).


merge_jsons_without_common_keys_test() ->
    Json1 = #{<<"a">> => <<"b">>, <<"c">> => #{<<"d">> => <<"d">>}},
    Json2 = #{<<"e">> => <<"f">>, <<"g">> => []},

    ?assertEqual(
        #{<<"a">> => <<"b">>, <<"c">> => #{<<"d">> => <<"d">>}, <<"e">> => <<"f">>, <<"g">> => []},
        json_utils:merge([Json1, Json2])
    ).


merge_jsons_with_simple_common_key_test() ->
    Json1 = #{<<"a">> => <<"b">>, <<"c">> => <<"d">>},
    Json2 = #{<<"a">> => <<"f">>, <<"g">> => []},

    ?assertEqual(
        #{<<"a">> => <<"f">>, <<"c">> => <<"d">>, <<"g">> => []},
        json_utils:merge([Json1, Json2])
    ).


merge_jsons_with_json_as_common_key_test() ->
    Json1 = #{<<"a">> => #{<<"a1">> => <<"b1">>}},
    Json2 = #{<<"a">> => #{<<"a2">> => <<"b2">>}},

    ?assertEqual(
        #{<<"a">> => #{<<"a1">> => <<"b1">>, <<"a2">> => <<"b2">>}},
        json_utils:merge([Json1, Json2])
    ).


merge_three_jsons_test() ->
    Json1 = #{<<"a">> => #{<<"a1">> => <<"b1">>}},
    Json2 = #{<<"a">> => #{<<"a2">> => <<"b2">>}, <<"b">> => <<"c">>},
    Json3 = #{<<"a">> => #{<<"a1">> => <<"b4">>, <<"a3">> => <<"b3">>}, <<"d">> => <<"e">>},

    ?assertEqual(
        #{
            <<"a">> =>  #{<<"a1">> => <<"b4">>, <<"a2">> => <<"b2">>, <<"a3">> => <<"b3">>},
            <<"b">> => <<"c">>,
            <<"d">> => <<"e">>
        },
        json_utils:merge([Json1, Json2, Json3])
    ).


-endif.