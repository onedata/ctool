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

-include_lib("eunit/include/eunit.hrl").

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

-endif.