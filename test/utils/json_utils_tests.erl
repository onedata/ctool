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

-include_lib("eunit/include/eunit.hrl").

-export([encode_proplist_test/0, decode_test/0]).

encode_proplist_test() ->
    Proplist = [{first, <<"first">>}, {second, <<"second">>}],
    Json = json_utils:encode(Proplist),
    ?assertEqual(<<"{\"first\":\"first\",\"second\":\"second\"}">>, Json).

decode_test() ->
    Json = <<"{\"first\":\"first\",\"second\":\"second\"}">>,
    Proplist = json_utils:decode(Json),
    ?assertEqual([{<<"first">>, <<"first">>}, {<<"second">>, <<"second">>}], Proplist).