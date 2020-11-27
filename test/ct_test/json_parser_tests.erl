%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% json_parsers eunits
%%% @end
%%%-------------------------------------------------------------------
-module(json_parser_tests).
-author("Tomasz Lichon").

-include_lib("eunit/include/eunit.hrl").

parse_json_binary_to_atom_proplist_test() ->
    ?assertEqual([{a, [a1]}, {b, [b1, b2]}], json_parser:parse_json_binary_to_atom_proplist(<<"{\"a\": [\"a1\"], \"b\": [\"b1\", \"b2\"]}">>)).
