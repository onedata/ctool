%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This file tests concerning http parser functionality.
%%% @end
%%%--------------------------------------------------------------------
-module(http_parser_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


parse_bytes_ranges_test_() ->
    ContentSize = 100,

    [
        % Parsing invalid format should fail
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"unicorns">>, ContentSize)
        ),
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes:5-10">>, ContentSize)
        ),
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes=5=10">>, ContentSize)
        ),
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes=10-5">>, ContentSize)
        ),
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes=-5-">>, ContentSize)
        ),
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes=10--5">>, ContentSize)
        ),
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes=10-15-">>, ContentSize)
        ),

        % Parsing entirely negative range should fail
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes=-15-10">>, ContentSize)
        ),

        % Parsing entirely exceeding content size range should fail
        ?_assertEqual(
            invalid,
            http_parser:parse_bytes_ranges(<<"bytes=100-150">>, ContentSize)
        ),

        % Parsing proper format should succeed
        ?_assertEqual(
            [{10, 99}],
            http_parser:parse_bytes_ranges(<<"bytes=10-">>, ContentSize)
        ),
        ?_assertEqual(
            [{90, 99}],
            http_parser:parse_bytes_ranges(<<"bytes=-10">>, ContentSize)
        ),
        ?_assertEqual(
            [{10, 15}],
            http_parser:parse_bytes_ranges(<<"bytes=10-15">>, ContentSize)
        ),
        ?_assertEqual(
            [{10, 15}, {50, 50}, {95, 99}],
            http_parser:parse_bytes_ranges(<<"bytes=10-15,50-50,-5">>, ContentSize)
        ),

        % When only RangeEnd exceeds ContentSize it should be trimmed
        ?_assertEqual(
            [{80, 99}],
            http_parser:parse_bytes_ranges(<<"bytes=80-120">>, ContentSize)
        )
    ].


-endif.
