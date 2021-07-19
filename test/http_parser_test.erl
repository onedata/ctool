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

-include("http/headers.hrl").
-include_lib("eunit/include/eunit.hrl").


parse_bytes_ranges_test_() ->
    ContentSize = 100,
    PBase = fun
        (undefined, ContentSize) ->
            http_parser:parse_range_header(#{headers => #{}}, ContentSize);
        (RangeBin, ContentSize) ->
            http_parser:parse_range_header(#{headers => #{?HDR_RANGE => RangeBin}}, ContentSize)
    end,
    P = fun(RangeOrUndefined) -> PBase(RangeOrUndefined, ContentSize) end,

    [
        ?_assertEqual(undefined, P(undefined)),

        % Parsing invalid format should fail
        ?_assertEqual(invalid, P(<<"unicorns">>)),
        ?_assertEqual(invalid, P(<<"bytes:5-10">>)),
        ?_assertEqual(invalid, P(<<"bytes=5=10">>)),
        ?_assertEqual(invalid, P(<<"bytes=10-5">>)),
        ?_assertEqual(invalid, P(<<"bytes=-5-">>)),
        ?_assertEqual(invalid, P(<<"bytes=10--5">>)),
        ?_assertEqual(invalid, P(<<"bytes=10-15-">>)),

        % Parsing entirely negative range should fail
        ?_assertEqual(invalid, P(<<"bytes=-15-10">>)),

        % Parsing entirely exceeding content size range should fail
        ?_assertEqual(invalid, P(<<"bytes=100-150">>)),

        % Parsing proper format should succeed
        ?_assertEqual([{10, 99}], P(<<"bytes=10-">>)),
        ?_assertEqual([{90, 99}], P(<<"bytes=-10">>)),
        ?_assertEqual([{10, 15}], P(<<"bytes=10-15">>)),
        ?_assertEqual([{10, 15}, {50, 50}, {95, 99}], P(<<"bytes=10-15,50-50,-5">>)),

        % White spaces and redundant commas should be ignored but only after 'bytes=' part
        ?_assertEqual(invalid, P(<<"  bytes=  , 10-15 , 50-50,    -5  ">>)),
        ?_assertEqual(invalid, P(<<"bytes   =  , 10-15 , 50-50,    -5  ">>)),
        ?_assertEqual([{10, 15}, {50, 50}, {95, 99}], P(<<"bytes=,,, ,    , 10-15,50-50,, \t  ,,,-5  ,,  ,">>)),
        ?_assertEqual([{10, 15}, {50, 50}, {95, 99}], P(<<"bytes=,,, ,    , 10-15, ,50-50,, \t  ,,,-5  ,,  ,">>)),
        
        % Parsing header with unknown content size should be possible
        ?_assertEqual([{10, unknown}], PBase(<<"bytes=10-">>, unknown)),
        ?_assertEqual(invalid, PBase(<<"bytes=-10">>, unknown)),
        ?_assertEqual([{10, 15}], PBase(<<"bytes=10-15">>, unknown)),
        ?_assertEqual([{10, 15}, {50, 50}], PBase(<<"bytes=10-15,50-50">>, unknown)),

        % When only RangeEnd exceeds ContentSize it should be trimmed
        ?_assertEqual([{80, 99}], P(<<"bytes=80-120">>))
    ].


parse_query_string_test_() ->
    P = fun
        (undefined) ->
            http_parser:parse_query_string(#{qs => <<>>});
        (QsBin) ->
            http_parser:parse_query_string(#{qs => QsBin})
    end,

    [
        ?_assertEqual(#{}, P(undefined)),
        ?_assertEqual(#{<<"key">> => true}, P(<<"key">>)),
        ?_assertEqual(#{<<"key">> => <<"true">>}, P(<<"key=true">>)),
        ?_assertEqual(#{<<"key">> => <<"val">>}, P(<<"key=val">>)),
        ?_assertEqual(#{<<"key">> => <<"10">>}, P(<<"key=10">>)),
        ?_assertEqual(#{<<"key1">> => <<"val">>, <<"key2">> => <<"10">>}, P(<<"key2=10&key1=val">>)),
        ?_assertEqual(#{<<"key">> => [<<"val3">>, <<"val2">>, <<"val1">>]}, P(<<"key=val1&key=val2&key=val3">>)),

        ?_assertEqual(
            #{
                <<"key1">> => [<<"val3">>, <<"val2">>, <<"val1">>],
                <<"key2">> => true,
                <<"key3">> => <<"false">>,
                <<"key4">> => <<"val">>
            }, P(<<"key2&key1=val1&key1=val2&key1=val3&key3=false&key4=val">>)
        )
    ].


-endif.
