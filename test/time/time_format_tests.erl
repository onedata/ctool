%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for time_format module.
%%%--------------------------------------------------------------------
-module(time_format_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Eunit tests
%%%===================================================================

general_conversion_test() ->
    Seconds = clock:timestamp_seconds(),
    DateTime = time_format:seconds_to_datetime(Seconds),
    Iso8601 = time_format:datetime_to_iso8601(DateTime),

    ?assertEqual(Seconds, time_format:datetime_to_seconds(DateTime)),
    ?assertEqual(Seconds, time_format:iso8601_to_seconds(Iso8601)),

    ?assertEqual(DateTime, time_format:seconds_to_datetime(Seconds)),
    ?assertEqual(DateTime, time_format:iso8601_to_datetime(Iso8601)),

    ?assertEqual(Iso8601, time_format:seconds_to_iso8601(Seconds)),
    ?assertEqual(Iso8601, time_format:datetime_to_iso8601(DateTime)).


specific_conversion_test() ->
    Seconds = 1600956903,
    SecondsMidnight = 1600905600,
    Iso8601 = <<"2020-09-24T14:15:03Z">>,
    Iso8601DateOnly = <<"2020-09-24">>,
    DateTime = {{2020, 9, 24}, {14, 15, 3}},
    DateTimeMidnight = {{2020, 9, 24}, {0, 0, 0}},

    ?assertEqual(Seconds, time_format:datetime_to_seconds(DateTime)),
    ?assertEqual(Seconds, time_format:iso8601_to_seconds(Iso8601)),

    ?assertEqual(DateTime, time_format:seconds_to_datetime(Seconds)),
    ?assertEqual(DateTime, time_format:iso8601_to_datetime(Iso8601)),

    ?assertEqual(Iso8601, time_format:seconds_to_iso8601(Seconds)),
    ?assertEqual(Iso8601, time_format:datetime_to_iso8601(DateTime)),

    ?assertEqual(SecondsMidnight, time_format:iso8601_to_seconds(Iso8601DateOnly)),
    ?assertEqual(DateTimeMidnight, time_format:iso8601_to_datetime(Iso8601DateOnly)).


-endif.
