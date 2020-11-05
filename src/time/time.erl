%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions of time units/formats used across different components
%%% and utility functions for time format conversion.
%%% @end
%%%-------------------------------------------------------------------
-module(time).
-author("Lukasz Opiola").

-type hours() :: integer().
-type seconds() :: integer().
-type millis() :: integer().
-type micros() :: integer().
-type nanos() :: integer().
-export_type([hours/0, seconds/0, millis/0, micros/0, nanos/0]).

-type infinity() :: infinity.
-export_type([infinity/0]).

-type iso8601() :: binary(). % YYYY-MM-DDThh:mm:ssZ
-export_type([iso8601/0]).

-export([seconds_to_datetime/1, datetime_to_seconds/1]).
-export([seconds_to_iso8601/1, iso8601_to_seconds/1]).
-export([datetime_to_iso8601/1, iso8601_to_datetime/1]).

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

%%%===================================================================
%%% API
%%%===================================================================

-spec seconds_to_datetime(seconds()) -> calendar:datetime().
seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds + ?UNIX_EPOCH).


-spec datetime_to_seconds(calendar:datetime()) -> seconds().
datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.


-spec seconds_to_iso8601(seconds()) -> iso8601().
seconds_to_iso8601(Seconds) ->
    datetime_to_iso8601(seconds_to_datetime(Seconds)).


-spec iso8601_to_seconds(iso8601()) -> seconds().
iso8601_to_seconds(Iso8601) ->
    datetime_to_seconds(iso8601_to_datetime(Iso8601)).


-spec datetime_to_iso8601(calendar:datetime()) -> iso8601().
datetime_to_iso8601(DateTime) ->
    iso8601:format(DateTime).


-spec iso8601_to_datetime(iso8601()) -> calendar:datetime().
iso8601_to_datetime(Iso8601) ->
    iso8601:parse(binary_to_list(Iso8601)).
