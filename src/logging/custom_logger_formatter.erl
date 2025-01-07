%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module covers customize logger formatting.
%%% @end
%%%-------------------------------------------------------------------
-module(custom_logger_formatter).

-include("logging.hrl").

-export([format/2]).

%%%===================================================================
%%% API
%%%===================================================================


-spec format(logger:log_event(), map()) -> unicode:chardata().
format(Map = #{level:=Level, meta:=Meta}, Config) ->
    Timestamp = maps:get(time, Meta),
    Label = default_level(Level),

    UpdatedConfig = Config#{single_line => false, template => customize_template(Config, Label, Timestamp)},
    logger_formatter:format(Map, UpdatedConfig).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec customize_template(map(), list(), integer()) -> list().
customize_template(Config, Label, Timestamp) ->
    Template = maps:get(template, Config, []),
    NoDate = maps:get(no_date, Config, false),
    lists:map(
        fun
            (level) ->
                Label;
            (time) -> case NoDate of
                false -> format_timestamp(Timestamp);
                true -> format_time(Timestamp)
            end;
            (Other) ->
                Other
        end,
        Template
    ).


%% @private
-spec default_level(atom()) -> list().
default_level(debug) -> "D";
default_level(info) -> "I";
default_level(notice) -> "N";
default_level(warning) -> "W";
default_level(error) -> "E";
default_level(critical) -> "C";
default_level(alert) -> "A";
default_level(emergency) -> "M".


%% @private
-spec format_timestamp(integer()) -> list().
format_timestamp(Timestamp) ->
    Milliseconds = (Timestamp rem 1000000) div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = time:seconds_to_datetime(Timestamp div 1000000),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
        [Year, Month, Day, Hour, Minute, Second, Milliseconds])).


%% @private
-spec format_time(integer()) -> list().
format_time(Timestamp) ->
    Milliseconds = (Timestamp rem 1000000) div 1000,
    {_Date, {Hour, Minute, Second}} = time:seconds_to_datetime(Timestamp div 1000000),
    lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B.~3..0B",
        [Hour, Minute, Second, Milliseconds])).