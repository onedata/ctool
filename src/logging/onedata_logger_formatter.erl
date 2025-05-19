%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module covers customized logger formatting,
%%% enabling tailored display of log levels and timestamps.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_logger_formatter).

-include("logging.hrl").

-export([format/2]).

%% one of "M", "A", "C", "E", "W", "N", "I", "D" (see function `level_to_label`)
-type log_level_label() :: string().
% for fields description see: https://www.erlang.org/doc/apps/kernel/logger_formatter.html#t:config/0
-type formatter_config() :: #{
    chars_limit     => pos_integer() | unlimited,
    depth           => pos_integer() | unlimited,
    legacy_header   => boolean(),
    max_size        => pos_integer() | unlimited,
    report_cb       => logger:report_cb(),
    single_line     => boolean(),
    template        => template(),
    time_designator => byte(),
    time_offset     => integer() | [byte()]
}.
-type template() :: [metakey() | {metakey(), template(), template()} | unicode:chardata()].
-type metakey() :: atom() | [atom()].

%%%===================================================================
%%% API
%%%===================================================================


-spec format(logger:log_event(), formatter_config()) -> unicode:chardata().
format(LogEvent = #{level := Level, meta := Meta}, Config) ->
    UpdatedConfig = Config#{
        single_line => false,
        template => customize_template(
            Config,
            Level,
            maps:get(time, Meta)
        )
    },
    logger_formatter:format(LogEvent, UpdatedConfig).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec customize_template(formatter_config(), logger:level(), time:seconds()) -> template().
customize_template(Config, Level, Timestamp) ->
    Template = maps:get(template, Config, []),
    NoDate = maps:get(no_date, Config, false),
    lists:map(
        fun
            (level) ->
                level_to_label(Level);
            (time) ->
                case NoDate of
                    false -> format_timestamp(with_date, Timestamp);
                    true -> format_timestamp(without_date, Timestamp)
                end;
            (Other) ->
                Other
        end,
        Template
    ).


%% @private
-spec level_to_label(logger:level()) -> log_level_label().
level_to_label(debug) -> "D";
level_to_label(info) -> "I";
level_to_label(notice) -> "N";
level_to_label(warning) -> "W";
level_to_label(error) -> "E";
level_to_label(critical) -> "C";
level_to_label(alert) -> "A";
level_to_label(emergency) -> "M".


%% @private
-spec format_timestamp(with_date | without_date, time:seconds()) -> string().
format_timestamp(Option, Timestamp) ->
    Milliseconds = (Timestamp rem 1000000) div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = time:seconds_to_datetime(Timestamp div 1000000),
    case Option of
        with_date ->
            str_utils:format(
                "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                [Year, Month, Day, Hour, Minute, Second, Milliseconds]
            );
        without_date ->
            str_utils:format("~2..0B:~2..0B:~2..0B.~3..0B", [Hour, Minute, Second, Milliseconds])
    end.