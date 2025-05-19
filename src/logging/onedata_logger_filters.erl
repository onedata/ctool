%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines customized logger filters.
%%%
%%% Filters can be used to include or exclude log events based on
%%% specific conditions.
%%%
%%% Each filter requires two arguments:
%%%   - logger:log_event() represents the log event data.
%%%   - logger:filter_arg() specifies the filter's behavior, typically `log` or `stop`.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_logger_filters).

-include("onedata.hrl").

-export([select_self_logs/2, file_access_audit_log_filter/2]).

%%%===================================================================
%%% API
%%%===================================================================


-spec select_self_logs(logger:log_event(), stop) -> logger:filter_return().
select_self_logs(#{meta := #{pid := Pid}} = LogEvent, stop) ->
    case self() of
        Pid -> LogEvent;
        _ -> stop % fixme remove??
    end.


% fixme w onedata_logger dodać do każdego loga kontekst ze to onedata logger jest i ignorować wszystkie inne logi podstawowych handlerach
% fixme natomiast w op zrobić nowy handler na to i tam na kontekst reagować
-spec file_access_audit_log_filter(logger:log_event(), stop | log) -> logger:filter_return().
file_access_audit_log_filter(LogEvent, log) ->
    Metadata = maps:get(meta, LogEvent),

    case maps:get(enable_file_access_audit_log, Metadata, false) of
        true -> LogEvent;
        false -> stop
    end;
file_access_audit_log_filter(LogEvent, stop) ->
    Metadata = maps:get(meta, LogEvent),
    EnableFileAccessAuditLog = maps:get(enable_file_access_audit_log, Metadata, false),
    case EnableFileAccessAuditLog of
        true -> stop;
        false -> LogEvent
    end.
