%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines customized logger filters.
%%% Each filter requires two arguments: log_event() and filter_arg().
%%%
%%% - log_event() represents the log event data.
%%% - filter_arg() specifies the filter's behavior, typically log or stop.
%%%
%%% Filters can be used to include or exclude log events based on
%%% specific conditions.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_logger_filters).

-include("onedata.hrl").

-export([select_self_logs/2, file_access_audit_log_filter/2]).

%%%===================================================================
%%% API
%%%===================================================================


-spec select_self_logs(logger:log_event(), stop) -> logger:filter_return().
select_self_logs(LogEvent, stop) ->
    Metadata = maps:get(meta, LogEvent),
    Pid = maps:get(pid, Metadata),
    case self() of
        Pid -> LogEvent;
        _ -> stop
    end.


-spec file_access_audit_log_filter(logger:log_event(), stop | log) -> logger:filter_return().
file_access_audit_log_filter(LogEvent, log) ->
    Metadata = maps:get(meta, LogEvent),

    EnableFileAccessAuditLog = maps:get(enable_file_access_audit_log, Metadata, false),
    case EnableFileAccessAuditLog of
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
