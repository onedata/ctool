%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Module with timestamp converting functions.
%%% TODO use iso8601 module in all functions in this module VFS-3825
%%% @end
%%%--------------------------------------------------------------------
-module(timestamp_utils).
-author("Tomasz Lichon").

%% API
-export([datetime_to_datestamp/1, datestamp_to_datetime/1, epoch_to_iso8601/1,
    datetime_to_epoch/1, iso8601_to_epoch/1]).

%% Macro with regex matching allowed datestamps
%%  * YYYY-MM-DDThh:mm:ssZ
%%  * YYYY-MM-DD
-define(DATESTAMP_REGEX,
    "(\\d{4})-(\\d{2})-(\\d{2})(?:$|(?:T(\\d{2}):(\\d{2}):(\\d{2})(?:.\\d{3})?Z){1})").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convert unix epoch to iso8601 format.
%% @end
%%--------------------------------------------------------------------
-spec epoch_to_iso8601(Epoch :: non_neg_integer()) -> binary().
epoch_to_iso8601(Epoch) ->
    iso8601:format({Epoch div 1000000, Epoch rem 1000000, 0}).

%%--------------------------------------------------------------------
%% @doc
%% Convert iso8601 format to unix epoch time.
%% @end
%%--------------------------------------------------------------------
-spec iso8601_to_epoch(binary()) -> non_neg_integer().
iso8601_to_epoch(Iso8601) ->
    DateTime = datestamp_to_datetime(Iso8601),
    datetime_to_epoch(DateTime).

%%-------------------------------------------------------------------
%% @doc
%% Converts erlang datetime to unix epoch time.
%% @end
%%-------------------------------------------------------------------
-spec datetime_to_epoch(calendar:datetime()) -> non_neg_integer().
datetime_to_epoch({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

%%%--------------------------------------------------------------------
%%% @doc
%%% Converts DateTime to format accepted by OAI-PMH which is in form
%%  YYYY-MM-DDThh:mm:ssZ
%%% @end
%%%--------------------------------------------------------------------
-spec datetime_to_datestamp(DateTime :: erlang:datetime()) -> binary().
datetime_to_datestamp(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    str_utils:format_bin(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Minute, Second]).

%%%--------------------------------------------------------------------
%%% @doc
%%% Converts datestamp from format defined by OAI-PMH to
%%% erlang:datetime() or erlang:date().
%%% Converts:
%%%     * YYYY-MM-DDT:hh:mm:ssZ to {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%%     * YYYY-MM-DD to {Year, Month, Day}
%%% @end
%%%--------------------------------------------------------------------
-spec datestamp_to_datetime(undefined | binary()) ->
    calendar:datetime() | calendar:date() | undefined | {error, invalid_date_format}.
datestamp_to_datetime(undefined) -> undefined;
datestamp_to_datetime(Datestamp) ->
    {ok, Regex} = re:compile(?DATESTAMP_REGEX),
    case re:run(Datestamp, Regex, [{capture, all_but_first, list}]) of
        {match, Matched} ->
            case [list_to_integer(E) || E <- Matched] of
                [Y, M, D, H, Min, S] ->
                    {{Y, M, D}, {H, Min, S}};
                [Y, M, D] ->
                    {Y, M, D};
                _ -> {error, invalid_date_format}
            end;
        nomatch -> {error, invalid_date_format}
    end.