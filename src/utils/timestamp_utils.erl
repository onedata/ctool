%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Module with timestamp converting functions.
%%% @end
%%%--------------------------------------------------------------------
-module(timestamp_utils).
-author("Tomasz Lichon").

%% API
-export([datetime_to_datestamp/1, datestamp_to_datetime/1]).

%% Macro with regex matching allowed datestamps
%%  * YYYY-MM-DDThh:mm:ssZ
%%  * YYYY-MM-DD
-define(DATESTAMP_REGEX,

    "(\\d{4})-(\\d{2})-(\\d{2})(?:$|(?:T(\\d{2}):(\\d{2}):(\\d{2})(?:.\\d{3})?Z){1})").
%%%===================================================================
%%% API
%%%===================================================================

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