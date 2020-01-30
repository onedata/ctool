%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides an API for geolocation queries.
%%% It uses the locus library, which consumes MaxMind's GeoLite2 databases.
%%% The module encapsulates the db file management and refreshing (fetching
%%% a newer version periodically).
%%% @end
%%%-------------------------------------------------------------------
-module(geo_db).
-author("Lukasz Opiola").

-include("logging.hrl").

-type lookup_error() :: not_found | invalid_address | database_unknown | database_not_loaded | ipv4_database.
-export_type([lookup_error/0]).

-type db_type() :: asn | country.
-type query_result() :: locus:database_entry().
-type last_refresh() :: non_neg_integer().
-type last_refresh_attempt() :: non_neg_integer().

% How often a DB validity check is performed
-define(VALIDITY_CHECK_INTERVAL, 3600000). % 1 hour
% How often a DB load attempt is retried upon failure
-define(LOAD_ATTEMPT_BACKOFF, 300000). % 5 minutes
% Critical section (per node and db type) to avoid race conditions when
% reading / refreshing the database.
-define(CRITICAL_SECTION(DbType, Fun), global:trans({{geo_db, DbType, node()}, self()}, Fun)).
-define(NOW(), time_utils:system_time_seconds()).
% Configurable env variables
-define(MAXMIND_LICENCE_KEY, ctool:get_env(maxmind_licence_key, undefined)).
-define(GEO_DB_PATH(DbType), maps:get(DbType, ctool:get_env(geo_db_path))).
-define(GEO_DB_MIRROR(DbType), maps:get(DbType, ctool:get_env(geo_db_mirror))).
-define(GEO_DB_STATUS_FILE, ctool:get_env(geo_db_status_path)).
-define(GEO_DB_REFRESH_PERIOD_SEC, ctool:get_env(geo_db_refresh_period_days) * 86400).
-define(GEO_DB_REFRESH_BACKOFF_SEC, ctool:get_env(geo_db_refresh_backoff_secs)).

%% API
-export([lookup/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Performs a GEO DB lookup, making sure that the DB is loaded and up to date.
%% @end
%%--------------------------------------------------------------------
-spec lookup(db_type(), ip_utils:ip()) -> {ok, query_result()} | {error, lookup_error()}.
lookup(DbType, IP) when DbType == asn orelse DbType == country ->
    case ensure_db_loaded(DbType) of
        true -> locus:lookup(DbType, IP);
        false -> {error, database_not_loaded}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_db_loaded(db_type()) -> boolean().
ensure_db_loaded(DbType) ->
    % Called not more frequently than every ?VALIDITY_CHECK_INTERVAL seconds
    % and handled in a critical section (per cluster node) to avoid race
    % conditions when reading / refreshing the database.
    % In case of failures, ?LOAD_ATTEMPT_BACKOFF applies.
    EnsureDb = fun() ->
        ?CRITICAL_SECTION(DbType, fun() ->
            try ensure_db_loaded_unsafe(DbType) of
                ok ->
                    {true, loaded, ?VALIDITY_CHECK_INTERVAL};
                {error, _} = Error ->
                    ?error(
                        "GEO DB could not be loaded due to ~w, next attempt in ~B seconds",
                        [Error, ?LOAD_ATTEMPT_BACKOFF div 1000]
                    ),
                    {true, not_loaded, ?LOAD_ATTEMPT_BACKOFF}
            catch Type:Reason ->
                ?error_stacktrace(
                    "Unexpected error when loading GEO DB (~p) - ~p:~p, next attempt in ~B seconds",
                    [DbType, Type, Reason, ?LOAD_ATTEMPT_BACKOFF div 1000]
                ),
                {true, not_loaded, ?LOAD_ATTEMPT_BACKOFF}
            end
        end)
    end,
    case simple_cache:get({db_loaded, DbType}, EnsureDb) of
        {ok, loaded} -> true;
        {ok, not_loaded} -> false
    end.


%% Unsafe - must be called in a critical section.
-spec ensure_db_loaded_unsafe(db_type()) -> ok | {error, term()}.
ensure_db_loaded_unsafe(DbType) ->
    case maybe_fetch_newer_db(DbType) of
        true -> locus:stop_loader(DbType); % Forces reload
        false -> ok
    end,
    case locus:get_info(DbType) of
        {ok, _} -> ok;
        {error, database_unknown} -> load_db(DbType);
        {error, _} = Error -> Error
    end.


-spec load_db(db_type()) -> ok | {error, term()}.
load_db(DbType) ->
    DbPath = ?GEO_DB_PATH(DbType),
    case filelib:is_file(DbPath) of
        false ->
            {error, {no_such_file, DbPath}};
        true ->
            locus:start_loader(DbType, DbPath),
            locus:wait_for_loader(DbType, 2000),
            case locus:get_info(DbType) of
                {ok, _} ->
                    ?debug("Successfully loaded ~p GEO DB (~s)", [DbType, DbPath]),
                    ok;
                {error, _} = Error ->
                    Error
            end
    end.


-spec maybe_fetch_newer_db(db_type()) -> boolean().
maybe_fetch_newer_db(DbType) ->
    case should_fetch_newer_db(DbType) of
        false -> false;
        true -> fetch_newer_db(DbType)
    end.


-spec should_fetch_newer_db(db_type()) -> boolean().
should_fetch_newer_db(DbType) ->
    Now = ?NOW(),
    RefreshPeriod = ?GEO_DB_REFRESH_PERIOD_SEC,
    Backoff = ?GEO_DB_REFRESH_BACKOFF_SEC,
    case read_status(DbType) of
        {LastRefresh, _} when Now - LastRefresh =< RefreshPeriod -> false;
        {_, LastAttempt} when Now - LastAttempt =< Backoff -> false;
        _ -> true
    end.


-spec fetch_newer_db(db_type()) -> boolean().
fetch_newer_db(DbType) ->
    case ?MAXMIND_LICENCE_KEY of
        undefined ->
            ?warning(
                "No MaxMind licence key found - skipping ~p GEO DB refresh. Next retry in ~B sec.",
                [DbType, ?GEO_DB_REFRESH_BACKOFF_SEC]
            ),
            write_status(DbType, keep, ?NOW()),
            false;
        LicenceKey ->
            fetch_newer_db(DbType, str_utils:to_binary(LicenceKey))
    end.

fetch_newer_db(DbType, LicenceKey) ->
    Now = ?NOW(),
    Mirror = str_utils:to_binary(?GEO_DB_MIRROR(DbType)),
    Url = http_utils:append_url_parameters(Mirror, #{<<"license_key">> => LicenceKey}),
    case http_client:get(Url) of
        {ok, 200, _, Data} ->
            file:write_file(?GEO_DB_PATH(DbType), Data),
            write_status(DbType, Now, Now),
            ?info("Fetched a newer ~p GEO DB (~s)", [DbType, Mirror]),
            true;
        OtherResult ->
            ?warning(
                "Cannot fetch newer ~p GEO DB (~s), next retry in ~B sec~nresult was: ~p",
                [DbType, Mirror, ?GEO_DB_REFRESH_BACKOFF_SEC, OtherResult]
            ),
            write_status(DbType, keep, Now),
            false
    end.


-spec read_status(db_type()) -> {last_refresh(), last_refresh_attempt()}.
read_status(DbType) ->
    DbStatus = maps:get(atom_to_binary(DbType, utf8), read_status_file(), #{}),
    LastRefresh = maps:get(<<"last-refresh">>, DbStatus, 0),
    LastRefreshAttempt = maps:get(<<"last-refresh-attempt">>, DbStatus, 0),
    {LastRefresh, LastRefreshAttempt}.


-spec write_status(db_type(), keep | last_refresh(), last_refresh_attempt()) -> ok.
write_status(DbType, LastRefresh, LastRefreshAttempt) ->
    JSON = read_status_file(),
    write_status_file(JSON#{atom_to_binary(DbType, utf8) => #{
        <<"last-refresh">> => case LastRefresh of
            keep ->
                DbStatus = maps:get(atom_to_binary(DbType, utf8), JSON, #{}),
                maps:get(<<"last-refresh">>, DbStatus, 0);
            _ ->
                LastRefresh
        end,
        <<"last-refresh-attempt">> => LastRefreshAttempt
    }}).


-spec read_status_file() -> json_utils:json_term().
read_status_file() ->
    {ok, Data} = file:read_file(?GEO_DB_STATUS_FILE),
    json_utils:decode(Data).


-spec write_status_file(json_utils:json_term()) -> ok.
write_status_file(JSON) ->
    ok = file:write_file(?GEO_DB_STATUS_FILE, json_utils:encode(JSON)).
