%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of GEO DB module.
%%% @end
%%%--------------------------------------------------------------------
-module(geo_db_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TIME_MOCK_STARTING_TIMESTAMP, 1500000000).
% Dummy query result that depends on timestamp (to verify if db is refreshed)
-define(QUERY_RESULT(Timestamp), #{<<"dummy-result">> => Timestamp}).
-define(DUMMY_DB_PATH(Dir, DbType), filename:join([Dir, atom_to_list(DbType) ++ "-geo-db.tar.gz"])).
-define(DUMMY_DB_MIRROR(DbType), <<"https://example.com/dl?edition_id=", (atom_to_binary(DbType, utf8))/binary>>).
-define(DUMMY_STATUS_FILE(Dir), filename:join([Dir, "geo-db-status.json"])).
-define(DUMMY_LICENSE_KEY, <<"LiCeNsE12345">>).

%%%===================================================================
%%% Test cases
%%%===================================================================

-define(TEST_CASES, [
    {"Lookup GEO DB", fun lookup_geo_db/1},
    {"Refresh GEO DB", fun refresh_geo_db/1},
    {"Absent or bad MaxMind licence key", fun absent_or_bad_maxmind_licence_key/1},
    {"Bad GEO DB path", fun bad_db_path/1},
    {"Bad GEO DB mirror", fun bad_db_mirror/1}
]).

geo_db_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        lists:map(fun({Desc, TestFun}) ->
            fun(Ctx) ->
                {Desc, ?_test(TestFun(Ctx))}
            end
        end, ?TEST_CASES)
    }.

%%%===================================================================
%%% Test fixtures and helper functions
%%%===================================================================

setup() ->
    node_cache:init(),
    meck:new(locus),
    meck:expect(locus, lookup, fun(DbType, _IP) ->
        case mock_is_db_loaded(DbType) of
            {true, Timestamp} -> {ok, ?QUERY_RESULT(Timestamp)};
            false -> {error, database_not_loaded}
        end
    end),
    meck:expect(locus, start_loader, fun(DbType, DbPath) ->
        case file:read_file(DbPath) of
            {ok, Data} ->
                mock_load_db(DbType, binary_to_integer(Data));
            _ ->
                {error, cannot_load_db}

        end
    end),
    meck:expect(locus, wait_for_loader, fun(_DbType, _) ->
        ok
    end),
    meck:expect(locus, stop_loader, fun(DbType) ->
        mock_unload_db(DbType)
    end),
    meck:expect(locus, get_info, fun(DbType) ->
        case mock_is_db_loaded(DbType) of
            {true, Timestamp} -> {ok, Timestamp};
            false -> {error, database_unknown}
        end
    end),

    meck:new(time_utils, []),
    meck:expect(time_utils, timestamp_seconds, fun() ->
        get_mocked_time()
    end),
    meck:expect(time_utils, timestamp_millis, fun() ->
        get_mocked_time() * 1000
    end),
    
    meck:new(node_cache, [passthrough]),
    meck:expect(node_cache, now, fun() ->
        get_mocked_time() * 1000
    end),

    TmpDir = mochitemp:mkdtemp(),

    ctool:set_env(maxmind_licence_key, ?DUMMY_LICENSE_KEY),
    ctool:set_env(geo_db_refresh_period_days, 3),
    ctool:set_env(geo_db_refresh_backoff_secs, 3600),
    % Dummy DB files hold the timestamp of their creation which is later
    % returned in dummy query results
    ctool:set_env(geo_db_path, #{
        asn =>     ?DUMMY_DB_PATH(TmpDir, asn),
        country => ?DUMMY_DB_PATH(TmpDir, country)
    }),
    file:write_file(?DUMMY_DB_PATH(TmpDir, asn), integer_to_binary(get_mocked_time())),
    file:write_file(?DUMMY_DB_PATH(TmpDir, country), integer_to_binary(get_mocked_time())),

    ctool:set_env(geo_db_mirror, #{
        asn =>     ?DUMMY_DB_MIRROR(asn),
        country => ?DUMMY_DB_MIRROR(country)
    }),
    meck:new(http_client, []),
    meck:expect(http_client, get, fun(Url) ->
        AnsUrl = str_utils:format_bin("~s&license_key=~s", [?DUMMY_DB_MIRROR(asn), ?DUMMY_LICENSE_KEY]),
        CountryUrl = str_utils:format_bin("~s&license_key=~s", [?DUMMY_DB_MIRROR(country), ?DUMMY_LICENSE_KEY]),
        case Url of
            AnsUrl -> {ok, 200, #{}, integer_to_binary(get_mocked_time())};
            CountryUrl -> {ok, 200, #{}, integer_to_binary(get_mocked_time())};
            _ -> {ok, 404, #{}, <<>>}
        end
    end),

    ctool:set_env(geo_db_status_path, ?DUMMY_STATUS_FILE(TmpDir)),
    file:write_file(?DUMMY_STATUS_FILE(TmpDir), json_utils:encode(#{
        <<"asn">> => #{
            <<"last-refresh">> => get_mocked_time(),
            <<"last-refresh-attempt">> => get_mocked_time()
        },
        <<"country">> => #{
            <<"last-refresh">> => get_mocked_time(),
            <<"last-refresh-attempt">> => get_mocked_time()
        }
    })),

    % Clean loaded DB cache
    node_cache:clear({db_loaded, asn}),
    node_cache:clear({db_loaded, country}),
    mock_unload_db(asn),
    mock_unload_db(country),

    #{tmpDir => TmpDir}.


teardown(#{tmpDir := TmpDir}) ->
    node_cache:destroy(),
    ?assert(meck:validate(locus)),
    ok = meck:unload(locus),
    ?assert(meck:validate(time_utils)),
    ok = meck:unload(time_utils),
    ?assert(meck:validate(node_cache)),
    ok = meck:unload(node_cache),
    ?assert(meck:validate(http_client)),
    ok = meck:unload(http_client),

    mochitemp:rmtempdir(TmpDir).


get_mocked_time() ->
    ctool:get_env(mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP).

simulate_time_passing(Seconds) ->
    ctool:set_env(mocked_time, get_mocked_time() + Seconds).

-define(DUMMY_ENV(DbType),
    binary_to_atom(str_utils:format_bin("db_loaded_~p", [DbType]), utf8)
).
mock_is_db_loaded(DbType) ->
    % {true, Timestamp} | false
    ctool:get_env(?DUMMY_ENV(DbType), false).

mock_load_db(DbType, Timestamp) ->
    ctool:set_env(?DUMMY_ENV(DbType), {true, Timestamp}).

mock_unload_db(DbType) ->
    ctool:set_env(?DUMMY_ENV(DbType), false).

%%%===================================================================
%%% Test functions
%%%===================================================================

lookup_geo_db(_) ->
    TimeAlpha = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    ?assertError(function_clause, geo_db:lookup(wat, "66.6.7.8")).


refresh_geo_db(#{tmpDir := TmpDir}) ->
    TimeAlpha = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    RefreshPeriod = 86400 * ctool:get_env(geo_db_refresh_period_days),

    simulate_time_passing(314),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    % After the refresh period, newer GEO DB should be fetched
    % (which will cause dummy query results to include a newer timestamp)
    simulate_time_passing(RefreshPeriod + 1 - 314),
    TimeBeta = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeBeta)}, geo_db:lookup(asn, "5.6.7.8")),

    % ASN and country DBs are refreshed separately
    simulate_time_passing(13),
    TimeGamma = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeGamma)}, geo_db:lookup(country, "9.6.7.8")),

    % Make sure the status file is updated accordingly
    ExpStatus = json_utils:encode(#{
        <<"asn">> => #{
            <<"last-refresh">> => TimeBeta,
            <<"last-refresh-attempt">> => TimeBeta
        },
        <<"country">> => #{
            <<"last-refresh">> => TimeGamma,
            <<"last-refresh-attempt">> => TimeGamma
        }
    }),
    ?assertEqual({ok, ExpStatus}, file:read_file(?DUMMY_STATUS_FILE(TmpDir))),

    simulate_time_passing(RefreshPeriod + 1),
    TimeOmega = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeOmega)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeOmega)}, geo_db:lookup(country, "9.6.7.8")),
    ExpStatusOmega = json_utils:encode(#{
        <<"asn">> => #{
            <<"last-refresh">> => TimeOmega,
            <<"last-refresh-attempt">> => TimeOmega
        },
        <<"country">> => #{
            <<"last-refresh">> => TimeOmega,
            <<"last-refresh-attempt">> => TimeOmega
        }
    }),
    ?assertEqual({ok, ExpStatusOmega}, file:read_file(?DUMMY_STATUS_FILE(TmpDir))).


absent_or_bad_maxmind_licence_key(_) ->
    TimeAlpha = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    % Databases should not be updated if license key is not given
    ctool:set_env(maxmind_licence_key, undefined),
    RefreshPeriod = 86400 * ctool:get_env(geo_db_refresh_period_days),
    simulate_time_passing(RefreshPeriod + 1),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    % Databases should not be updated if a bad license key is given
    ctool:set_env(maxmind_licence_key, <<"bad-key">>),
    RefreshBackoff = ctool:get_env(geo_db_refresh_backoff_secs),
    simulate_time_passing(RefreshBackoff + 1),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    % Set a correct license key - after the backoff, the databases should be updated
    ctool:set_env(maxmind_licence_key, ?DUMMY_LICENSE_KEY),
    simulate_time_passing(RefreshBackoff + 1),
    TimeBeta = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeBeta)}, geo_db:lookup(asn, "5.6.7.8")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeBeta)}, geo_db:lookup(country, "9.6.7.8")).


bad_db_path(#{tmpDir := TmpDir}) ->
    TimeAlpha = get_mocked_time(),

    ctool:set_env(geo_db_path, #{
        asn =>     "/a/b/c/d/e/asn",
        country => "/a/b/c/d/e/country"
    }),
    ?assertEqual({error, database_not_loaded}, geo_db:lookup(asn, "1.2.3.4")),
    ?assertEqual({error, database_not_loaded}, geo_db:lookup(country, "1.2.3.5")),

    % Even if correct paths are set, next load attempts should be backed off
    ctool:set_env(geo_db_path, #{
        asn =>     ?DUMMY_DB_PATH(TmpDir, asn),
        country => ?DUMMY_DB_PATH(TmpDir, country)
    }),
    ?assertEqual({error, database_not_loaded}, geo_db:lookup(asn, "1.2.3.4")),
    ?assertEqual({error, database_not_loaded}, geo_db:lookup(country, "1.2.3.5")),

    % Wait until the backoff passes
    simulate_time_passing(301), % Not configurable, see ?LOAD_ATTEMPT_BACKOFF in geo_db
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "1.2.3.4")),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "1.2.3.5")).


bad_db_mirror(#{tmpDir := TmpDir}) ->
    TimeAlpha = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),

    simulate_time_passing(7),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    ctool:set_env(geo_db_mirror, #{
        asn =>     "https://a.b/c/d/e/asn",
        country => "https://a.b/c/d/e/country"
    }),

    RefreshPeriod = 86400 * ctool:get_env(geo_db_refresh_period_days),
    simulate_time_passing(RefreshPeriod + 1),
    TimeGamma = get_mocked_time(),
    % Refresh should fail, causing the old DB to be still used
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),

    simulate_time_passing(9),
    TimeDelta = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    % Make sure the status file is updated accordingly
    ExpStatus = json_utils:encode(#{
        <<"asn">> => #{
            <<"last-refresh">> => TimeAlpha,
            <<"last-refresh-attempt">> => TimeGamma
        },
        <<"country">> => #{
            <<"last-refresh">> => TimeAlpha,
            <<"last-refresh-attempt">> => TimeDelta
        }
    }),
    ?assertEqual({ok, ExpStatus}, file:read_file(?DUMMY_STATUS_FILE(TmpDir))),

    % Even if correct mirrors are set, next refresh attempts should be backed off
    ctool:set_env(geo_db_mirror, #{
        asn =>     ?DUMMY_DB_MIRROR(asn),
        country => ?DUMMY_DB_MIRROR(country)
    }),
    simulate_time_passing(17),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(asn, "5.6.7.8")),
    simulate_time_passing(4),
    ?assertEqual({ok, ?QUERY_RESULT(TimeAlpha)}, geo_db:lookup(country, "9.6.7.8")),

    % Wait until the backoff passes
    RefreshBackoff = ctool:get_env(geo_db_refresh_backoff_secs),
    simulate_time_passing(RefreshBackoff + 1 - 17 - 4),
    TimeTheta = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeTheta)}, geo_db:lookup(asn, "5.6.7.8")),

    simulate_time_passing(19),
    TimeOmega = get_mocked_time(),
    ?assertEqual({ok, ?QUERY_RESULT(TimeOmega)}, geo_db:lookup(country, "9.6.7.8")),

    ExpStatusOmega = json_utils:encode(#{
        <<"asn">> => #{
            <<"last-refresh">> => TimeTheta,
            <<"last-refresh-attempt">> => TimeTheta
        },
        <<"country">> => #{
            <<"last-refresh">> => TimeOmega,
            <<"last-refresh-attempt">> => TimeOmega
        }
    }),
    ?assertEqual({ok, ExpStatusOmega}, file:read_file(?DUMMY_STATUS_FILE(TmpDir))).

-endif.