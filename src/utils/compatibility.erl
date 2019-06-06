%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles verification of compatibility between Onedata services
%%% and integrity checks of GUI packages.
%%% The checks are performed based on a registry acquired from a compatibility
%%% file (JSON). The registry is cached in memory, although a fresh version
%%% might be fetched when there is a chance that it is outdated - in case an
%%% incompatibility between services or unverified GUI is detected.
%%%
%%% The registry is versioned using the 'revision' field (integer). It should be
%%% incremented upon any modification. Later, it is used to decide if the newly
%%% fetched registry is newer than the local one and should replace it.
%%%
%%% The compatibility registry is serializable to JSON and based on a nested map
%%% with the following structure:
%%% {
%%%     "revision": 1,
%%%
%%%     "compatibility": {
%%%         "onezone:oneprovider": {
%%%             "18.02.0-rc13": [
%%%                 "18.02.0-rc13"
%%%             ],
%%%             "18.02.1": [
%%%                 "18.02.0-rc13",
%%%                 "18.02.1"
%%%             ],
%%%             "19.02.0-beta1": [
%%%                 "18.02.1",
%%%                 "19.02.0-beta1"
%%%             ]
%%%         },
%%%         "oneprovider:oneprovider": {
%%%             "18.02.0-rc13": [
%%%                 "18.02.0-rc13"
%%%             ],
%%%             "18.02.1": [
%%%                 "18.02.0-rc13",
%%%                 "18.02.1"
%%%             ],
%%%             "19.02.0-beta1": [
%%%                 "19.02.0-beta1"
%%%             ]
%%%         },
%%%         "oneprovider:oneclient": {
%%%             "18.02.0-rc13": [
%%%                 "18.02.0-rc13"
%%%             ],
%%%             "18.02.1": [
%%%                 "18.02.0-rc13",
%%%                 "18.02.1"
%%%             ],
%%%             "19.02.0-beta1": [
%%%                 "19.02.0-beta1"
%%%             ]
%%%         }
%%%     },
%%%
%%%     "gui-sha256": {
%%%         "oz-worker": {
%%%             "19.02.0-beta1": [
%%%                 "3a6d28653c347965a2e2e6211849a12799f463c8b4801a56a22f0b48e51cde65"
%%%             ]
%%%         },
%%%         "op-worker": {
%%%             "19.02.0-beta1": [
%%%                 "53cc8692eb0b7c6823c9cd5022b64ba66739efe0637b0e31b134c67c6365cb0c"
%%%             ]
%%%         },
%%%         "onepanel": {
%%%             "19.02.0-beta1": [
%%%                 "8e741fa13536aad50dff4c81a29217fb7437cd5511ac7fcf8f0de05884d5768b"
%%%             ]
%%%         },
%%%         "harvester": {
%%%             "19.02.0-beta1": [
%%%                 "1a29258ad50dff4c1fa18c7fc8e745811a44d576835317fb7fb37cd5a8f0de06"
%%%             ]
%%%         }
%%%     }
%%% }
%%% @end
%%%--------------------------------------------------------------------
-module(compatibility).
-author("Lukasz Opiola").

-include("onedata.hrl").
-include("logging.hrl").
-include("global_definitions.hrl").

% A nested map acquired by parsing the JSON file
-type registry() :: map().
% Section is denoted by a list of nested keys in the registry map
-type section() :: [NestedKey :: binary()].
% Entry can be a product version or GUI hash - there is a list of entries
% specified per every version in the compatibility registry.
-type entry() :: onedata:release_version() | onedata:gui_hash().

-define(REGISTRY_PATH, begin
    {ok, __Path} = application:get_env(?CTOOL_APP_NAME, compatibility_registry_path),
    __Path
end).
-define(REGISTRY_CACHE_TTL, application:get_env(?CTOOL_APP_NAME, compatibility_registry_cache_ttl, 900)). % 15 minutes
-define(REGISTRY_MIRRORS, application:get_env(?CTOOL_APP_NAME, compatibility_registry_mirrors, [])).

-export([check_products_compatibility/4]).
-export([get_compatible_versions/3]).
-export([verify_gui_hash/3]).
-export([clear_registry_cache/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Checks if the two products in given versions are compatible with each other.
%% @end
%%--------------------------------------------------------------------
-spec check_products_compatibility(
    ProductA :: onedata:product(), VersionA :: onedata:release_version(),
    ProductB :: onedata:product(), VersionB :: onedata:release_version()) ->
    true | {false, CompatibleVersions :: [onedata:release_version()]} |
    {error, unknown_version | cannot_parse_registry}.
check_products_compatibility(?ONEZONE, VersionA, ?ONEPROVIDER, VersionB) ->
    check_entry([<<"compatibility">>, <<"onezone:oneprovider">>], VersionA, VersionB);
check_products_compatibility(?ONEPROVIDER, VersionA, ?ONEPROVIDER, VersionB) ->
    check_entry([<<"compatibility">>, <<"oneprovider:oneprovider">>], VersionA, VersionB);
check_products_compatibility(?ONEPROVIDER, VersionA, ?ONECLIENT, VersionB) ->
    check_entry([<<"compatibility">>, <<"oneprovider:oneclient">>], VersionA, VersionB);
check_products_compatibility(_, _, _, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of product versions compatible with another product in given
%% version. The list is always taken from the local registry (no fetch is attempted).
%% @end
%%--------------------------------------------------------------------
-spec get_compatible_versions(
    ProductA :: onedata:product(), VersionA :: onedata:release_version(),
    ProductB :: onedata:product()) ->
    {ok, CompatibleVersions :: [onedata:release_version()]} |
    {error, unknown_version | cannot_parse_registry}.
get_compatible_versions(?ONEZONE, VersionA, ?ONEPROVIDER) ->
    get_entries([<<"compatibility">>, <<"onezone:oneprovider">>], VersionA, local);
get_compatible_versions(?ONEPROVIDER, VersionA, ?ONEPROVIDER) ->
    get_entries([<<"compatibility">>, <<"oneprovider:oneprovider">>], VersionA, local);
get_compatible_versions(?ONEPROVIDER, VersionA, ?ONECLIENT) ->
    get_entries([<<"compatibility">>, <<"oneprovider:oneclient">>], VersionA, local);
get_compatible_versions(_, _, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Checks if the GUI hash is valid for the service in given version.
%% @end
%%--------------------------------------------------------------------
-spec verify_gui_hash(onedata:gui(), onedata:release_version(), onedata:gui_hash()) ->
    true | {false, CorrectHashes :: [onedata:gui_hash()]} |
    {error, unknown_version | cannot_parse_registry}.
verify_gui_hash(?OZ_WORKER_GUI, Version, GuiHash) ->
    check_entry([<<"gui-sha256">>, <<"oz-worker">>], Version, GuiHash);
verify_gui_hash(?OP_WORKER_GUI, Version, GuiHash) ->
    check_entry([<<"gui-sha256">>, <<"op-worker">>], Version, GuiHash);
verify_gui_hash(?ONEPANEL_GUI, Version, GuiHash) ->
    check_entry([<<"gui-sha256">>, <<"onepanel">>], Version, GuiHash);
verify_gui_hash(?HARVESTER_GUI, Version, GuiHash) ->
    check_entry([<<"gui-sha256">>, <<"harvester">>], Version, GuiHash);
verify_gui_hash(_, _, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Instantly clears registry cache - the next check will cause the registry file
%% to be read again from disk and (if required) a fetch attempt will be performed.
%% @end
%%--------------------------------------------------------------------
-spec clear_registry_cache() -> ok.
clear_registry_cache() ->
    simple_cache:clear(compatibility_registry),
    simple_cache:clear(compatibility_registry_fetch_backoff),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if given entry is on the list of all valid entries for given
%% registry section and version.
%% @end
%%--------------------------------------------------------------------
-spec check_entry(section(), onedata:release_version(), entry()) ->
    true | {false, ValidEntries :: [entry()]} |
    {error, unknown_version | cannot_parse_registry}.
check_entry(Section, Version, Entry) ->
    case check_entry(Section, Version, Entry, local) of
        true ->
            true;
        FailureResult ->
            case should_fetch_registry() of
                true ->
                    case check_entry(Section, Version, Entry, fetch) of
                        {error, cannot_fetch_registry} ->
                            FailureResult;
                        Result ->
                            Result
                    end;
                false ->
                    FailureResult
            end
    end.


%% @private
-spec check_entry(section(), onedata:release_version(), entry(), Strategy :: local | fetch) ->
    true | {false, ValidEntries :: [entry()]} |
    {error, unknown_version | cannot_parse_registry | cannot_fetch_registry}.
check_entry(Section, Version, Entry, Strategy) ->
    case get_entries(Section, Version, Strategy) of
        {error, _} = Error ->
            Error;
        {ok, Entries} ->
            case lists:member(Entry, Entries) of
                true -> true;
                false -> {false, Entries}
            end
    end.


%% @private
-spec get_entries(section(), onedata:release_version(), Strategy :: local | fetch) ->
    {ok, [entry()]} |
    {error, unknown_version | cannot_parse_registry | cannot_fetch_registry}.
get_entries(Section, Version, Strategy) ->
    case get_registry(Strategy) of
        {error, _} = Error ->
            Error;
        {ok, Registry} ->
            AllVersions = get_section(Section, Registry),
            case maps:find(Version, AllVersions) of
                error ->
                    {error, unknown_version};
                {ok, Entries} ->
                    {ok, Entries}
            end
    end.


%% @private
-spec should_fetch_registry() -> boolean().
should_fetch_registry() ->
    Now = time_utils:system_time_seconds(),
    case simple_cache:get(compatibility_registry_fetch_backoff) of
        {ok, BackoffUntil} -> BackoffUntil < Now;
        {error, not_found} -> true
    end.


%% @private
-spec reset_fetch_backoff() -> ok.
reset_fetch_backoff() ->
    simple_cache:put(
        compatibility_registry_fetch_backoff,
        time_utils:system_time_seconds() + ?REGISTRY_CACHE_TTL
    ).


%% @private
-spec get_registry(Strategy :: local | fetch) ->
    {ok, registry()} | {error, cannot_parse_registry | cannot_fetch_registry}.
get_registry(local) ->
    simple_cache:get(compatibility_registry, fun() ->
        case file:read_file(?REGISTRY_PATH) of
            {ok, Binary} ->
                case parse_registry(Binary) of
                    {error, cannot_parse_registry} ->
                        {error, cannot_parse_registry};
                    {ok, Registry} ->
                        {true, Registry, timer:seconds(?REGISTRY_CACHE_TTL)}
                end;
            Other ->
                ?error("Cannot parse compatibility registry (~s) due to ~w", [
                    ?REGISTRY_PATH, Other
                ]),
                {error, cannot_parse_registry}
        end
    end);
get_registry(fetch) ->
    reset_fetch_backoff(),
    Mirrors = ?REGISTRY_MIRRORS,
    case fetch_registry(Mirrors) of
        {ok, Binary, Registry, Mirror} ->
            try
                maybe_overwrite_registry(Binary, Registry, Mirror)
            catch Type:Reason ->
                ?error_stacktrace("Error processing newly fetched compatibility registry - ~p:~p, mirror: ~s", [
                    Type, Reason, Mirror
                ]),
                {error, cannot_fetch_registry}
            end;
        {error, cannot_fetch_registry} ->
            ?warning("Cannot fetch compatibility registry, tried mirrors:~n~s", [
                str_utils:join_binary(
                    [str_utils:to_binary(M) || M <- Mirrors],
                    <<"\n">>
                )
            ]),
            {error, cannot_fetch_registry}
    end.


%% @private
-spec fetch_registry([Mirror :: http_client:url()]) ->
    {ok, Binary :: binary(), registry(), Mirror :: http_client:url()} |
    {error, cannot_fetch_registry}.
fetch_registry([Mirror | Rest]) ->
    case http_client:get(Mirror) of
        {ok, 200, _, Binary} ->
            case parse_registry(Binary) of
                {error, cannot_parse_registry} ->
                    ?debug("Cannot parse registry from mirror ~s", [Mirror]),
                    fetch_registry(Rest);
                {ok, Registry} ->
                    {ok, Binary, Registry, Mirror}
            end;
        Other ->
            ?debug("Cannot fetch compatibility registry from mirror ~s - ~p", [
                Mirror, Other
            ]),
            fetch_registry(Rest)
    end;
fetch_registry([]) ->
    {error, cannot_fetch_registry}.


%% @private
-spec maybe_overwrite_registry(Binary :: binary(), registry(), Mirror :: http_client:url()) ->
    {ok, registry()}.
maybe_overwrite_registry(Binary, Registry, Mirror) ->
    {ok, LocalRegistry} = get_registry(local),
    case revision(Registry) > revision(LocalRegistry) of
        true ->
            RegistryPath = ?REGISTRY_PATH,
            ?info(
                "Fetched a new compatibility registry from mirror ~s "
                "(rev. ~B), overwriting the old one at ~s",
                [Mirror, revision(Registry), RegistryPath]
            ),
            ok = file:write_file(RegistryPath, Binary),
            clear_registry_cache(),
            {ok, Registry};
        false ->
            ?debug(
                "Ignoring compatibility registry fetched from mirror ~s "
                "- revision (~B) not newer than local (~B)",
                [Mirror, revision(Registry), revision(LocalRegistry)]
            ),
            {ok, LocalRegistry}
    end.


%% @private
-spec parse_registry(Binary :: binary()) -> {ok, registry()} | {error, cannot_parse_registry}.
parse_registry(Binary) ->
    try
        Registry = json_utils:decode(Binary),
        true = revision(Registry) > 0,

        %% In case of compatibility between providers, the relation is symmetrical,
        %% but the registry file might not have symmetrical entries - they are
        %% coalesced here.
        CompatibilitySection = maps:get(<<"compatibility">>, Registry, #{}),
        OPvsOPSection = maps:get(<<"oneprovider:oneprovider">>, CompatibilitySection, #{}),

        {ok, Registry#{
            <<"compatibility">> => CompatibilitySection#{
                <<"oneprovider:oneprovider">> => maps:fold(fun(VersionA, CompatibleVersions, OuterAcc) ->
                    lists:foldl(fun(VersionB, InnerAcc) ->
                        InnerAcc#{
                            VersionB => lists:usort([VersionA | maps:get(VersionB, InnerAcc, [])])
                        }
                    end, OuterAcc, CompatibleVersions)
                end, OPvsOPSection, OPvsOPSection)
            }
        }}
    catch
        Type:Reason ->
            ?debug_stacktrace("Cannot parse compatibility registry due to ~p:~p", [
                Type, Reason
            ]),
            {error, cannot_parse_registry}
    end.


%% @private
-spec get_section(section(), registry()) -> map().
get_section([], Map) ->
    Map;
get_section([Key | Rest], Map) ->
    get_section(Rest, maps:get(Key, Map, #{})).


%% @private
-spec revision(registry()) -> pos_integer().
revision(#{<<"revision">> := Revision}) ->
    Revision.
