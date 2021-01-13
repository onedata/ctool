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
%%%     "revision": 2019060700,
%%%
%%%     "compatibility": {
%%%         "onezone:oneprovider": {
%%%             "18.02.0-rc13": ["18.02.0-rc13"],
%%%             "18.02.1": ["18.02.0-rc13", "18.02.1", "18.02.2"],
%%%             "18.02.2": ["18.02.0-rc13", "18.02.1", "18.02.2"],
%%%             "19.02.0-beta1": ["18.02.1", "18.02.2", "19.02.0-beta1"]
%%%         },
%%%         "oneprovider:oneprovider": {
%%%             "18.02.0-rc13": ["18.02.0-rc13"],
%%%             "18.02.1": ["18.02.0-rc13", "18.02.1"],
%%%             "18.02.2": ["18.02.0-rc13", "18.02.1", "18.02.2"],
%%%             "19.02.0-beta1": ["19.02.0-beta1"]
%%%         },
%%%         "oneprovider:oneclient": {
%%%             "18.02.0-rc13": ["18.02.0-rc13"],
%%%             "18.02.1": ["18.02.0-rc13", "18.02.1", "18.02.2"],
%%%             "18.02.2": ["18.02.0-rc13", "18.02.1", "18.02.2"],
%%%             "19.02.0-beta1": ["19.02.0-beta1"]
%%%         }
%%%     },
%%%
%%%     "gui-sha256": {
%%%         "op-worker": {
%%%             "19.02.0-beta1": ["bd47689bd7ef220d73ef4a61672b9f43dc60ae5815ce2aa5f2c8673f3eaafc85"]
%%%         },
%%%         "onepanel": {
%%%             "19.02.0-beta1": ["c5f9a1009588f3ae1407dc978b0c057213dd3753abb9d224acdbfc209ffadadd"]
%%%         },
%%%         "harvester": {
%%%             "19.02.0-beta1": {
%%%                 "ecrin": ["1a29258ad50dff4c1fa18c7fc8e745811a44d576835317fb7fb37cd5a8f0de06"]
%%%             }
%%%         }
%%%     }
%%% }
%%% @end
%%%--------------------------------------------------------------------
-module(compatibility).
-author("Lukasz Opiola").

-include("onedata.hrl").
-include("logging.hrl").

-export([check_products_compatibility/4]).
-export([get_compatible_versions/3]).
-export([verify_gui_hash/3]).
-export([check_for_updates/2]).
-export([peek_current_registry_revision/0]).
-export([clear_registry_cache/0]).

% A nested map acquired by parsing the JSON file
-type registry() :: map().
% Section is denoted by a list of nested keys in the registry map
-type section() :: kv_utils:path(binary()).
% Entry can be a product version or GUI hash - there is a list of entries
% specified per every version in the compatibility registry.
-type entry() :: onedata:release_version() | onedata:gui_hash().
% A positive integer denoting the revision of the compatibility registry,
% incremented upon every update.
-type revision() :: pos_integer().
% Strategy when retrieving the registry - returns either the current local
% version stored in the registry file, or attempts to fetch a newer version
% from known mirrors (but with a configurable backoff).
-type strategy() :: local | check_for_updates.
% Verbose error returned when the version queried for does not exist in the registry
-type unknown_version_error() :: {unknown_version, onedata:release_version(), {revision, revision()}}.

-define(CURRENT_REGISTRY_FILE, ctool:get_env(current_compatibility_registry_file)).
-define(DEFAULT_REGISTRY_FILE, ctool:get_env(default_compatibility_registry_file)).
-define(REGISTRY_CACHE_TTL, ctool:get_env(compatibility_registry_cache_ttl_secs, 900)). % 15 min
-define(CHECK_FOR_UPDATE_BACKOFF, ctool:get_env(compatibility_registry_check_for_updates_backoff_secs, 900)). % 15 min
-define(REGISTRY_MIRRORS, ctool:get_env(compatibility_registry_mirrors, [])).

% regex to retrieve release version from git full build version
-define(OC_VERSION_REGEXP, <<"^(?<release>[\\w.]+(-\\w+)?)(-\\d+-g\\w+)?$">>).

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
    {error, unknown_version_error() | cannot_parse_registry}.
check_products_compatibility(?ONEZONE, VersionA, ?ONEPROVIDER, VersionB) ->
    check_entry([<<"compatibility">>, <<"onezone:oneprovider">>], VersionA, VersionB);
check_products_compatibility(?ONEPROVIDER, VersionA, ?ONEZONE, VersionB) ->
    check_entry([<<"compatibility">>, <<"oneprovider:onezone">>], VersionA, VersionB);
check_products_compatibility(?ONEPROVIDER, VersionA, ?ONEPROVIDER, VersionB) ->
    check_entry([<<"compatibility">>, <<"oneprovider:oneprovider">>], VersionA, VersionB);
check_products_compatibility(?ONEPROVIDER, VersionA, ?ONECLIENT, VersionB) ->
    check_entry([<<"compatibility">>, <<"oneprovider:oneclient">>],
        VersionA, normalize_oneclient_version(VersionB));
check_products_compatibility(_, _, _, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of product versions compatible with another product in given version.
%% @end
%%--------------------------------------------------------------------
-spec get_compatible_versions(
    ProductA :: onedata:product(), VersionA :: onedata:release_version(),
    ProductB :: onedata:product()) ->
    {ok, CompatibleVersions :: [onedata:release_version()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
get_compatible_versions(?ONEZONE, VersionA, ?ONEPROVIDER) ->
    get_entries([<<"compatibility">>, <<"onezone:oneprovider">>], VersionA);
get_compatible_versions(?ONEPROVIDER, VersionA, ?ONEZONE) ->
    get_entries([<<"compatibility">>, <<"oneprovider:onezone">>], VersionA);
get_compatible_versions(?ONEPROVIDER, VersionA, ?ONEPROVIDER) ->
    get_entries([<<"compatibility">>, <<"oneprovider:oneprovider">>], VersionA);
get_compatible_versions(?ONEPROVIDER, VersionA, ?ONECLIENT) ->
    get_entries([<<"compatibility">>, <<"oneprovider:oneclient">>], VersionA);
get_compatible_versions(_, _, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Checks if the GUI hash is valid for the service in given version.
%% @end
%%--------------------------------------------------------------------
-spec verify_gui_hash(?OP_WORKER_GUI | ?ONEPANEL_GUI | ?HARVESTER_GUI,
    onedata:release_version(), onedata:gui_hash()) ->
    true | {false, CorrectHashes :: [onedata:gui_hash()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
verify_gui_hash(?OP_WORKER_GUI, Version, GuiHash) ->
    check_entry([<<"gui-sha256">>, <<"op-worker">>], Version, GuiHash);
verify_gui_hash(?ONEPANEL_GUI, Version, GuiHash) ->
    check_entry([<<"gui-sha256">>, <<"onepanel">>], Version, GuiHash);
verify_gui_hash(?HARVESTER_GUI, Version, GuiHash) ->
    check_entry([<<"gui-sha256">>, <<"harvester">>], Version, GuiHash);
verify_gui_hash(_, _, _) ->
    error(badarg).


-spec check_for_updates([Mirror :: http_client:url()], TrustedCaCerts :: [public_key:der_encoded()]) ->
    {ok, registry()} | {error, not_updated}.
check_for_updates(Mirrors, TrustedCaCerts) ->
    lists_utils:foldl_while(fun(Mirror, _) ->
        case fetch_registry(Mirror, TrustedCaCerts) of
            {ok, Binary, Registry} ->
                try
                    case overwrite_registry_if_newer(Binary, Registry, Mirror) of
                        true ->
                            {halt, {ok, Registry}};
                        false ->
                            {cont, {error, not_updated}}
                    end
                catch Type:Reason ->
                    ?error_stacktrace(
                        "Error processing newly fetched compatibility registry from mirror: ~s~n~w:~p", [
                            Type, Reason, Mirror
                        ]),
                    {cont, {error, not_updated}}
                end;
            error ->
                {cont, {error, not_updated}}
        end
    end, {error, not_updated}, Mirrors).


-spec peek_current_registry_revision() -> {ok, revision()} | {error, cannot_parse_registry}.
peek_current_registry_revision() ->
    case get_registry(local) of
        {ok, Registry} ->
            {ok, revision(Registry)};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Instantly clears registry cache - the next check will cause the registry file
%% to be read again from disk and (if required) a check for update will be
%% performed.
%% @end
%%--------------------------------------------------------------------
-spec clear_registry_cache() -> ok.
clear_registry_cache() ->
    node_cache:clear({?MODULE, registry}),
    node_cache:clear({?MODULE, check_for_updates_active_backoff}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if given entry is on the list of all valid entries for given registry
%% section and version. May check for updates in case of a negative result.
%% @end
%%--------------------------------------------------------------------
-spec check_entry(section(), onedata:release_version(), entry()) ->
    true | {false, ValidEntries :: [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
check_entry(Section, Version, Entry) ->
    case check_entry(Section, Version, Entry, local) of
        true ->
            true;
        FailureResult ->
            case check_entry(Section, Version, Entry, check_for_updates) of
                {error, not_updated} ->
                    FailureResult;
                Result ->
                    Result
            end
    end.


%% @private
-spec check_entry(section(), onedata:release_version(), entry(), strategy()) ->
    true | {false, ValidEntries :: [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry | not_updated}.
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the list of all valid entries for given registry section and version.
%% May check for updates in case of a negative result.
%% @end
%%--------------------------------------------------------------------
-spec get_entries(section(), onedata:release_version()) ->
    {ok, [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
get_entries(Section, Version) ->
    case get_entries(Section, Version, local) of
        {ok, Entries} ->
            {ok, Entries};
        FailureResult ->
            case get_entries(Section, Version, check_for_updates) of
                {error, not_updated} ->
                    FailureResult;
                Result ->
                    Result
            end
    end.


%% @private
-spec get_entries(section(), onedata:release_version(), strategy()) ->
    {ok, [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry | not_updated}.
get_entries(Section, Version, Strategy) ->
    case get_registry(Strategy) of
        {error, _} = Error ->
            Error;
        {ok, Registry} ->
            AllVersions = get_section(Section, Registry),
            case maps:find(Version, AllVersions) of
                error ->
                    {error, {unknown_version, Version, {revision, revision(Registry)}}};
                {ok, Entries} ->
                    {ok, Entries}
            end
    end.


%% @private
-spec should_check_for_updates() -> boolean().
should_check_for_updates() ->
    IsOnBackoff = node_cache:get({?MODULE, check_for_updates_active_backoff}, false),
    not IsOnBackoff.


%% @private
-spec restart_check_for_updates_backoff() -> ok.
restart_check_for_updates_backoff() ->
    node_cache:put({?MODULE, check_for_updates_active_backoff}, true, ?CHECK_FOR_UPDATE_BACKOFF).


%% @private
-spec get_registry(strategy()) ->
    {ok, registry()} | {error, cannot_parse_registry | not_updated}.
get_registry(local) ->
    node_cache:acquire({?MODULE, registry}, fun() ->
        take_default_registry_if_newer(),
        case file:read_file(?CURRENT_REGISTRY_FILE) of
            {ok, Binary} ->
                case parse_registry(Binary) of
                    {error, cannot_parse_registry} ->
                        {error, cannot_parse_registry};
                    {ok, Registry} ->
                        {ok, Registry, ?REGISTRY_CACHE_TTL}
                end;
            Other ->
                ?error("Cannot parse compatibility registry (~s) due to ~w", [
                    ?CURRENT_REGISTRY_FILE, Other
                ]),
                {error, cannot_parse_registry}
        end
    end);
get_registry(check_for_updates) ->
    case should_check_for_updates() of
        false ->
            {error, not_updated};
        true ->
            restart_check_for_updates_backoff(),
            Mirrors = ?REGISTRY_MIRRORS,
            check_for_updates(Mirrors, [])
    end.


%% @private
-spec fetch_registry(Mirror :: http_client:url(), TrustedCaCerts :: [public_key:der_encoded()]) ->
    {ok, Binary :: binary(), registry()} | error.
fetch_registry(Mirror, TrustedCaCerts) ->
    case http_client:get(Mirror, #{}, <<>>, [{ssl_options, [{cacerts, TrustedCaCerts}]}]) of
        {ok, 200, _, Binary} ->
            case parse_registry(Binary) of
                {error, cannot_parse_registry} ->
                    ?warning("Cannot parse compatibility registry from mirror: ~s", [Mirror]),
                    error;
                {ok, Registry} ->
                    {ok, Binary, Registry}
            end;
        Other ->
            ?warning("Cannot fetch compatibility registry from mirror: ~s", [Mirror]),
            ?debug("Cannot fetch compatibility registry from mirror: ~s~nFetch result: ~p", [Mirror, Other]),
            error
    end.


%% @private
-spec overwrite_registry_if_newer(Binary :: binary(), registry(), Mirror :: http_client:url()) ->
    boolean().
overwrite_registry_if_newer(Binary, Registry, Mirror) ->
    ShouldOverwrite = case get_registry(local) of
        {ok, LocalRegistry} ->
            case revision(Registry) > revision(LocalRegistry) of
                true ->
                    true;
                false ->
                    ?debug(
                        "Ignoring compatibility registry fetched from mirror ~s "
                        "- revision (~B) not newer than local (~B)",
                        [Mirror, revision(Registry), revision(LocalRegistry)]
                    ),
                    false
            end;
        {error, cannot_parse_registry} ->
            % handle situations when the local registry has been deleted or is broken
            true
    end,

    case ShouldOverwrite of
        true ->
            RegistryFile = ?CURRENT_REGISTRY_FILE,
            ?info(
                "Fetched a newer compatibility registry from mirror ~s "
                "(rev. ~B), overwriting the old one at ~s",
                [Mirror, revision(Registry), RegistryFile]
            ),
            ok = file:write_file(RegistryFile, Binary),
            clear_registry_cache(),
            true;
        false ->
            false
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
        OPvsOPCoalesced = maps:fold(fun(VersionA, CompatibleVersions, OuterAcc) ->
            lists:foldl(fun(VersionB, InnerAcc) ->
                InnerAcc#{
                    VersionB => lists:usort([VersionA | maps:get(VersionB, InnerAcc, [])])
                }
            end, OuterAcc, CompatibleVersions)
        end, OPvsOPSection, OPvsOPSection),

        % Registry contains compatible provider versions for each zone version.
        % Reversed relation needs to be calculated.
        OZvsOPSection = maps:get(<<"onezone:oneprovider">>, CompatibilitySection, #{}),
        OPvsOZ = maps:fold(fun(OzVersion, CompatibleOpVersions, OuterAcc) ->
            lists:foldl(fun(OpVersion, InnerAcc) ->
                maps:update_with(OpVersion, fun(CompOzVersions) ->
                    [OzVersion | CompOzVersions]
                end, [OzVersion], InnerAcc)
            end, OuterAcc, CompatibleOpVersions)
        end, #{}, OZvsOPSection),

        %% Harvester GUI entries have another nesting level with human-readable
        %% labels (e.g. "ecrin") - it is flattened here.
        GuiShaSection = maps:get(<<"gui-sha256">>, Registry, #{}),
        HarvesterGuiSection = maps:get(<<"harvester">>, GuiShaSection, #{}),
        HarvesterGuiCoalesced = maps:map(fun(_Version, LabelMap) ->
            lists:flatten(maps:values(LabelMap))
        end, HarvesterGuiSection),

        {ok, Registry#{
            <<"compatibility">> => CompatibilitySection#{
                <<"oneprovider:oneprovider">> => OPvsOPCoalesced,
                <<"oneprovider:onezone">> => OPvsOZ
            },
            <<"gui-sha256">> => GuiShaSection#{
                <<"harvester">> => HarvesterGuiCoalesced
            }
        }}
    catch
        Type:Reason ->
            ?debug_stacktrace("Cannot parse compatibility registry due to ~p:~p", [
                Type, Reason
            ]),
            {error, cannot_parse_registry}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function checks if the default registry for current software version is
%% newer than the stored registry, and if so - replaces the registry with the
%% default. This may happen if the software is upgraded to a newer version
%% before it had a chance to fetch the newer registry.
%% @end
%%--------------------------------------------------------------------
-spec take_default_registry_if_newer() -> ok.
take_default_registry_if_newer() ->
    case {peek_revision(?DEFAULT_REGISTRY_FILE), peek_revision(?CURRENT_REGISTRY_FILE)} of
        {{ok, Default}, {ok, Current}} when Default > Current ->
            {ok, _} = file:copy(?DEFAULT_REGISTRY_FILE, ?CURRENT_REGISTRY_FILE),
            ?notice("Replaced the compatibility registry with the default one (rev. ~B)", [
                Default
            ]);
        {{ok, _Default}, {ok, _Current}} ->
            ?debug("Compatibility registry is not older than the default one");
        {{ok, Default}, {error, cannot_parse_registry}} ->
            {ok, _} = file:copy(?DEFAULT_REGISTRY_FILE, ?CURRENT_REGISTRY_FILE),
            ?notice("Cannot read local compatibility registry - replacing with the default one (rev. ~B)", [
                Default
            ]);
        {Other1, Other2} ->
            ?warning(
                "Cannot compare current and default compatibility registry~n"
                "    Default: ~p~n"
                "    Current: ~p",
                [Other1, Other2]
            )
    end.


%% @private
-spec peek_revision(file:name_all()) -> {ok, revision()} | {error, cannot_parse_registry}.
peek_revision(RegistryFile) ->
    case file:read_file(RegistryFile) of
        {ok, Binary} ->
            case parse_registry(Binary) of
                {error, cannot_parse_registry} ->
                    {error, cannot_parse_registry};
                {ok, Registry} ->
                    {ok, revision(Registry)}
            end;
        Other ->
            ?error("Cannot parse compatibility registry (~s) due to ~w", [
                RegistryFile, Other
            ]),
            {error, cannot_parse_registry}
    end.


%% @private
-spec get_section(section(), registry()) -> map().
get_section(Section, Map) ->
    kv_utils:get(Section, Map, #{}).


%% @private
-spec revision(registry()) -> revision().
revision(#{<<"revision">> := Revision}) ->
    Revision.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Oneclient version can be provided as full build version (e.g 19.02.0-beta1-10-gb51aef),
%% so it needs to be normalized by retrieving release version.
%% @end
%%--------------------------------------------------------------------
-spec normalize_oneclient_version(binary()) -> binary().
normalize_oneclient_version(Version) ->
    case re:run(Version, ?OC_VERSION_REGEXP, [{capture, all_names, binary}]) of
        {match, [NormalizedVersion]} -> NormalizedVersion;
        nomatch -> Version
    end.
