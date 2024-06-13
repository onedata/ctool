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
%%% Due to the fact that compatibility reference is read from file, on
%%% multi-node clusters the knowledge about the compatibility might differ
%%% among different nodes. For this reason, the module uses the concept of a
%%% resolver that needs to know all cluster nodes and will perform unification
%%% of the knowledge while answering queries.
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

-export([build_resolver/2]).
-export([check_products_compatibility/5]).
-export([get_compatible_versions/4]).
-export([verify_gui_hash/4]).
-export([check_for_updates/2]).
-export([peek_current_registry_revision/1]).
-export([clear_registry_cache/0]).
% internal RPC
-export([overwrite_registry/1]).


% Opaque record that holds information required for answering compatibility queries
% on a multi-node cluster. Must be instantiated and passed to this module for every query.
-record(resolver, {
    nodes :: [node()],
    extra_trusted_cacerts :: [public_key:der_encoded()]
}).
-opaque resolver() :: #resolver{}.
-export_type([resolver/0]).

% Private record holding an instance of compatibility registry.
-record(registry, {
    % raw binary data read from a compatibility file or a mirror (must be an encoded JSON)
    raw :: binary(),
    % a nested map acquired by parsing the raw binary
    parsed :: #{binary() => _},
    revision :: revision()
}).
-type registry() :: #registry{}.

% A positive integer denoting the revision of the compatibility registry,
% incremented upon every update.
-type revision() :: pos_integer().
% Section is denoted by a list of nested keys in the registry map
-type section() :: kv_utils:path(binary()).
% Entry can be a product version or GUI hash - there is a list of entries
% specified per every version in the compatibility registry.
-type entry() :: onedata:release_version() | onedata:gui_hash().
% Strategy when retrieving the registry - returns either the current local
% version stored in the registry file, or attempts to fetch a newer version
% from known mirrors (but with a configurable backoff).
-type strategy() :: local | check_for_updates.
% Verbose error returned when the version queried for does not exist in the registry
-type unknown_version_error() :: {unknown_version, onedata:release_version(), {revision, revision()}}.
% An url from which the compatibility registry can be downloaded
-type mirror() :: http_client:url().

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
%% Builds the resolver required for making queries. All nodes in the cluster
%% should be provided so that the compatibility files can be kept coherent on
%% all of them.
%% @end
%%--------------------------------------------------------------------
-spec build_resolver([node()], ExtraTrustedCaCerts :: [public_key:der_encoded()]) -> resolver().
build_resolver(Nodes, ExtraTrustedCaCerts) when is_list(Nodes) andalso is_list(ExtraTrustedCaCerts) ->
    #resolver{nodes = Nodes, extra_trusted_cacerts = ExtraTrustedCaCerts}.


%%--------------------------------------------------------------------
%% @doc
%% Checks if the two products in given versions are compatible with each other.
%% @end
%%--------------------------------------------------------------------
-spec check_products_compatibility(
    resolver(),
    ProductA :: onedata:product(), VersionA :: onedata:release_version(),
    ProductB :: onedata:product(), VersionB :: onedata:release_version()) ->
    true | {false, CompatibleVersions :: [onedata:release_version()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
check_products_compatibility(Resolver, ?ONEZONE, VersionA, ?ONEPROVIDER, VersionB) ->
    check_entry(Resolver, [<<"compatibility">>, <<"onezone:oneprovider">>], VersionA, VersionB);
check_products_compatibility(Resolver, ?ONEPROVIDER, VersionA, ?ONEZONE, VersionB) ->
    check_entry(Resolver, [<<"compatibility">>, <<"oneprovider:onezone">>], VersionA, VersionB);
check_products_compatibility(Resolver, ?ONEPROVIDER, VersionA, ?ONEPROVIDER, VersionB) ->
    check_entry(Resolver, [<<"compatibility">>, <<"oneprovider:oneprovider">>], VersionA, VersionB);
check_products_compatibility(Resolver, ?ONEPROVIDER, VersionA, ?ONECLIENT, VersionB) ->
    check_entry(Resolver, [<<"compatibility">>, <<"oneprovider:oneclient">>],
        VersionA, normalize_oneclient_version(VersionB));
check_products_compatibility(_, _, _, _, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of product versions compatible with another product in given version.
%% @end
%%--------------------------------------------------------------------
-spec get_compatible_versions(
    resolver(),
    ProductA :: onedata:product(), VersionA :: onedata:release_version(),
    ProductB :: onedata:product()) ->
    {ok, CompatibleVersions :: [onedata:release_version()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
get_compatible_versions(Resolver, ?ONEZONE, VersionA, ?ONEPROVIDER) ->
    get_entries(Resolver, [<<"compatibility">>, <<"onezone:oneprovider">>], VersionA);
get_compatible_versions(Resolver, ?ONEPROVIDER, VersionA, ?ONEZONE) ->
    get_entries(Resolver, [<<"compatibility">>, <<"oneprovider:onezone">>], VersionA);
get_compatible_versions(Resolver, ?ONEPROVIDER, VersionA, ?ONEPROVIDER) ->
    get_entries(Resolver, [<<"compatibility">>, <<"oneprovider:oneprovider">>], VersionA);
get_compatible_versions(Resolver, ?ONEPROVIDER, VersionA, ?ONECLIENT) ->
    get_entries(Resolver, [<<"compatibility">>, <<"oneprovider:oneclient">>], VersionA);
get_compatible_versions(_, _, _, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Checks if the GUI hash is valid for the service in given version.
%% @end
%%--------------------------------------------------------------------
-spec verify_gui_hash(resolver(), ?OP_WORKER_GUI | ?ONEPANEL_GUI | ?HARVESTER_GUI,
    onedata:release_version(), onedata:gui_hash()) ->
    true | {false, CorrectHashes :: [onedata:gui_hash()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
verify_gui_hash(Resolver, ?OP_WORKER_GUI, Version, GuiHash) ->
    check_entry(Resolver, [<<"gui-sha256">>, <<"op-worker">>], Version, GuiHash);
verify_gui_hash(Resolver, ?ONEPANEL_GUI, Version, GuiHash) ->
    check_entry(Resolver, [<<"gui-sha256">>, <<"onepanel">>], Version, GuiHash);
verify_gui_hash(Resolver, ?HARVESTER_GUI, Version, GuiHash) ->
    check_entry(Resolver, [<<"gui-sha256">>, <<"harvester">>], Version, GuiHash);
verify_gui_hash(_, _, _, _) ->
    error(badarg).


-spec check_for_updates(resolver(), [mirror()]) -> {ok, registry()} | {error, not_updated}.
check_for_updates(Resolver, Mirrors) ->
    lists_utils:foldl_while(fun(Mirror, _) ->
        case fetch_registry(Resolver, Mirror) of
            {ok, Registry} ->
                try
                    case take_registry_from_mirror_if_newer(Resolver, Registry, Mirror) of
                        true ->
                            {halt, {ok, Registry}};
                        false ->
                            {cont, {error, not_updated}}
                    end
                catch Type:Reason:Stacktrace ->
                    ?error_stacktrace("Error processing newly fetched compatibility registry from mirror: ~ts~n~w:~tp", [
                        Mirror, Type, Reason
                    ], Stacktrace),
                    {cont, {error, not_updated}}
                end;
            error ->
                {cont, {error, not_updated}}
        end
    end, {error, not_updated}, Mirrors).


-spec peek_current_registry_revision(resolver()) -> {ok, revision()} | {error, cannot_parse_registry}.
peek_current_registry_revision(Resolver) ->
    case get_registry(Resolver, local) of
        {ok, Registry} ->
            {ok, Registry#registry.revision};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Instantly clears registry cache - the next check will cause the registry file
%% to be read again from disk and (if required) a check for update will be performed.
%% @end
%%--------------------------------------------------------------------
-spec clear_registry_cache() -> ok.
clear_registry_cache() ->
    node_cache:clear({?MODULE, registry}),
    node_cache:clear({?MODULE, check_for_updates_active_backoff}).


%% @private
%% exported for internal RPC
-spec overwrite_registry(registry()) -> ok.
overwrite_registry(#registry{raw = Binary, revision = Revision}) ->
    RegistryFile = ?CURRENT_REGISTRY_FILE,
    ok = file:write_file(RegistryFile, Binary),
    ?info("Overwritten the compatibility registry with a newer one (rev. ~B) at ~ts", [
        Revision, RegistryFile
    ]),
    clear_registry_cache().

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
-spec check_entry(resolver(), section(), onedata:release_version(), entry()) ->
    true | {false, ValidEntries :: [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
check_entry(Resolver, Section, Version, Entry) ->
    case check_entry(Resolver, Section, Version, Entry, local) of
        true ->
            true;
        FailureResult ->
            case check_entry(Resolver, Section, Version, Entry, check_for_updates) of
                {error, not_updated} ->
                    FailureResult;
                Result ->
                    Result
            end
    end.


%% @private
-spec check_entry(resolver(), section(), onedata:release_version(), entry(), strategy()) ->
    true | {false, ValidEntries :: [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry | not_updated}.
check_entry(Resolver, Section, Version, Entry, Strategy) ->
    case get_entries(Resolver, Section, Version, Strategy) of
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
-spec get_entries(resolver(), section(), onedata:release_version()) ->
    {ok, [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry}.
get_entries(Resolver, Section, Version) ->
    case get_entries(Resolver, Section, Version, local) of
        {ok, Entries} ->
            {ok, Entries};
        FailureResult ->
            case get_entries(Resolver, Section, Version, check_for_updates) of
                {error, not_updated} ->
                    FailureResult;
                Result ->
                    Result
            end
    end.


%% @private
-spec get_entries(resolver(), section(), onedata:release_version(), strategy()) ->
    {ok, [entry()]} |
    {error, unknown_version_error() | cannot_parse_registry | not_updated}.
get_entries(Resolver, Section, Version, Strategy) ->
    case get_registry(Resolver, Strategy) of
        {error, _} = Error ->
            Error;
        {ok, Registry} ->
            AllVersions = get_section(Section, Registry),
            case maps:find(Version, AllVersions) of
                error ->
                    {error, {unknown_version, Version, {revision, Registry#registry.revision}}};
                {ok, Entries} ->
                    {ok, Entries}
            end
    end.


%% @private
-spec get_registry(resolver(), strategy()) ->
    {ok, registry()} | {error, cannot_parse_registry | not_updated}.
get_registry(Resolver, local) ->
    node_cache:acquire({?MODULE, registry}, fun() ->
        take_default_registry_if_newer(Resolver),
        case read_registry(?CURRENT_REGISTRY_FILE) of
            {ok, Registry} ->
                {ok, Registry, ?REGISTRY_CACHE_TTL};
            {error, cannot_parse_registry} ->
                {error, cannot_parse_registry}
        end
    end);
get_registry(Resolver, check_for_updates) ->
    case should_check_for_updates() of
        false ->
            {error, not_updated};
        true ->
            restart_check_for_updates_backoff(),
            check_for_updates(Resolver, ?REGISTRY_MIRRORS)
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
-spec fetch_registry(resolver(), mirror()) -> {ok, registry()} | error.
fetch_registry(#resolver{extra_trusted_cacerts = TrustedCaCerts}, Mirror) ->
    case http_client:get(Mirror, #{}, <<>>, [{ssl_options, [{cacerts, TrustedCaCerts}]}]) of
        {ok, 200, _, Response} ->
            case parse_registry(Response) of
                {error, cannot_parse_registry} ->
                    ?warning("Cannot parse compatibility registry from mirror: ~ts", [Mirror]),
                    error;
                {ok, Registry} ->
                    {ok, Registry}
            end;
        Other ->
            ?warning("Cannot fetch compatibility registry from mirror: ~ts", [Mirror]),
            ?debug("Cannot fetch compatibility registry from mirror: ~ts~nFetch result: ~tp", [Mirror, Other]),
            error
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
-spec take_default_registry_if_newer(resolver()) -> ok.
take_default_registry_if_newer(Resolver) ->
    case {read_registry(?DEFAULT_REGISTRY_FILE), read_registry(?CURRENT_REGISTRY_FILE)} of
        {{ok, Default}, {ok, Current}} when Default#registry.revision > Current#registry.revision ->
            ?notice("Replacing the compatibility registry with the default one (rev. ~B)", [Default#registry.revision]),
            overwrite_registry_on_all_nodes(Resolver, Default);
        {{ok, _Default}, {ok, _Current}} ->
            ?debug("Compatibility registry is not older than the default one");
        {{ok, Default = #registry{revision = Revision}}, {error, cannot_parse_registry}} ->
            ?notice("Cannot read local compatibility registry - replacing with the default one (rev. ~B)", [Revision]),
            overwrite_registry_on_all_nodes(Resolver, Default);
        {Other1, Other2} ->
            ?warning(
                "Cannot compare current and default compatibility registry~n"
                "    Default: ~tp~n"
                "    Current: ~tp",
                [Other1, Other2]
            )
    end.


%% @private
-spec take_registry_from_mirror_if_newer(resolver(), registry(), mirror()) -> boolean().
take_registry_from_mirror_if_newer(Resolver, Registry = #registry{revision = CandidateRevision}, Mirror) ->
    case get_registry(Resolver, local) of
        {ok, #registry{revision = LocalRevision}} when LocalRevision >= CandidateRevision ->
            ?debug("Ignoring compatibility registry fetched from mirror ~ts - revision (~B) not newer than local (~B)", [
                Mirror, CandidateRevision, LocalRevision
            ]),
            false;
        _ ->
            % also handles situations when the local registry has been deleted or is broken and cannot be parsed
            ?info("Fetched a newer (rev. ~B) compatibility registry from mirror ~ts", [CandidateRevision, Mirror]),
            overwrite_registry_on_all_nodes(Resolver, Registry),
            true
    end.


%% @private
-spec overwrite_registry_on_all_nodes(resolver(), registry()) -> ok.
overwrite_registry_on_all_nodes(#resolver{nodes = Nodes}, Registry) ->
    lists:foreach(fun(Node) ->
        case rpc:call(Node, ?MODULE, overwrite_registry, [Registry]) of
            ok ->
                ok;
            {badrpc, Reason} ->
                ?warning(
                    "Failed to overwrite the compatibility registry on node ~tp - it might have "
                    "an outdated knowledge about Onedata services compatibility.~nError was: ~tp", [
                        Node, {badrpc, Reason}
                    ]
                )
        end
    end, Nodes).


-spec read_registry(file:name_all()) -> {ok, registry()} | {error, cannot_parse_registry}.
read_registry(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            parse_registry(Binary);
        Other ->
            ?error("Cannot read compatibility registry (~ts) due to ~w", [FilePath, Other]),
            {error, cannot_parse_registry}
    end.


%% @private
-spec parse_registry(RawRegistry :: binary()) -> {ok, registry()} | {error, cannot_parse_registry}.
parse_registry(RawRegistry) ->
    try
        Parsed = json_utils:decode(RawRegistry),
        Revision = maps:get(<<"revision">>, Parsed),
        Revision > 0 orelse throw(invalid_revision),

        %% In case of compatibility between providers, the relation is symmetrical,
        %% but the registry file might not have symmetrical entries - they are
        %% coalesced here.
        CompatibilitySection = maps:get(<<"compatibility">>, Parsed, #{}),
        OPvsOPSection = maps:get(<<"oneprovider:oneprovider">>, CompatibilitySection, #{}),
        OPvsOPCoalesced = maps:fold(fun(VersionA, CompatibleVersions, OuterAcc) ->
            lists:foldl(fun(VersionB, InnerAcc) ->
                InnerAcc#{
                    VersionB => lists:usort([VersionA | maps:get(VersionB, InnerAcc, [])])
                }
            end, OuterAcc, CompatibleVersions)
        end, OPvsOPSection, OPvsOPSection),

        % Parsed contains compatible provider versions for each zone version.
        % Reversed relation needs to be calculated.
        OZvsOPSection = maps:get(<<"onezone:oneprovider">>, CompatibilitySection, #{}),
        OPvsOZ = maps:fold(fun(OzVersion, CompatibleOpVersions, OuterAcc) ->
            lists:foldl(fun(OpVersion, InnerAcc) ->
                maps:update_with(OpVersion, fun(CompOzVersions) ->
                    lists:usort([OzVersion | CompOzVersions])
                end, [OzVersion], InnerAcc)
            end, OuterAcc, CompatibleOpVersions)
        end, #{}, OZvsOPSection),

        %% Harvester GUI entries have another nesting level with human-readable
        %% labels (e.g. "ecrin") - it is flattened here.
        GuiShaSection = maps:get(<<"gui-sha256">>, Parsed, #{}),
        HarvesterGuiSection = maps:get(<<"harvester">>, GuiShaSection, #{}),
        HarvesterGuiCoalesced = maps:map(fun(_Version, LabelMap) ->
            lists:flatten(maps:values(LabelMap))
        end, HarvesterGuiSection),

        {ok, #registry{
            raw = RawRegistry,
            revision = Revision,
            parsed = Parsed#{
                <<"compatibility">> => CompatibilitySection#{
                    <<"oneprovider:oneprovider">> => OPvsOPCoalesced,
                    <<"oneprovider:onezone">> => OPvsOZ
                },
                <<"gui-sha256">> => GuiShaSection#{
                    <<"harvester">> => HarvesterGuiCoalesced
                }
            }
        }}
    catch
        Class:Reason:Stacktrace ->
            ?debug_exception("Cannot parse compatibility registry", Class, Reason, Stacktrace),
            {error, cannot_parse_registry}
    end.


%% @private
-spec get_section(section(), registry()) -> map().
get_section(Section, #registry{parsed = Parsed}) ->
    kv_utils:get(Section, Parsed, #{}).


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
