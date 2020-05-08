%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to space support stages.
%%%
%%% Support stage is defined for supporting storages and providers and expressed
%%% in the #support_stage_details{} record - one per space per provider. The
%%% record stores information about support stages of all provider's storages
%%% (provider can support the space with more than one storage) and the
%%% resulting support stage of the provider. The provider's stage is
%%% always inferred from the storage stages and cannot be modified directly -
%%% serves as a summary of the provider's support in the space.
%%%
%%% NOTE: consult ./support_stage.jpg for broader descriptions and a diagram
%%% with possible transitions, which is essentially implemented in this module.
%%% @end
%%%-------------------------------------------------------------------
-module(support_stage).
-author("Lukasz Opiola").

-include("space_support/support_stage.hrl").
-include("errors.hrl").
-include("logging.hrl").

-type provider_id() :: onedata:service_id().
-type storage_id() :: gri:entity_id().
-type resize_target() :: non_neg_integer().

% Holds information about support stages of all provider's storages and the
% resulting support stage of the provider.
-type details() :: #support_stage_details{}.

% Current support stage of a storage:
%   none - the storage is not supporting the space (not represented per se in
%             the details - should be interpreted as lack of entry)
%   joining - phase of initial setup after a space is supported and when the
%             storage cannot be used yet, transitions to active when the
%             provider has caught up with dbsync changes from other providers.
%   active - the storage is operational
%   {resizing, TargetSize} - the granted storage quota by this storage is being
%             changed, which may be a long process if replicas need to be
%             evicted to free up space. If support is being completely revoked,
%             the stage yields {resizing, 0}. During resizing, the target size
%             may be changed.
%   purging (unsupporting) - the file data on the storage is being purged
%   retiring (unsupporting) - the owning provider is finalizing the storage's
%             retirement process - if this was the last supporting storage, it
%             is purging the database by deleting docs related to space support.
%   retired - the storage has supported the space in the past, but not anymore
-type storage_support_stage() :: none | joining | active | {resizing, resize_target()}
| purging | retiring | retired.

% Current support stage of a provider - cannot be changed directly, it always
% results from stages of its storages:
%   none - the provider is not supporting the space (not represented per se in
%             the details - should be interpreted as lack of entry)
%   joining - the provider has freshly supported the space and is catching up
%            with dbsync changes from other providers
%   active - the provider is operational and up-to-date with other providers,
%            all storages are in 'active' stage
%   remodelling - the provider is undergoing changes concerning its storage(s)
%            that support the space, e.g. a storage has been added or removed
%            and is not yet in active stage (such modifications may require
%            long-lasting data transfers between storages).
%   evicting_replicas (unsupporting) - the provider has chosen to cease support
%            for the space with all of its storages and is now safely migrating
%            data to other providers
%   purging_storages (unsupporting) - the provider has evicted all replicas and
%            is now purging its storages from support remnants
%   purging_database (unsupporting) - the provider purged its storages and is
%            now purging the database (including file metadata, locations etc)
%   retired - the provider has supported the space in the past, but not anymore
-type provider_support_stage() :: none | joining | active | remodelling
| evicting_replicas | purging_storages | purging_database | retired.
-export_type([storage_id/0, details/0, provider_support_stage/0, storage_support_stage/0]).

-type per_provider() :: #{provider_id() => details()}.
-export_type([per_provider/0]).

-export([init_support/3, finalize_support/3]).
-export([init_resize/4, finalize_resize/3]).
-export([init_unsupport/3, complete_unsupport_resize/3, complete_unsupport_purge/3, finalize_unsupport/3]).

-export([apply_transition/4]).
-export([insert_legacy_support_entry/2]).
-export([mark_legacy_support_revocation/2]).
-export([lookup_details_by_provider/2]).
-export([per_provider_to_json/1, per_provider_from_json/1]).
-export([serialize/2, deserialize/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init_support(per_provider(), provider_id(), storage_id()) ->
    {ok, per_provider()} | errors:error().
init_support(PerProvider, ProviderId, StorageId) ->
    apply_transition(PerProvider, ProviderId, StorageId, joining).


-spec finalize_support(per_provider(), provider_id(), storage_id()) ->
    {ok, per_provider()} | errors:error().
finalize_support(PerProvider, ProviderId, StorageId) ->
    apply_transition(PerProvider, ProviderId, StorageId, active).


-spec init_resize(per_provider(), provider_id(), storage_id(), resize_target()) ->
    {ok, per_provider()} | errors:error().
init_resize(PerProvider, ProviderId, StorageId, TargetSize) when TargetSize > 0 ->
    apply_transition(PerProvider, ProviderId, StorageId, {resizing, TargetSize}).


-spec finalize_resize(per_provider(), provider_id(), storage_id()) ->
    {ok, per_provider()} | errors:error().
finalize_resize(PerProvider, ProviderId, StorageId) ->
    apply_transition(PerProvider, ProviderId, StorageId, active).


-spec init_unsupport(per_provider(), provider_id(), storage_id()) ->
    {ok, per_provider()} | errors:error().
init_unsupport(PerProvider, ProviderId, StorageId) ->
    apply_transition(PerProvider, ProviderId, StorageId, {resizing, 0}).


-spec complete_unsupport_resize(per_provider(), provider_id(), storage_id()) ->
    {ok, per_provider()} | errors:error().
complete_unsupport_resize(PerProvider, ProviderId, StorageId) ->
    apply_transition(PerProvider, ProviderId, StorageId, purging).


-spec complete_unsupport_purge(per_provider(), provider_id(), storage_id()) ->
    {ok, per_provider()} | errors:error().
complete_unsupport_purge(PerProvider, ProviderId, StorageId) ->
    apply_transition(PerProvider, ProviderId, StorageId, retiring).


-spec finalize_unsupport(per_provider(), provider_id(), storage_id()) ->
    {ok, per_provider()} | errors:error().
finalize_unsupport(PerProvider, ProviderId, StorageId) ->
    apply_transition(PerProvider, ProviderId, StorageId, retired).


%%--------------------------------------------------------------------
%% @doc
%% Low level API for changing support stages - functions above are wrappers
%% using this function to provide a high level API for space support lifecycle.
%% @end
%%--------------------------------------------------------------------
-spec apply_transition(per_provider(), provider_id(), storage_id(), storage_support_stage()) ->
    {ok, per_provider()} | errors:error().
apply_transition(PerProvider, ProviderId, StorageId, NewStorageStage) ->
    CurrentStageDetails = case lookup_details_by_provider(PerProvider, ProviderId) of
        % legacy entry is treated as if there was no entry at all
        % (legacy providers will appear as joining after upgrade).
        {ok, StageDetails} when StageDetails /= ?LEGACY_SUPPORT_STAGE_DETAILS ->
            StageDetails;
        _ ->
            #support_stage_details{
                provider_stage = none,
                per_storage = #{
                    StorageId => none
                }
            }
    end,
    case apply_stage_transition(CurrentStageDetails, StorageId, NewStorageStage) of
        {ok, NewStageDetails} ->
            {ok, PerProvider#{ProviderId => NewStageDetails}};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Inserts a special indication that the provider is in legacy version and does
%% not recognize supports stages.
%% @end
%%--------------------------------------------------------------------
-spec insert_legacy_support_entry(per_provider(), provider_id()) ->
    per_provider().
insert_legacy_support_entry(PerProvider, ProviderId) ->
    PerProvider#{ProviderId => ?LEGACY_SUPPORT_STAGE_DETAILS}.


%%--------------------------------------------------------------------
%% @doc
%% Cleans up the legacy support entry. Legacy supports are not retained in
%% support history, but removed completely.
%% @end
%%--------------------------------------------------------------------
-spec mark_legacy_support_revocation(per_provider(), provider_id()) ->
    per_provider().
mark_legacy_support_revocation(PerProvider, ProviderId) ->
    maps:remove(ProviderId, PerProvider).


-spec lookup_details_by_provider(per_provider(), provider_id()) ->
    {ok, details()} | error.
lookup_details_by_provider(PerProvider, ProviderId) ->
    maps:find(ProviderId, PerProvider).


-spec per_provider_to_json(per_provider()) -> json_utils:json_map().
per_provider_to_json(PerProvider) ->
    maps:map(fun(_ProviderId, SupportStage) ->
        details_to_json(SupportStage)
    end, PerProvider).


-spec per_provider_from_json(json_utils:json_map()) -> per_provider().
per_provider_from_json(PerProviderJson) ->
    maps:map(fun(_ProviderId, SupportStage) ->
        details_from_json(SupportStage)
    end, PerProviderJson).


-spec details_to_json(details()) -> json_utils:json_term().
details_to_json(?LEGACY_SUPPORT_STAGE_DETAILS) ->
    <<"legacySupport">>;
details_to_json(#support_stage_details{provider_stage = ProviderStage, per_storage = PerStorage}) ->
    #{
        <<"providerStage">> => serialize(provider, ProviderStage),
        <<"perStorage">> => maps:map(fun(_StorageId, StorageStage) ->
            serialize(storage, StorageStage)
        end, PerStorage)
    }.


-spec details_from_json(json_utils:json_term()) -> details().
details_from_json(<<"legacySupport">>) ->
    ?LEGACY_SUPPORT_STAGE_DETAILS;
details_from_json(#{<<"providerStage">> := ProviderStage, <<"perStorage">> := PerStorage}) ->
    #support_stage_details{
        provider_stage = deserialize(provider, ProviderStage),
        per_storage = maps:map(fun(_StorageId, StorageStage) ->
            deserialize(storage, StorageStage)
        end, PerStorage)
    }.


-spec serialize(storage | provider, storage_support_stage() | provider_support_stage()) -> binary().
serialize(storage, none) -> <<"none">>;
serialize(storage, joining) -> <<"joining">>;
serialize(storage, active) -> <<"active">>;
serialize(storage, {resizing, TargetSize}) -> <<"resizing:", (integer_to_binary(TargetSize))/binary>>;
serialize(storage, purging) -> <<"purging">>;
serialize(storage, retiring) -> <<"retiring">>;
serialize(storage, retired) -> <<"retired">>;

serialize(provider, none) -> <<"none">>;
serialize(provider, joining) -> <<"joining">>;
serialize(provider, active) -> <<"active">>;
serialize(provider, remodelling) -> <<"remodelling">>;
serialize(provider, evicting_replicas) -> <<"evictingReplicas">>;
serialize(provider, purging_storages) -> <<"purgingStorages">>;
serialize(provider, purging_database) -> <<"purgingDatabase">>;
serialize(provider, retired) -> <<"retired">>.


-spec deserialize(storage | provider, binary()) -> storage_support_stage() | provider_support_stage().
deserialize(storage, <<"none">>) -> none;
deserialize(storage, <<"joining">>) -> joining;
deserialize(storage, <<"active">>) -> active;
deserialize(storage, <<"resizing:", TargetSize/binary>>) -> {resizing, binary_to_integer(TargetSize)};
deserialize(storage, <<"purging">>) -> purging;
deserialize(storage, <<"retiring">>) -> retiring;
deserialize(storage, <<"retired">>) -> retired;

deserialize(provider, <<"none">>) -> none;
deserialize(provider, <<"joining">>) -> joining;
deserialize(provider, <<"active">>) -> active;
deserialize(provider, <<"remodelling">>) -> remodelling;
deserialize(provider, <<"evictingReplicas">>) -> evicting_replicas;
deserialize(provider, <<"purgingStorages">>) -> purging_storages;
deserialize(provider, <<"purgingDatabase">>) -> purging_database;
deserialize(provider, <<"retired">>) -> retired.

%%%===================================================================
%%% Internal functions - state transitions for support stages
%%%===================================================================

%% @private
-spec apply_stage_transition(details(), storage_id(), storage_support_stage()) ->
    {ok, details()} | errors:error().
apply_stage_transition(SupportStageDetails, StorageId, NewStorageStage) ->
    #support_stage_details{
        provider_stage = ProviderStage,
        per_storage = PerStorage
    } = SupportStageDetails,

    CurrentStorageStage = maps:get(StorageId, PerStorage, none),
    case is_transition_allowed(CurrentStorageStage, NewStorageStage) of
        true ->
            NewStagesPerStorage = PerStorage#{StorageId => NewStorageStage},
            case infer_provider_stage(ProviderStage, NewStagesPerStorage) of
                {ok, NewProviderStage} ->
                    {ok, #support_stage_details{
                        provider_stage = NewProviderStage,
                        per_storage = NewStagesPerStorage
                    }};
                illegal ->
                    ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ProviderStage, CurrentStorageStage)
            end;
        false ->
            ?ERROR_ILLEGAL_SUPPORT_STAGE_TRANSITION(ProviderStage, CurrentStorageStage)
    end.


%% @private
-spec infer_provider_stage(provider_support_stage(), #{storage_id() => storage_support_stage()}) ->
    {ok, provider_support_stage()} | illegal.
infer_provider_stage(ProviderStage, StorageStages) ->
    StorageStagesOrdset = ordsets:from_list(maps:values(StorageStages)),
    % 'retired' state is only archival and does not affect the resulting provider
    % support stage, apart from the last transition to 'retired' - remove to reduce
    % the number of combinations in transitions
    case {ProviderStage, ordsets:del_element(retired, StorageStagesOrdset)} of
        {none, [joining]} -> {ok, joining};

        % All storages are in joining stage (a new one have just appeared)
        {joining, [joining]} -> {ok, joining};
        % All joining storages have become active
        {joining, [active]} -> {ok, active};
        % Any mix of the above or one of the active storages transitioned to another state
        {joining, _} -> {ok, remodelling};

        % All storages have begun the retirement procedure
        {active, [{resizing, 0}]} -> {ok, evicting_replicas};
        % At least one storage is not retiring
        {active, _} -> {ok, remodelling};

        % All storages have become active after a remodelling
        {remodelling, [active]} -> {ok, active};
        % Check if there are any storages that haven't yet started the retirement process
        {remodelling, StatesOrdset} ->
            case ordsets:subtract(StatesOrdset, ordsets:from_list([{resizing, 0}, purging, retiring])) of
                % All storages have begun the retirement process
                [] -> {ok, evicting_replicas};
                % Some storages are not active or retiring -> remodelling is in progress
                _ -> {ok, remodelling}
            end;

        {evicting_replicas, StatesOrdset} ->
            % Check if there are any storages that haven't yet transitioned to purging or retiring
            case ordsets:subtract(StatesOrdset, ordsets:from_list([purging, retiring])) of
                % All storages have begun or finished purging the file data
                [] -> {ok, purging_storages};
                % Some storages are still resizing to zero (evicting replicas) - wait with purging_storages
                [{resizing, 0}] -> {ok, evicting_replicas};
                % Some storages are not purging or resizing to zero, which means that one of the resize
                % processes has been modified to other target size or a new storage joined - cancel retirement
                _ -> {ok, remodelling}
            end;

        % All storages have finished purging and transitioned to retiring
        {purging_storages, [retiring]} -> {ok, purging_database};
        % At least one storage is still purging
        {purging_storages, [purging, retiring]} -> {ok, purging_storages};
        % Some storages are not purging nor retiring, which means that a new storage joined - cancel retirement
        {purging_storages, _} -> {ok, remodelling};

        % All storages have become retired (matches to empty list as retired state was removed from the ordset)
        {purging_database, []} -> {ok, retired};
        % At least one storage is still retiring
        {purging_database, [retiring]} -> {ok, purging_database};
        % Joining a new storage is not allowed when the database is being purged
        {purging_database, _} -> illegal;

        % One of the storages have become joining again - provider's re-support after retirement
        {retired, [joining]} -> {ok, joining};

        % Other transitions are not allowed, such changes to the space support must be declined
        {_ ,_} -> illegal
    end.


%% @private
-spec is_transition_allowed(From :: storage_support_stage(), To :: storage_support_stage()) -> boolean().
is_transition_allowed(none, joining) -> true;
is_transition_allowed(joining, active) -> true;
is_transition_allowed(active, {resizing, TargetSize}) when is_integer(TargetSize) -> true;
is_transition_allowed({resizing, 0}, purging) -> true;
is_transition_allowed({resizing, _}, {resizing, TargetSize}) when is_integer(TargetSize) -> true;
is_transition_allowed({resizing, TargetSize}, active) when TargetSize > 0 -> true;
is_transition_allowed(purging, retiring) -> true;
is_transition_allowed(retiring, retired) -> true;
is_transition_allowed(retired, joining) -> true;
is_transition_allowed(_, _) -> false.
