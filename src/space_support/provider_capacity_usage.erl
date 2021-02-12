%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to provider's capacity
%%% usage statistics within a space.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_capacity_usage).
-author("Lukasz Opiola").

-include("space_support/provider_capacity_usage.hrl").

-export([new_registry/0]).
-export([lookup_by_provider/2]).
-export([register_support/4]).
-export([register_unsupport/3]).
-export([build_report/2, consume_report/3]).
-export([report_to_json/1, report_from_json/1]).
-export([registry_to_json/1, registry_from_json/1]).

-type bytes() :: ?UNKNOWN_USAGE_VALUE | non_neg_integer().
% Denotes that the capacity is either fully or close to fully utilized.
-type overfull() :: boolean().
-export_type([bytes/0, overfull/0]).

% The registry object (one for a space) accumulates all information about
% storage capacity usage for each storage of each supporting provider.
-type storage_capacity_usage() :: #storage_capacity_usage{}.
-type provider_capacity_usage() :: #provider_capacity_usage{}.
-type registry() :: #{onedata:provider_id() => provider_capacity_usage()}.
-export_type([registry/0]).

% Storage usage report regularly submitted by a provider - should include all
% its supporting storages, but due to asynchronicity of changes in supports
% there might be missing or superfluous entries.
-type report() :: #{onedata:storage_id() => bytes()}.
-export_type([report/0]).

% This record carries information about a registry update outcome - the updated
% registry and a list of transitions from/to joining/desync state.
-type registry_update_result() :: #capacity_usage_registry_update_result{}.
-export_type([registry_update_result/0]).

-define(OVERFULL_THRESHOLD_PERCENT, ctool:get_env(provider_capacity_usage_overfull_threshold_percent, 98)).

%%%===================================================================
%%% API
%%%===================================================================

-spec new_registry() -> registry().
new_registry() ->
    #{}.


-spec lookup_by_provider(registry(), onedata:provider_id()) -> {ok, provider_capacity_usage()} | error.
lookup_by_provider(Registry, ProviderId) ->
    maps:find(ProviderId, Registry).


-spec register_support(registry(), onedata:provider_id(), onedata:storage_id(), bytes()) ->
    registry_update_result().
register_support(Registry, ProviderId, StorageId, SupportSize) ->
    PrevProviderUsage = maps:get(ProviderId, Registry, #provider_capacity_usage{}),
    DefaultEntry = #storage_capacity_usage{
        total = SupportSize,
        used = ?UNKNOWN_USAGE_VALUE,
        overfull = false
    },
    NewRegistry = Registry#{ProviderId => update_entries_per_storage(
        PrevProviderUsage, maps:put(StorageId, DefaultEntry, PrevProviderUsage#provider_capacity_usage.per_storage)
    )},
    build_registry_update_result(Registry, NewRegistry).


-spec register_unsupport(registry(), onedata:provider_id(), onedata:storage_id()) ->
    registry_update_result().
register_unsupport(Registry, ProviderId, StorageId) ->
    PrevProviderUsage = maps:get(ProviderId, Registry, #provider_capacity_usage{}),
    NewRegistry = case maps:remove(StorageId, PrevProviderUsage#provider_capacity_usage.per_storage) of
        Empty when map_size(Empty) =:= 0 ->
            maps:remove(ProviderId, Registry);
        NewPerStorage ->
            Registry#{ProviderId => update_entries_per_storage(PrevProviderUsage, NewPerStorage)}
    end,
    build_registry_update_result(Registry, NewRegistry).


-spec build_report([onedata:storage_id()], fun((onedata:storage_id()) -> bytes())) -> report().
build_report(Storages, BuildReportForStorage) ->
    lists:foldl(fun(StorageId, Acc) ->
        Acc#{StorageId => BuildReportForStorage(StorageId)}
    end, #{}, Storages).


-spec consume_report(registry(), onedata:provider_id(), report()) ->
    registry_update_result().
consume_report(Registry, ProviderId, Report) ->
    PrevProviderUsage = maps:get(ProviderId, Registry, #provider_capacity_usage{}),
    PrevPerStorage = PrevProviderUsage#provider_capacity_usage.per_storage,
    ReportWithRegisteredStorages = maps:with(maps:keys(PrevPerStorage), Report),
    NewPerStorage = maps:fold(fun(StorageId, BytesUsed, Acc) ->
        maps:update_with(StorageId, fun(StorageUsage) ->
            StorageUsage#storage_capacity_usage{
                used = BytesUsed,
                overfull = is_overfull(BytesUsed, StorageUsage#storage_capacity_usage.total)
            }
        end, Acc)
    end, PrevPerStorage, ReportWithRegisteredStorages),
    NewRegistry = Registry#{ProviderId => update_entries_per_storage(PrevProviderUsage, NewPerStorage)},
    build_registry_update_result(Registry, NewRegistry).


-spec report_to_json(report()) -> json_utils:json_map().
report_to_json(Report) ->
    sanitize_report(Report).


-spec report_from_json(json_utils:json_map()) -> report().
report_from_json(ReportJson) ->
    sanitize_report(ReportJson).


-spec registry_to_json(registry()) -> json_utils:json_map().
registry_to_json(Registry) ->
    maps:map(fun(ProviderId, ProviderCapacityUsage) when is_binary(ProviderId) ->
        provider_capacity_usage_to_json(ProviderCapacityUsage)
    end, Registry).


-spec registry_from_json(json_utils:json_map()) -> registry().
registry_from_json(RegistryJson) ->
    maps:map(fun(ProviderId, ProviderCapacityUsage) when is_binary(ProviderId) ->
        provider_capacity_usage_from_json(ProviderCapacityUsage)
    end, RegistryJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec is_overfull(Used :: bytes(), Total :: bytes()) -> boolean().
is_overfull(Used, Total) ->
    Used * 100 / Total > ?OVERFULL_THRESHOLD_PERCENT.


%% @private
-spec update_entries_per_storage(provider_capacity_usage(), #{onedata:storage_id() => storage_capacity_usage()}) ->
    provider_capacity_usage().
update_entries_per_storage(ProviderCapacityUsage, NewUsagePerStorage) ->
    ProviderCapacityUsage#provider_capacity_usage{
        overfull = lists:any(fun(StorageUsage) ->
            StorageUsage#storage_capacity_usage.overfull
        end, maps:values(NewUsagePerStorage)),
        per_storage = NewUsagePerStorage
    }.


%% @private
-spec build_registry_update_result(registry(), registry()) -> registry_update_result().
build_registry_update_result(Identical, Identical) ->
    #capacity_usage_registry_update_result{
        new_registry = Identical,
        transitioned_to_overfull = [],
        transitioned_from_overfull = []
    };
build_registry_update_result(Prev, New) ->
    {PrevOverfull, PrevFree} = partition_providers_by_overfull(Prev),
    {NewOverfull, NewFree} = partition_providers_by_overfull(New),
    #capacity_usage_registry_update_result{
        new_registry = New,
        % only include providers that previously supported the space
        transitioned_to_overfull = lists_utils:intersect(PrevFree, NewOverfull),
        transitioned_from_overfull = lists_utils:intersect(PrevOverfull, NewFree)
    }.


%% @private
-spec partition_providers_by_overfull(registry()) ->
    {Overfull :: [onedata:provider_id()], Free :: [onedata:provider_id()]}.
partition_providers_by_overfull(Registry) ->
    Providers = maps:keys(Registry),
    lists:partition(fun(ProviderId) ->
        ProviderUsage = maps:get(ProviderId, Registry),
        ProviderUsage#provider_capacity_usage.overfull
    end, Providers).


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Reports do not require any transformation prior to JSON encoding/decoding,
%% but they are sanitized.
%% @end
%%--------------------------------------------------------------------
-spec sanitize_report(report()) -> report().
sanitize_report(Report) ->
    maps:map(fun
        (StId, Bytes) when is_binary(StId) andalso is_integer(Bytes) andalso Bytes >= 0 -> Bytes;
        (_, _) -> error(badarg)
    end, Report).


%% @private
-spec provider_capacity_usage_to_json(provider_capacity_usage()) -> json_utils:json_map().
provider_capacity_usage_to_json(ProviderUsage) ->
    #{
        <<"overfull">> => ProviderUsage#provider_capacity_usage.overfull,
        <<"perStorage">> => maps:map(fun(StorageId, StorageUsage) when is_binary(StorageId) ->
            storage_capacity_usage_to_json(StorageUsage)
        end, ProviderUsage#provider_capacity_usage.per_storage)
    }.


%% @private
-spec provider_capacity_usage_from_json(json_utils:json_map()) -> provider_capacity_usage().
provider_capacity_usage_from_json(ProviderUsageJson) ->
    #provider_capacity_usage{
        overfull = maps:get(<<"overfull">>, ProviderUsageJson),
        per_storage = maps:map(fun(StorageId, StorageUsageJson) when is_binary(StorageId) ->
            storage_capacity_usage_from_json(StorageUsageJson)
        end, maps:get(<<"perStorage">>, ProviderUsageJson))
    }.


%% @private
-spec storage_capacity_usage_to_json(storage_capacity_usage()) -> json_utils:json_map().
storage_capacity_usage_to_json(StorageUsage) ->
    #{
        <<"total">> => StorageUsage#storage_capacity_usage.total,
        <<"used">> => StorageUsage#storage_capacity_usage.used,
        <<"overfull">> => StorageUsage#storage_capacity_usage.overfull
    }.


%% @private
-spec storage_capacity_usage_from_json(json_utils:json_map()) -> storage_capacity_usage().
storage_capacity_usage_from_json(StorageUsageJson) ->
    #storage_capacity_usage{
        total = maps:get(<<"total">>, StorageUsageJson),
        used = maps:get(<<"used">>, StorageUsageJson),
        overfull = maps:get(<<"overfull">>, StorageUsageJson)
    }.
