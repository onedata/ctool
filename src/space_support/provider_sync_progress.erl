%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to provider sync progress
%%% statistics in a space. The sync progress of a provider is represented as a
%%% summary of dbsync sequence numbers of other providers supporting the space
%%% that were seen and processed by the provider.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_sync_progress).
-author("Lukasz Opiola").

-type provider_id() :: onedata:service_id().
% Denotes the database sequence - an increasing number that reflects the number
% of changed documents in database since the beginning of space support.
-type seq() :: non_neg_integer().
-type timestamp() :: time_utils:seconds().
% Stores information about latest sequence currently known for given provider.
% Each provider has its own sync progress stats, which allows checking if it is
% up to date with other providers or falling behind.
% Includes information about self, which is always up to date and used for
% comparing with the sequence other providers know about the provider.
% Timestamp denotes the time when the sequence was seen.
-type stats() :: #{provider_id() => {seq(), timestamp()}}.
% Holds sync progress stats for each provider supporting the space.
-type per_provider() :: #{provider_id() => stats()}.
-export_type([stats/0, per_provider/0]).

-export([lookup_by_provider/2]).
-export([coalesce_all/2]).
-export([update_for_provider/4]).
-export([inspect_progress_between/3]).
-export([to_json/1, from_json/1]).
-export([per_provider_to_json/1, per_provider_from_json/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec lookup_by_provider(per_provider(), provider_id()) ->
    {ok, stats()} | error.
lookup_by_provider(PerProvider, ProviderId) ->
    maps:find(ProviderId, PerProvider).


%%--------------------------------------------------------------------
%% @doc
%% Coalesces the sync progress for all providers (see update_for_provider/4).
%% @end
%%--------------------------------------------------------------------
-spec coalesce_all(per_provider(), [provider_id()]) ->
    per_provider().
coalesce_all(PerProvider, AllProviders) ->
    lists:foldl(fun(ProviderId, AccPerProvider) ->
        PreviousSyncProgress = maps:get(ProviderId, AccPerProvider, #{}),
        update_for_provider(AccPerProvider, AllProviders, ProviderId, PreviousSyncProgress)
    end, PerProvider, AllProviders).


%%--------------------------------------------------------------------
%% @doc
%% Updates sync progress for a provider. First, coalesces given sync progress
%% stats knowing the list of all space supporting providers,
%% adding missing entries and removing superfluous entries.
%% @end
%%--------------------------------------------------------------------
-spec update_for_provider(per_provider(), [provider_id()], provider_id(), stats()) ->
    per_provider().
update_for_provider(PerProvider, AllProviders, ProviderId, SyncProgressStats) ->
    % Take only supporting providers from the map
    WithExistingProviders = maps:with(AllProviders, SyncProgressStats),
    % Add supporting providers that were not in the map, with default seq and timestamp
    MissingProviders = lists:subtract(AllProviders, maps:keys(WithExistingProviders)),
    Coalesced = lists:foldl(fun(MissingProvider, Acc) ->
        Acc#{MissingProvider => {1, 0}}
    end, WithExistingProviders, MissingProviders),
    PerProvider#{ProviderId => Coalesced}.


%%--------------------------------------------------------------------
%% @doc
%% Inspects the SubjectProvider's knowledge of OtherProvider's dbsync sequences.
%% Returns two values (sequence numbers):
%%   Known - the highest sequence of OtherProvider known by SubjectProvider
%%   Latest - the current sequence of OtherProvider, as reported to Onezone by OtherProvider
%% As reporting is done in intervals, for some time Known seq might be higher
%% than the one reported by OtherProvider - for that reason always the higher of
%% the two is returned as latest.
%% Can be called for the same provider, in such case will return the same value twice.
%% @end
%%--------------------------------------------------------------------
-spec inspect_progress_between(per_provider(), provider_id(), provider_id()) ->
    {Known :: seq(), Latest :: seq()}.
inspect_progress_between(PerProvider, SubjectProvider, OtherProvider) ->
    #{OtherProvider := {Known, _T1}} = maps:get(SubjectProvider, PerProvider),
    #{OtherProvider := {Latest, _T2}} = maps:get(OtherProvider, PerProvider),
    {Known, max(Known, Latest)}.


-spec to_json(stats()) -> json_utils:json_map().
to_json(SyncProgressStats) ->
    maps:map(fun(_PrId, {Seq, Timestamp}) ->
        #{<<"seq">> => Seq, <<"timestamp">> => Timestamp}
    end, SyncProgressStats).


-spec from_json(json_utils:json_map()) -> stats().
from_json(SyncProgressStatsJson) ->
    maps:map(fun(PrId, #{<<"seq">> := Seq, <<"timestamp">> := Time}) when is_binary(PrId), is_integer(Seq), is_integer(Time) ->
        {Seq, Time}
    end, SyncProgressStatsJson).


-spec per_provider_to_json(per_provider()) -> json_utils:json_map().
per_provider_to_json(PerProvider) ->
    maps:map(fun(_PrId, SyncProgressStats) ->
        to_json(SyncProgressStats)
    end, PerProvider).


-spec per_provider_from_json(json_utils:json_map()) -> per_provider().
per_provider_from_json(PerProviderJson) ->
    maps:map(fun(_PrId, SyncProgressStatsJson) ->
        from_json(SyncProgressStatsJson)
    end, PerProviderJson).
