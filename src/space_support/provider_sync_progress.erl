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
%%% summary of dbsync sequence numbers of peer providers supporting the space
%%% that were seen and processed by the provider.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_sync_progress).
-author("Lukasz Opiola").

-include("space_support/provider_sync_progress.hrl").

-type provider_id() :: onedata:service_id().
-export_type([provider_id/0]).

% Seq stands for database sequence - an increasing number that reflects the
% number of changed documents in database since the beginning of space support.
% Seen seq is the latest seq of peer provider that was seen by the subject
% provider. Because of the synchronization delays, the emitting provider may
% already be on a higher seq when the updates reach its peers. Latest emitted seq
% for a provider is the seen seq reported by each provider under own provider id.
-type seen_seq() :: non_neg_integer().
% Each seen_seq() is coupled with the timestamp denoting the moment when it was
% emitted by the provider (the timestamp is assigned once and does not change
% for the seq).
-type seq_timestamp() :: time:seconds().
% Difference between the latest seq emitted by a peer provider and the
% latest seq seen by the subject provider.
% Indicates the number of changes that need to be seen to become fully synced.
-type diff() :: non_neg_integer().
% Difference between the timestamp of the latest seq emitted by a peer provider
% and the timestamp of the latest seq seen by the subject provider (in seconds).
% Indicates the delay of changes propagation between the providers.
-type delay() :: time:seconds().
% Indicates desynchronization state - when the seq synchronization delay between
% providers exceeds a certain threshold. The provider marked with desync should
% be perceived as having considerably outdated view on current changes in the space.
-type desync() :: boolean().
% Expresses the sync progress of the subject provider with a peer provider.
% Holds precomputed sync progress summary that results from latest reports sent
% by providers (recomputed on every report).
% Each provider has a peer_summary record for itself that denotes the latest
% emitted seq and its timestamp.
-type peer_summary() :: #peer_summary{}.
-export_type([seen_seq/0, seq_timestamp/0, diff/0, delay/0, desync/0, peer_summary/0]).

% Denotes that a provider is in legacy version and does not recognize the sync
% progress model. Such provider does not report its progress and will have
% default entries in the registry (seq = 1) until it is upgraded. Such
% provider cannot be marked as active (is always perceived as joining).
-type legacy() :: boolean().
% Indicates that the provider is not yet actively supporting the space,
% but catching up with changes from other providers. The flag is to true set
% when the provider grants support for the space and hasn't had any active
% previous support. Then, it is set to false when the provider catches up with
% peer providers. This transition (true -> false) is a trigger for transition
% between joining and active support stage. After that, this flag remains
% unchanged during the support lifecycle.
-type joining() :: boolean().
% Denotes that the subject provider does not support the space anymore and
% the stored values are archival. Such entries are kept until all peer providers
% have seen the latest seq of the retired provider - and then removed in the
% process of pruning the registry.
-type archival() :: boolean().
% Summarizes the sync progress of a provider in the space, holds the summary
% per peer provider. Each provider in a space has its own corresponding
% provider_summary.
-type provider_summary() :: #provider_summary{}.
-export_type([legacy/0, joining/0, archival/0, provider_summary/0]).

% The registry object (one for a space) accumulates all information about sync
% progresses of all supporting (and archival) providers.
-type registry() :: #{provider_id() => provider_summary()}.
-export_type([registry/0]).

% This record carries information about a registry update outcome - the updated
% registry and a list of transitions from/to joining/desync state.
-type registry_update_result() :: #registry_update_result{}.
-export_type([registry_update_result/0]).

% Hold a sync progress report - reports are sent by each provider and used to
% compute the sync progress entries. In a space with N providers, each provider
% periodically sends a collective report that consists of N
% sync_progress_reports (for each provider, including self).
-record(report, {
    seen_seq :: seen_seq(),
    seq_timestamp :: seq_timestamp()
}).
-type report() :: #report{}.
-type collective_report() :: #{provider_id() => report()}.
-export_type([collective_report/0]).

-export([new/0]).
-export([lookup/2]).
-export([register_support/3]).
-export([register_unsupport/2]).
-export([register_legacy_support/3]).
-export([register_upgrade_of_legacy_support/2]).
-export([register_legacy_unsupport/2]).
-export([consume_collective_report/3]).
-export([build_collective_report/2]).
-export([prune_archival_entries/1]).
-export([collective_report_to_json/1, collective_report_from_json/1]).
-export([registry_to_json/1, registry_from_json/1]).

-define(DESYNC_THRESHOLD_SECONDS, ctool:get_env(provider_sync_progress_desync_threshold_seconds, 300)). % 5 minutes

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> registry().
new() ->
    #{}.


-spec lookup(registry(), provider_id()) -> {ok, provider_summary()} | error.
lookup(Registry, ProviderId) ->
    maps:find(ProviderId, Registry).


%%--------------------------------------------------------------------
%% @doc
%% Adds information to the registry about a new supporting provider. Must be
%% called only upon the first support (consecutive supports with different
%% storages must not be reported).
%% NOTE: must not be used for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec register_support(registry(), provider_id(), time:seconds()) -> registry_update_result().
register_support(Registry, NewProviderId, SpaceCreationTimestamp) ->
    register_support_internal(Registry, NewProviderId, SpaceCreationTimestamp, _Legacy = false).


%%--------------------------------------------------------------------
%% @doc
%% Adds information to the registry that a provider has ceased support for the
%% space. Must be called only when the provider revokes support with its last
%% storage (from all that were supporting the space).
%% NOTE: must not be used for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec register_unsupport(registry(), provider_id()) -> registry_update_result().
register_unsupport(Registry, ProviderId) ->
    ProviderSummary = case lookup(Registry, ProviderId) of
        error ->
            error({support_not_registered, ProviderId});
        {ok, #provider_summary{archival = true}} ->
            error({support_not_registered, ProviderId});
        {ok, #provider_summary{legacy = true}} ->
            error({legacy_provider, ProviderId});
        {ok, PS} ->
            PS
    end,
    NewRegistry = update_registry_entries(Registry, #{
        ProviderId => ProviderSummary#provider_summary{archival = true}
    }),
    build_registry_update_result(Registry, NewRegistry).


%%--------------------------------------------------------------------
%% @doc
%% Equivalent of register_support/3 for legacy providers.
%% Marks the provider as non-conformant to the new support model, which changes
%% the logic that is applied in regard to sync progress.
%% @end
%%--------------------------------------------------------------------
-spec register_legacy_support(registry(), provider_id(), time:seconds()) -> registry_update_result().
register_legacy_support(Registry, NewProviderId, SpaceCreationTimestamp) ->
    register_support_internal(Registry, NewProviderId, SpaceCreationTimestamp, _Legacy = true).


%%--------------------------------------------------------------------
%% @doc
%% Should be called when a legacy provider upgrades its support to the new model
%% and can start reporting its sync progress.
%% @end
%%--------------------------------------------------------------------
register_upgrade_of_legacy_support(Registry, ProviderId) ->
    ProviderSummary = case lookup(Registry, ProviderId) of
        error ->
            error({support_not_registered, ProviderId});
        {ok, #provider_summary{archival = true}} ->
            error({support_not_registered, ProviderId});
        {ok, #provider_summary{legacy = true} = PS} ->
            PS;
        {ok, _} ->
            error({not_a_legacy_provider, ProviderId})
    end,
    NewRegistry = update_registry_entries(Registry, #{
        ProviderId => update_provider_summary_entries(
            ProviderSummary#provider_summary{legacy = false}, #{}
        )
    }),
    build_registry_update_result(Registry, NewRegistry).


%%--------------------------------------------------------------------
%% @doc
%% Works like register_unsupport/2, but for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec register_legacy_unsupport(registry(), provider_id()) -> registry_update_result().
register_legacy_unsupport(Registry, ProviderId) ->
    ProviderSummary = case lookup(Registry, ProviderId) of
        error ->
            error({support_not_registered, ProviderId});
        {ok, #provider_summary{archival = true}} ->
            error({support_not_registered, ProviderId});
        {ok, #provider_summary{legacy = true} = PS} ->
            PS;
        {ok, _} ->
            error({not_a_legacy_provider, ProviderId})
    end,
    NewRegistry = update_registry_entries(Registry, #{
        ProviderId => ProviderSummary#provider_summary{archival = true}
    }),
    build_registry_update_result(Registry, NewRegistry).


%%--------------------------------------------------------------------
%% @doc
%% Consumes a report sent by a subject provider, updating its sync progress
%% towards peers, and the peers' progresses towards the subject.
%% NOTE: must not be used for legacy providers.
%% @end
%%--------------------------------------------------------------------
-spec consume_collective_report(registry(), provider_id(), collective_report()) -> registry_update_result().
consume_collective_report(Registry, SubjectId, CollectiveReport) ->
    case lookup(Registry, SubjectId) of
        {ok, #provider_summary{legacy = true}} ->
            error({legacy_provider, SubjectId});
        _ ->
            ok
    end,
    UpdatedRegistry = apply_collective_report(Registry, SubjectId, CollectiveReport),
    UpdatedProviderSummary = maps:get(SubjectId, UpdatedRegistry),
    FinalRegistry = UpdatedRegistry#{
        SubjectId => UpdatedProviderSummary#provider_summary{last_report = global_clock:timestamp_seconds()}
    },
    build_registry_update_result(Registry, FinalRegistry).


-spec build_collective_report([provider_id()], fun((provider_id()) -> {seen_seq(), seq_timestamp()})) ->
    collective_report().
build_collective_report(AllProviders, BuildReportForProvider) ->
    lists:foldl(fun(PeerId, Acc) ->
        {SeenSeq, SeqTimestamp} = BuildReportForProvider(PeerId),
        Acc#{PeerId => #report{seen_seq = SeenSeq, seq_timestamp = SeqTimestamp}}
    end, #{}, AllProviders).


%%--------------------------------------------------------------------
%% @doc
%% Prunes the registry by removing archival entries that can be safely removed,
%% i.e. all peer providers have seen the latest seq emitted by the provider to
%% be removed.
%% @end
%%--------------------------------------------------------------------
-spec prune_archival_entries(registry()) -> registry_update_result().
prune_archival_entries(Registry) ->
    AllProviders = maps:keys(Registry),
    ProvidersToPrune = maps:fold(fun
        (_SubjectId, #provider_summary{archival = false}, Acc) ->
            Acc;
        (SubjectId, ProviderSummary, Acc) ->
            #report{seen_seq = LatestSeq} = lookup_latest_report(ProviderSummary, SubjectId),
            AllProvidersCaughtUp = lists:all(fun(PeerId) ->
                case lookup_latest_report(Registry, PeerId, SubjectId) of
                    #report{seen_seq = LatestSeq} -> true;
                    _ -> false
                end
            end, lists:delete(SubjectId, AllProviders)),
            case AllProvidersCaughtUp of
                true -> [SubjectId | Acc];
                false -> Acc
            end
    end, [], Registry),

    WithoutRegistryEntries = maps:without(ProvidersToPrune, Registry),
    WithoutEntriesInPeers = maps:map(fun(_PeerId, ProviderSummary) ->
        ProviderSummary#provider_summary{
            per_peer = maps:without(ProvidersToPrune, ProviderSummary#provider_summary.per_peer)
        }
    end, WithoutRegistryEntries),
    build_registry_update_result(Registry, WithoutEntriesInPeers).


-spec collective_report_to_json(collective_report()) -> json_utils:json_map().
collective_report_to_json(CollectiveReport) ->
    maps:map(fun(_PrId, Report) ->
        report_to_json(Report)
    end, CollectiveReport).


-spec collective_report_from_json(json_utils:json_map()) -> collective_report().
collective_report_from_json(CollectiveReportJson) ->
    maps:map(fun(PrId, ReportJson) when is_binary(PrId) ->
        report_from_json(ReportJson)
    end, CollectiveReportJson).


-spec registry_to_json(registry()) -> json_utils:json_map().
registry_to_json(Registry) ->
    maps:map(fun(_PrId, ProviderSummary) ->
        provider_summary_to_json(ProviderSummary)
    end, Registry).


-spec registry_from_json(json_utils:json_map()) -> registry().
registry_from_json(RegistryJson) ->
    maps:map(fun(_PrId, ProviderSummaryJson) ->
        provider_summary_from_json(ProviderSummaryJson)
    end, RegistryJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec register_support_internal(registry(), provider_id(), time:seconds(), legacy()) ->
    registry_update_result().
register_support_internal(Registry, NewProviderId, SpaceCreationTimestamp, Legacy) ->
    case lookup(Registry, NewProviderId) of
        {ok, #provider_summary{archival = false}} ->
            error({support_already_registered, NewProviderId});
        {ok, Existing = #provider_summary{archival = true}} ->
            NewRegistry = update_registry_entries(Registry, #{
                NewProviderId => update_provider_summary_entries(
                    Existing#provider_summary{legacy = Legacy, joining = true, desync = true, archival = false},
                    #{}
                )
            }),
            build_registry_update_result(Registry, NewRegistry);
        error ->
            WithInitialPeerReports = update_registry_entries(Registry, maps:map(fun(_PeerId, ProviderSummaryOfPeer) ->
                update_provider_summary_entries(ProviderSummaryOfPeer, #{
                    NewProviderId => initial_peer_summary(SpaceCreationTimestamp)
                })
            end, Registry)),

            WithNewProvider = update_registry_entries(WithInitialPeerReports, #{
                NewProviderId => #provider_summary{
                    legacy = Legacy,
                    joining = true,
                    desync = true,
                    archival = false,
                    per_peer = #{}
                }
            }),

            AllProviders = maps:keys(WithNewProvider),
            CollectiveReport = build_collective_report(AllProviders, fun(_PeerId) ->
                {1, SpaceCreationTimestamp}
            end),
            NewRegistry = apply_collective_report(WithNewProvider, NewProviderId, CollectiveReport),
            build_registry_update_result(Registry, NewRegistry)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Applies a report of a subject provider, updating its sync progress
%% towards peers, and the peers' progresses towards the subject.
%% Returns the modified registry.
%% @end
%%--------------------------------------------------------------------
-spec apply_collective_report(registry(), provider_id(), collective_report()) -> registry().
apply_collective_report(Registry, SubjectId, CollectiveReport) ->
    AllProviders = maps:keys(Registry),
    % take only existing providers from the map
    WithExistingProviders = maps:with(AllProviders, CollectiveReport),

    % update the subject provider's summary, calculating new peer summary for each of its peers
    ProviderSummaryOfSubject = maps:get(SubjectId, Registry),
    NewPerPeerSummaries = maps:map(fun(PeerId, ReportOfSubject) ->
        LatestReportOfPeer = case PeerId of
            SubjectId -> ReportOfSubject;
            _ -> lookup_latest_report(Registry, PeerId, PeerId)
        end,
        build_peer_summary(ReportOfSubject, LatestReportOfPeer)
    end, WithExistingProviders),
    SubjectRecalculated = Registry#{
        SubjectId => update_provider_summary_entries(ProviderSummaryOfSubject, NewPerPeerSummaries)
    },

    % update the summaries of all subject's peers, calculating new sync progress
    % with the subject provider
    LatestReportOfSubject = maps:get(SubjectId, CollectiveReport),
    maps:fold(fun(PeerId, _Report, RegistryAcc) ->
        ProviderSummaryOfPeer = maps:get(PeerId, RegistryAcc),
        LatestReportOfPeer = lookup_latest_report(ProviderSummaryOfPeer, SubjectId),
        NewPeerSummaryOfSubject = build_peer_summary(LatestReportOfPeer, LatestReportOfSubject),
        RegistryAcc#{
            PeerId => update_provider_summary_entries(ProviderSummaryOfPeer, #{SubjectId => NewPeerSummaryOfSubject})
        }
    end, SubjectRecalculated, WithExistingProviders).


%% @private
-spec lookup_latest_report(registry(), provider_id(), provider_id()) -> report().
lookup_latest_report(Registry, SubjectId, PeerId) when is_map(Registry) ->
    lookup_latest_report(maps:get(SubjectId, Registry), PeerId).


%% @private
-spec lookup_latest_report(provider_summary(), provider_id()) -> report().
lookup_latest_report(#provider_summary{per_peer = PerPeer}, PeerId) ->
    #peer_summary{seen_seq = Seq, seq_timestamp = Timestamp} = maps:get(PeerId, PerPeer),
    #report{seen_seq = Seq, seq_timestamp = Timestamp}.


%% @private
-spec initial_peer_summary(time:seconds()) -> peer_summary().
initial_peer_summary(SpaceCreationTimestamp) ->
    #peer_summary{
        seen_seq = 1,
        seq_timestamp = SpaceCreationTimestamp,
        diff = 0,
        delay = 0,
        desync = false
    }.


%% @private
-spec build_peer_summary(report(), report()) -> peer_summary().
build_peer_summary(ReportOfSubject, ReportOfPeer) ->
    SeenSeq = ReportOfSubject#report.seen_seq,
    SeenSeqTimestamp = ReportOfSubject#report.seq_timestamp,
    % As reporting is done in intervals, for some time seq seen by the
    % subject might be higher than the one reported by the peer - for
    % that reason always the higher of the two is returned as latest.
    {LatestSeq, LatestSeqTimestamp} = case SeenSeq > ReportOfPeer#report.seen_seq of
        true ->
            {SeenSeq, SeenSeqTimestamp};
        false ->
            {ReportOfPeer#report.seen_seq, ReportOfPeer#report.seq_timestamp}
    end,
    Diff = LatestSeq - SeenSeq,
    Delay = LatestSeqTimestamp - SeenSeqTimestamp,
    #peer_summary{
        seen_seq = SeenSeq,
        seq_timestamp = SeenSeqTimestamp,
        diff = Diff,
        delay = Delay,
        desync = Delay > ?DESYNC_THRESHOLD_SECONDS
    }.


%% @private
-spec update_registry_entries(registry(), #{provider_id() => provider_summary()}) -> registry().
update_registry_entries(Registry, NewSummaries) ->
    maps:merge(Registry, NewSummaries).


%% @private
-spec update_provider_summary_entries(provider_summary(), #{provider_id() => peer_summary()}) -> provider_summary().
update_provider_summary_entries(ProviderSummary, NewPeerSummaries) ->
    NewPerPeer = maps:merge(ProviderSummary#provider_summary.per_peer, NewPeerSummaries),
    Desync = maps:fold(fun(_, #peer_summary{desync = DesyncTowardsPeer}, Acc) ->
        Acc orelse DesyncTowardsPeer
    end, false, NewPerPeer),
    ProviderSummary#provider_summary{
        per_peer = NewPerPeer,
        desync = Desync,
        % the joining flag is set upon first support and unset when the provider becomes
        % synced with its peers (desync flag becomes false), then it does not change anymore
        % NOTE that legacy providers cannot be marked as joined unless they upgrade, even
        % if they are not desynced
        joining = case ProviderSummary#provider_summary.joining of
            true -> Desync orelse ProviderSummary#provider_summary.legacy;
            false -> false
        end
    }.


%% @private
-spec build_registry_update_result(registry(), registry()) -> registry_update_result().
build_registry_update_result(Identical, Identical) ->
    #registry_update_result{
        new_registry = Identical,
        transitioned_from_joining = [],
        transitioned_to_desync = [],
        transitioned_from_desync = []
    };
build_registry_update_result(Prev, New) ->
    #registry_update_result{
        new_registry = New,
        transitioned_from_joining = find_transitions(Prev, New, fun(PS) -> not PS#provider_summary.joining end),
        transitioned_to_desync = find_transitions(Prev, New, fun(PS) -> PS#provider_summary.desync end),
        transitioned_from_desync = find_transitions(Prev, New, fun(PS) -> not PS#provider_summary.desync end)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds all providers that are currently in the desired state (Condition
%% evaluates to true), but weren't previously (Condition evaluates to false or
%% the provider was not present at all) - which means that there has been a
%% transition to desired state.
%% @end
%%--------------------------------------------------------------------
-spec find_transitions(registry(), registry(), fun((provider_summary()) -> boolean())) -> [provider_id()].
find_transitions(Previous, Current, Condition) ->
    CurrentlyInDesiredState = maps:fold(fun(ProviderId, ProviderSummary, Acc) ->
        case Condition(ProviderSummary) of
            true -> [ProviderId | Acc];
            false -> Acc
        end
    end, [], Current),
    lists:filter(fun(ProviderId) ->
        case lookup(Previous, ProviderId) of
            error ->
                % the transition is considered to have taken place if the provider hasn't existed before
                true;
            {ok, #provider_summary{archival = true}} ->
                % archival providers are treated as previously inexistent, but only if they rejoined
                case lookup(Current, ProviderId) of
                    {ok, #provider_summary{archival = true}} -> false;
                    {ok, #provider_summary{archival = false}} -> true
                end;
            {ok, ProviderSummary} ->
                not Condition(ProviderSummary)
        end
    end, CurrentlyInDesiredState).


%% @private
-spec report_to_json(report()) -> json_utils:json_map().
report_to_json(#report{seen_seq = SeenSeq, seq_timestamp = SeqTimestamp}) ->
    #{<<"seenSeq">> => SeenSeq, <<"seqTimestamp">> => SeqTimestamp}.


%% @private
-spec report_from_json(json_utils:json_map()) -> report().
report_from_json(#{<<"seenSeq">> := Seq, <<"seqTimestamp">> := Time}) when is_integer(Seq), is_integer(Time) ->
    #report{seen_seq = Seq, seq_timestamp = Time}.


%% @private
-spec provider_summary_to_json(provider_summary()) -> json_utils:json_map().
provider_summary_to_json(ProviderSummary) ->
    #{
        <<"legacy">> => ProviderSummary#provider_summary.legacy,
        <<"joining">> => ProviderSummary#provider_summary.joining,
        <<"archival">> => ProviderSummary#provider_summary.archival,
        <<"desync">> => ProviderSummary#provider_summary.desync,
        <<"lastReport">> => ProviderSummary#provider_summary.last_report,
        <<"perPeer">> => maps:map(fun(_PrId, PeerSummary) ->
            peer_summary_to_json(PeerSummary)
        end, ProviderSummary#provider_summary.per_peer)
    }.


%% @private
-spec provider_summary_from_json(json_utils:json_map()) -> provider_summary().
provider_summary_from_json(ProviderSummaryJson) ->
    #provider_summary{
        legacy = maps:get(<<"legacy">>, ProviderSummaryJson),
        joining = maps:get(<<"joining">>, ProviderSummaryJson),
        archival = maps:get(<<"archival">>, ProviderSummaryJson),
        desync = maps:get(<<"desync">>, ProviderSummaryJson),
        last_report = maps:get(<<"lastReport">>, ProviderSummaryJson),
        per_peer = maps:map(fun(_PrId, PeerSummaryJson) ->
            peer_summary_from_json(PeerSummaryJson)
        end, maps:get(<<"perPeer">>, ProviderSummaryJson))
    }.


%% @private
-spec peer_summary_to_json(peer_summary()) -> json_utils:json_map().
peer_summary_to_json(ProviderSummary) ->
    #{
        <<"seenSeq">> => ProviderSummary#peer_summary.seen_seq,
        <<"seqTimestamp">> => ProviderSummary#peer_summary.seq_timestamp,
        <<"diff">> => ProviderSummary#peer_summary.diff,
        <<"delay">> => ProviderSummary#peer_summary.delay,
        <<"desync">> => ProviderSummary#peer_summary.desync
    }.


%% @private
-spec peer_summary_from_json(json_utils:json_map()) -> peer_summary().
peer_summary_from_json(ProviderSummaryJson) ->
    #peer_summary{
        seen_seq = maps:get(<<"seenSeq">>, ProviderSummaryJson),
        seq_timestamp = maps:get(<<"seqTimestamp">>, ProviderSummaryJson),
        diff = maps:get(<<"diff">>, ProviderSummaryJson),
        delay = maps:get(<<"delay">>, ProviderSummaryJson),
        desync = maps:get(<<"desync">>, ProviderSummaryJson)
    }.
