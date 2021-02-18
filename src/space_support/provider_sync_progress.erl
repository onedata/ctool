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
%%%
%%% Legacy providers have a dedicated API ensuring backward compatibility and
%%% creating default entries, as they do not recognize the sync progress concept.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_sync_progress).
-author("Lukasz Opiola").

-include("space_support/provider_sync_progress.hrl").

-export([new_registry/1]).
-export([lookup/2]).
-export([register_support/3]).
-export([register_unsupport/3]).
-export([register_upgrade_of_legacy_support/2]).
-export([consume_collective_report/3]).
-export([build_collective_report/2]).
-export([prune_archival_entries/1]).
-export([collective_report_to_json/1, collective_report_from_json/1]).
-export([registry_to_json/1, registry_from_json/1]).

% Seq stands for database sequence - an increasing number that reflects the
% number of changed documents in database since the beginning of space support.
% This modules uses the concept of "seen" seq - the latest seq of a peer provider
% that was seen by the subject provider. Because of the synchronization delays,
% the emitting provider may already be on a higher seq when the updates reach
% its peers. Latest emitted seq for a provider is the seen seq reported by each
% provider under own provider id.
-type seq() :: non_neg_integer().
% Each seq() is coupled with the timestamp denoting the moment when the
% database document associated with the sequence number has been persisted by
% the provider (the timestamp is assigned once and does not change for the seq).
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
% Each provider has a #sync_progress_with_peer{} record for itself that denotes
% the latest emitted seq and its timestamp.
-type sync_progress_with_peer() :: #sync_progress_with_peer{}.
-export_type([seq/0, seq_timestamp/0, diff/0, delay/0, desync/0, sync_progress_with_peer/0]).

% Denotes that a provider is in legacy version and does not recognize the sync
% progress model. Such provider does not report its progress and will have
% default entries in the registry (seq = 1). It will be marked as joining until
% it is upgraded - then is starts catching up with modern providers.
-type legacy() :: boolean().
% Indicates that the provider is not yet actively supporting the space,
% but catching up with changes from other providers. The flag is set to true
% when the provider grants support for the space and hasn't had any active
% previous support. Then, it is set to false when the provider catches up with
% peer providers. This transition (true -> false) is a trigger for transition
% between joining and active support stage. After that, this flag remains
% unchanged during the support lifecycle.
-type joining() :: boolean().
% Denotes that the subject provider does not support the space anymore and
% the stored values are archival - the registry state is "frozen" at the
% moment of space unsupport. Such entries are kept until all peer providers
% have seen the latest seq of the retired provider - and then removed in the
% process of pruning the registry.
-type archival() :: boolean().
% Summarizes the sync progress of a provider in the space, holds the entries
% per peer provider. Each provider in a space has its own corresponding
% provider_sync_progress.
-type provider_sync_progress() :: #provider_sync_progress{}.
-export_type([legacy/0, joining/0, archival/0, provider_sync_progress/0]).

% The registry object (one for a space) accumulates all information about sync
% progresses of all supporting (and archival) providers.
-type registry() :: #sync_progress_registry{}.
-type per_provider() :: #{onedata:provider_id() => provider_sync_progress()}.
-export_type([registry/0, per_provider/0]).

% Holds a sync progress report - reports are sent by each provider and used to
% compute the sync progress entries. In a space with N providers, each provider
% periodically sends a collective report that consists of N
% sync_progress_reports (for each provider, including self).
-record(report, {
    seen_seq :: seq(),
    seq_timestamp :: seq_timestamp()
}).
-type report() :: #report{}.
-type collective_report() :: #{onedata:provider_id() => report()}.
-export_type([collective_report/0]).

% This record carries information about a registry update outcome - the updated
% registry and a list of transitions from/to joining/desync state.
-type registry_update_result() :: #sync_progress_registry_update_result{}.
-export_type([registry_update_result/0]).

-define(DESYNC_THRESHOLD_SECONDS, ctool:get_env(provider_sync_progress_desync_threshold_seconds, 300)). % 5 minutes

%%%===================================================================
%%% API
%%%===================================================================

-spec new_registry(time:seconds()) -> registry().
new_registry(SpaceCreationTime) ->
    #sync_progress_registry{
        space_creation_time = SpaceCreationTime,
        per_provider = #{}
    }.


-spec lookup(registry(), onedata:provider_id()) -> {ok, provider_sync_progress()} | error.
lookup(Registry, ProviderId) ->
    maps:find(ProviderId, Registry#sync_progress_registry.per_provider).


%%--------------------------------------------------------------------
%% @doc
%% Adds information to the registry about a new supporting provider. Must be
%% called only upon the first support (consecutive supports with different
%% storages must not be reported).
%% @end
%%--------------------------------------------------------------------
-spec register_support(registry(), support_stage:support_model(), onedata:provider_id()) ->
    registry_update_result().
register_support(Registry, SupportModel, NewProviderId) ->
    ensure_current_support_model(Registry, NewProviderId, none),
    IsLegacy = (SupportModel == legacy),
    SpaceCreationTime = Registry#sync_progress_registry.space_creation_time,
    PerProvider = Registry#sync_progress_registry.per_provider,
    case lookup(Registry, NewProviderId) of
        {ok, Existing = #provider_sync_progress{archival = true}} ->
            NewPerProvider = PerProvider#{
                NewProviderId => update_entries_per_peer(
                    Existing#provider_sync_progress{
                        legacy = IsLegacy,
                        joining = true,
                        desync = true,
                        archival = false},
                    #{}
                )
            },
            build_registry_update_result(Registry, NewPerProvider);
        error ->
            WithInitialPeerReports = maps:merge(PerProvider, maps:map(fun(_PeerId, ProviderSyncProgressOfPeer) ->
                update_entries_per_peer(ProviderSyncProgressOfPeer, #{
                    NewProviderId => initial_sync_progress_with_peer(SpaceCreationTime)
                })
            end, PerProvider)),

            WithNewProvider = WithInitialPeerReports#{
                NewProviderId => #provider_sync_progress{
                    legacy = IsLegacy,
                    joining = true,
                    desync = true,
                    archival = false,
                    per_peer = #{}
                }
            },

            AllProviders = maps:keys(WithNewProvider),
            CollectiveReport = build_collective_report(AllProviders, fun(_PeerId) ->
                {1, SpaceCreationTime}
            end),
            NewPerProvider = apply_collective_report(WithNewProvider, NewProviderId, CollectiveReport),
            build_registry_update_result(Registry, NewPerProvider)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds information to the registry that a provider has ceased support for the
%% space. Must be called only when the provider revokes support with its last
%% storage (from all that were supporting the space).
%% @end
%%--------------------------------------------------------------------
-spec register_unsupport(registry(), support_stage:support_model(), onedata:provider_id()) ->
    registry_update_result().
register_unsupport(Registry, SupportModel, ProviderId) ->
    ensure_current_support_model(Registry, ProviderId, SupportModel),
    NewPerProvider = maps:update_with(ProviderId, fun(ProviderSyncProgress) ->
        ProviderSyncProgress#provider_sync_progress{archival = true}
    end, Registry#sync_progress_registry.per_provider),
    build_registry_update_result(Registry, NewPerProvider).


%%--------------------------------------------------------------------
%% @doc
%% Should be called when a legacy provider upgrades its support to the new model
%% and can start reporting its sync progress.
%% @end
%%--------------------------------------------------------------------
-spec register_upgrade_of_legacy_support(registry(), onedata:provider_id()) -> registry_update_result().
register_upgrade_of_legacy_support(Registry, ProviderId) ->
    ensure_current_support_model(Registry, ProviderId, legacy),
    NewPerProvider = maps:update_with(ProviderId, fun(ProviderSyncProgress) ->
        update_entries_per_peer(ProviderSyncProgress#provider_sync_progress{legacy = false}, #{})
    end, Registry#sync_progress_registry.per_provider),
    build_registry_update_result(Registry, NewPerProvider).


%%--------------------------------------------------------------------
%% @doc
%% Consumes a report sent by a subject provider, updating its sync progress
%% towards peers, and the peers' progresses towards the subject.
%% @end
%%--------------------------------------------------------------------
-spec consume_collective_report(registry(), onedata:provider_id(), collective_report()) -> registry_update_result().
consume_collective_report(Registry, SubjectId, CollectiveReport) ->
    ensure_current_support_model(Registry, SubjectId, modern),
    SanitizedReport = sanitize_collective_report(CollectiveReport, Registry#sync_progress_registry.space_creation_time),
    UpdatedPerProvider = apply_collective_report(Registry#sync_progress_registry.per_provider, SubjectId, SanitizedReport),
    FinalPerProvider = maps:update_with(SubjectId, fun(ProviderSyncProgress) ->
        ProviderSyncProgress#provider_sync_progress{last_report = global_clock:timestamp_seconds()}
    end, UpdatedPerProvider),
    build_registry_update_result(Registry, FinalPerProvider).


-spec build_collective_report([onedata:provider_id()], fun((onedata:provider_id()) -> {seq(), seq_timestamp()})) ->
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
    PerProvider = Registry#sync_progress_registry.per_provider,
    AllProviders = maps:keys(PerProvider),
    ProvidersToPrune = maps:fold(fun
        (_SubjectId, #provider_sync_progress{archival = false}, Acc) ->
            Acc;
        (SubjectId, ProviderSyncProgress, Acc) ->
            #report{seen_seq = LatestSeq} = lookup_latest_report(ProviderSyncProgress, SubjectId),
            AllProvidersCaughtUp = lists:all(fun(PeerId) ->
                case lookup_latest_report(PerProvider, PeerId, SubjectId) of
                    #report{seen_seq = LatestSeq} -> true;
                    _ -> false
                end
            end, lists:delete(SubjectId, AllProviders)),
            case AllProvidersCaughtUp of
                true -> [SubjectId | Acc];
                false -> Acc
            end
    end, [], PerProvider),

    WithoutRegistryEntries = maps:without(ProvidersToPrune, PerProvider),
    WithoutEntriesInPeers = maps:map(fun(_PeerId, ProviderSyncProgress) ->
        ProviderSyncProgress#provider_sync_progress{
            per_peer = maps:without(ProvidersToPrune, ProviderSyncProgress#provider_sync_progress.per_peer)
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
    #{
        <<"spaceCreationTime">> => Registry#sync_progress_registry.space_creation_time,
        <<"perProvider">> => maps:map(fun(_PrId, ProviderSyncProgress) ->
            provider_sync_progress_to_json(ProviderSyncProgress)
        end, Registry#sync_progress_registry.per_provider)
    }.


-spec registry_from_json(json_utils:json_map()) -> registry().
registry_from_json(RegistryJson) ->
    #sync_progress_registry{
        space_creation_time = maps:get(<<"spaceCreationTime">>, RegistryJson),
        per_provider = maps:map(fun(_PrId, ProviderSyncProgressJson) ->
            provider_sync_progress_from_json(ProviderSyncProgressJson)
        end, maps:get(<<"perProvider">>, RegistryJson))
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks that given provider is marked to have the expected support generation
%% in given registry and raises an error if the expectation is not met.
%% The 'none' atom indicates that there is expected to be no support at all.
%% @end
%%--------------------------------------------------------------------
-spec ensure_current_support_model(registry(), onedata:provider_id(), none | support_stage:support_model()) ->
    ok | no_return().
ensure_current_support_model(Registry, ProviderId, ExpectedSupportModel) ->
    case {ExpectedSupportModel, lookup(Registry, ProviderId)} of
        {none, {ok, #provider_sync_progress{archival = false}}} ->
            error({support_already_registered, ProviderId});
        {none, _} ->
            ok;

        {_, error} ->
            error({support_not_registered, ProviderId});
        {_, {ok, #provider_sync_progress{archival = true}}} ->
            error({support_not_registered, ProviderId});

        {modern, {ok, #provider_sync_progress{legacy = true}}} ->
            error({legacy_provider, ProviderId});
        {modern, {ok, _}} ->
            ok;

        {legacy, {ok, #provider_sync_progress{legacy = false}}} ->
            error({not_a_legacy_provider, ProviderId});
        {legacy, {ok, _}} ->
            ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes sure that the timestamps in the report are not lower than the SpaceCreationTime.
%% Such reports may occur for example just after supporting a space - when providers do
%% not know any sequences of others, they send reports with timestamps set to zero.
%% This ensures that calculated delay between any two providers cannot exceed the space's
%% length of life.
%% @end
%%--------------------------------------------------------------------
-spec sanitize_collective_report(collective_report(), time:seconds()) -> collective_report().
sanitize_collective_report(CollectiveReport, SpaceCreationTime) ->
    maps:map(fun
        (_PrId, R = #report{seq_timestamp = T}) when T < SpaceCreationTime ->
            R#report{seq_timestamp = SpaceCreationTime};
        (_PrId, Report) ->
            Report
    end, CollectiveReport).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Applies a report of a subject provider, updating its sync progress
%% towards peers, and the peers' progresses towards the subject.
%% Returns the modified registry.
%% @end
%%--------------------------------------------------------------------
-spec apply_collective_report(per_provider(), onedata:provider_id(), collective_report()) -> per_provider().
apply_collective_report(PerProvider, SubjectId, CollectiveReport) ->
    AllProviders = maps:keys(PerProvider),
    % take only existing providers from the map
    WithExistingProviders = maps:with(AllProviders, CollectiveReport),

    % update the subject provider's sync progress, calculating sync progress with each of its peers
    ProviderSyncProgressOfSubject = maps:get(SubjectId, PerProvider),
    NewSubjectsProgressPerPeer = maps:map(fun(PeerId, ReportOfSubject) ->
        LatestReportOfPeer = case PeerId of
            SubjectId -> ReportOfSubject;
            _ -> lookup_latest_report(PerProvider, PeerId, PeerId)
        end,
        calculate_sync_progress_with_peer(ReportOfSubject, LatestReportOfPeer)
    end, WithExistingProviders),
    SubjectRecalculated = PerProvider#{
        SubjectId => update_entries_per_peer(ProviderSyncProgressOfSubject, NewSubjectsProgressPerPeer)
    },

    % update the sync progress of all subject's peers, calculating new sync progress with the subject provider
    LatestReportOfSubject = maps:get(SubjectId, CollectiveReport),
    maps:fold(fun(PeerId, _Report, RegistryAcc) ->
        ProviderSyncProgressOfPeer = maps:get(PeerId, RegistryAcc),
        LatestReportOfPeer = lookup_latest_report(ProviderSyncProgressOfPeer, SubjectId),
        NewPeersSyncProgressWithSubject = calculate_sync_progress_with_peer(LatestReportOfPeer, LatestReportOfSubject),
        RegistryAcc#{
            PeerId => update_entries_per_peer(ProviderSyncProgressOfPeer, #{
                SubjectId => NewPeersSyncProgressWithSubject
            })
        }
    end, SubjectRecalculated, WithExistingProviders).


%% @private
-spec lookup_latest_report(per_provider(), onedata:provider_id(), onedata:provider_id()) -> report().
lookup_latest_report(PerProvider, SubjectId, PeerId) when is_map(PerProvider) ->
    ProviderSyncProgress = maps:get(SubjectId, PerProvider),
    lookup_latest_report(ProviderSyncProgress, PeerId).


%% @private
-spec lookup_latest_report(provider_sync_progress(), onedata:provider_id()) -> report().
lookup_latest_report(#provider_sync_progress{per_peer = PerPeer}, PeerId) ->
    #sync_progress_with_peer{seen_seq = Seq, seq_timestamp = Timestamp} = maps:get(PeerId, PerPeer),
    #report{seen_seq = Seq, seq_timestamp = Timestamp}.


%% @private
-spec initial_sync_progress_with_peer(time:seconds()) -> sync_progress_with_peer().
initial_sync_progress_with_peer(SpaceCreationTime) ->
    #sync_progress_with_peer{
        seen_seq = 1,
        seq_timestamp = SpaceCreationTime,
        diff = 0,
        delay = 0,
        desync = false
    }.


%% @private
-spec calculate_sync_progress_with_peer(report(), report()) -> sync_progress_with_peer().
calculate_sync_progress_with_peer(ReportOfSubject, ReportOfPeer) ->
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
    #sync_progress_with_peer{
        seen_seq = SeenSeq,
        seq_timestamp = SeenSeqTimestamp,
        diff = Diff,
        delay = Delay,
        desync = Delay > ?DESYNC_THRESHOLD_SECONDS
    }.


%% @private
-spec update_entries_per_peer(provider_sync_progress(), #{onedata:provider_id() => sync_progress_with_peer()}) ->
    provider_sync_progress().
update_entries_per_peer(ProviderSyncProgress, NewProgressPerPeer) ->
    NewPerPeer = maps:merge(ProviderSyncProgress#provider_sync_progress.per_peer, NewProgressPerPeer),
    Desync = maps:fold(fun(_, #sync_progress_with_peer{desync = DesyncTowardsPeer}, Acc) ->
        Acc orelse DesyncTowardsPeer
    end, false, NewPerPeer),
    ProviderSyncProgress#provider_sync_progress{
        per_peer = NewPerPeer,
        desync = Desync,
        % the joining flag is set upon first support and unset when the provider becomes
        % synced with its peers (desync flag becomes false), then it does not change anymore
        % NOTE that legacy providers cannot be marked as joined unless they upgrade, even
        % if they are not desynced
        joining = case ProviderSyncProgress#provider_sync_progress.joining of
            true -> Desync orelse ProviderSyncProgress#provider_sync_progress.legacy;
            false -> false
        end
    }.


%% @private
-spec build_registry_update_result(registry(), per_provider()) ->
    registry_update_result().
build_registry_update_result(PreviousRegistry = #sync_progress_registry{per_provider = Identical}, Identical) ->
    #sync_progress_registry_update_result{
        new_registry = PreviousRegistry,
        transitioned_from_joining = [],
        transitioned_to_desync = [],
        transitioned_from_desync = []
    };
build_registry_update_result(PreviousRegistry = #sync_progress_registry{per_provider = Prev}, New) ->
    #sync_progress_registry_update_result{
        new_registry = PreviousRegistry#sync_progress_registry{per_provider = New},
        transitioned_from_joining = find_transitions(Prev, New, fun(PS) -> not PS#provider_sync_progress.joining end),
        transitioned_to_desync = find_transitions(Prev, New, fun(PS) -> PS#provider_sync_progress.desync end),
        transitioned_from_desync = find_transitions(Prev, New, fun(PS) -> not PS#provider_sync_progress.desync end)
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
-spec find_transitions(per_provider(), per_provider(), fun((provider_sync_progress()) -> boolean())) ->
    [onedata:provider_id()].
find_transitions(Previous, Current, Condition) ->
    CurrentlyInDesiredState = maps:fold(fun(ProviderId, ProviderSyncProgress, Acc) ->
        case Condition(ProviderSyncProgress) of
            true -> [ProviderId | Acc];
            false -> Acc
        end
    end, [], Current),
    lists:filter(fun(ProviderId) ->
        case maps:find(ProviderId, Previous) of
            error ->
                % the transition is considered to have taken place if the provider hasn't existed before
                true;
            {ok, #provider_sync_progress{archival = true}} ->
                % archival providers are treated as previously inexistent, but only if they rejoined
                case maps:find(ProviderId, Current) of
                    {ok, #provider_sync_progress{archival = true}} -> false;
                    {ok, #provider_sync_progress{archival = false}} -> true
                end;
            {ok, ProviderSyncProgress} ->
                not Condition(ProviderSyncProgress)
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
-spec provider_sync_progress_to_json(provider_sync_progress()) -> json_utils:json_map().
provider_sync_progress_to_json(ProviderSyncProgress) ->
    #{
        <<"legacy">> => ProviderSyncProgress#provider_sync_progress.legacy,
        <<"joining">> => ProviderSyncProgress#provider_sync_progress.joining,
        <<"archival">> => ProviderSyncProgress#provider_sync_progress.archival,
        <<"desync">> => ProviderSyncProgress#provider_sync_progress.desync,
        <<"lastReport">> => ProviderSyncProgress#provider_sync_progress.last_report,
        <<"perPeer">> => maps:map(fun(_PrId, SyncProgressWithPeer) ->
            sync_progress_with_peer_to_json(SyncProgressWithPeer)
        end, ProviderSyncProgress#provider_sync_progress.per_peer)
    }.


%% @private
-spec provider_sync_progress_from_json(json_utils:json_map()) -> provider_sync_progress().
provider_sync_progress_from_json(ProviderSyncProgressJson) ->
    #provider_sync_progress{
        legacy = maps:get(<<"legacy">>, ProviderSyncProgressJson),
        joining = maps:get(<<"joining">>, ProviderSyncProgressJson),
        archival = maps:get(<<"archival">>, ProviderSyncProgressJson),
        desync = maps:get(<<"desync">>, ProviderSyncProgressJson),
        last_report = maps:get(<<"lastReport">>, ProviderSyncProgressJson),
        per_peer = maps:map(fun(_PrId, SyncProgressWithPeerJson) ->
            sync_progress_with_peer_from_json(SyncProgressWithPeerJson)
        end, maps:get(<<"perPeer">>, ProviderSyncProgressJson))
    }.


%% @private
-spec sync_progress_with_peer_to_json(sync_progress_with_peer()) -> json_utils:json_map().
sync_progress_with_peer_to_json(SyncProgressWithPeer) ->
    #{
        <<"seenSeq">> => SyncProgressWithPeer#sync_progress_with_peer.seen_seq,
        <<"seqTimestamp">> => SyncProgressWithPeer#sync_progress_with_peer.seq_timestamp,
        <<"diff">> => SyncProgressWithPeer#sync_progress_with_peer.diff,
        <<"delay">> => SyncProgressWithPeer#sync_progress_with_peer.delay,
        <<"desync">> => SyncProgressWithPeer#sync_progress_with_peer.desync
    }.


%% @private
-spec sync_progress_with_peer_from_json(json_utils:json_map()) -> sync_progress_with_peer().
sync_progress_with_peer_from_json(SyncProgressWithPeerJson) ->
    #sync_progress_with_peer{
        seen_seq = maps:get(<<"seenSeq">>, SyncProgressWithPeerJson),
        seq_timestamp = maps:get(<<"seqTimestamp">>, SyncProgressWithPeerJson),
        diff = maps:get(<<"diff">>, SyncProgressWithPeerJson),
        delay = maps:get(<<"delay">>, SyncProgressWithPeerJson),
        desync = maps:get(<<"desync">>, SyncProgressWithPeerJson)
    }.
