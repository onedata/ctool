%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to space support - parameters of
%%% support, dbsync state and provider's support state in a space.
%%% @end
%%%-------------------------------------------------------------------
-module(space_support).
-author("Lukasz Opiola").

% Parameters of space support, inscribed in ?SUPPORT_SPACE tokens and assigned
% to the provider when supporting a space
-record(space_support_parameters, {
    data_write = global :: data_write(),
    metadata_replication = eager :: metadata_replication()
}).
-type parameters() :: #space_support_parameters{}.
-type data_write() :: global | none.
-type metadata_replication() :: eager | lazy | none.
-export_type([parameters/0, data_write/0, metadata_replication/0]).

% Denotes the database sequence - an increasing number that reflects the number
% of changed documents in database since the beginning of space support.
-type seq() :: non_neg_integer().
-type seq_per_provider() :: #{provider_id() => seq()}.
% Stores information about latest sequence currently known for given provider.
% Each provider has its own dbsync_state, which allows checking if it is
% up to date with other providers or falling behind.
% Includes information about self, which is always up to date and used for
% comparing with which sequence other providers know about the provider.
% Paired with timestamp of the last dbsync_state report that was posted.
-type dbsync_state() :: {time_utils:seconds(), seq_per_provider()}.
-export_type([dbsync_state/0, seq_per_provider/0]).

% Current support state of a provider:
%   joining - phase of initial setup after a space is supported and when the
%             provider is catching up with dbsync changes from other providers
%   active - the provider is operational and up-to-date with other providers
%   desync - the provider is falling behind other providers in terms of dbsync
%             changes and has outdated view on resources in the space
%   remodelling - the provider has recently ceased support for the space with
%             one of its storages, but still supports it with another one, which
%             requires remodelling (moving around) of data on the storages
%   retiring - the provider has recently ceased support for the space with all
%             of its storages and is now safely migrating data and metadata to
%             other providers
%   retired - the provider has supported the space in the past
-type support_state() :: joining | active | desync | remodelling | retiring | retired.
-export_type([support_state/0]).

-type provider_id() :: onedata:service_id().
-type parameters_per_provider() :: #{provider_id() => parameters()}.
-type dbsync_state_per_provider() :: #{provider_id() => dbsync_state()}.
-type support_state_per_provider() :: #{provider_id() => support_state()}.
-export_type([parameters_per_provider/0, dbsync_state_per_provider/0, support_state_per_provider/0]).

-export([build_parameters/2]).
-export([lookup_parameters_for_provider/2]).
-export([update_parameters_for_provider/3]).
-export([remove_parameters_for_provider/2]).
-export([get_data_write/1, get_metadata_replication/1]).
-export([serialize_parameters/1, deserialize_parameters/1]).
-export([parameters_to_json/1, parameters_from_json/1]).
-export([parameters_per_provider_to_json/1, parameters_per_provider_from_json/1]).

-export([lookup_dbsync_state_for_provider/2]).
-export([build_dbsync_state/3]).
-export([inspect_dbsync_state_between/3]).
-export([update_dbsync_state_for_provider/3]).
-export([dbsync_state_per_provider_to_json/1]).

-export([lookup_support_state_for_provider/2]).
-export([update_support_state_for_provider/3]).
-export([support_state_per_provider_to_json/1, support_state_per_provider_from_json/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec build_parameters(data_write(), metadata_replication()) -> parameters().
build_parameters(DataWrite, MetadataReplication) ->
    #space_support_parameters{data_write = DataWrite, metadata_replication = MetadataReplication}.


-spec lookup_parameters_for_provider(parameters_per_provider(), provider_id()) ->
    {ok, parameters()} | error.
lookup_parameters_for_provider(ParametersPerProvider, ProviderId) ->
    maps:find(ProviderId, ParametersPerProvider).


-spec update_parameters_for_provider(parameters_per_provider(), provider_id(), parameters()) ->
    parameters_per_provider().
update_parameters_for_provider(ParametersPerProvider, ProviderId, Parameters) ->
    ParametersPerProvider#{ProviderId => Parameters}.


-spec remove_parameters_for_provider(parameters_per_provider(), provider_id()) ->
    parameters_per_provider().
remove_parameters_for_provider(ParametersPerProvider, ProviderId) ->
    maps:remove(ProviderId, ParametersPerProvider).


-spec get_data_write(parameters()) -> data_write().
get_data_write(#space_support_parameters{data_write = DW}) -> DW.


-spec get_metadata_replication(parameters()) -> metadata_replication().
get_metadata_replication(#space_support_parameters{metadata_replication = MR}) -> MR.


-spec serialize_parameters(parameters()) -> <<_:16>>.
serialize_parameters(#space_support_parameters{data_write = DW, metadata_replication = MR}) ->
    SerializedDW = case DW of
        global -> <<"g">>;
        none -> <<"n">>
    end,
    SerializedMR = case MR of
        eager -> <<"e">>;
        lazy -> <<"l">>;
        none -> <<"n">>
    end,
    <<SerializedDW/binary, SerializedMR/binary>>.


-spec deserialize_parameters(<<_:16>>) -> parameters().
deserialize_parameters(<<SerializedDW:1/binary, SerializedMR/binary>>) ->
    DW = case SerializedDW of
        <<"g">> -> global;
        <<"n">> -> none
    end,
    MR = case SerializedMR of
        <<"e">> -> eager;
        <<"l">> -> lazy;
        <<"n">> -> none
    end,
    #space_support_parameters{data_write = DW, metadata_replication = MR}.


-spec parameters_to_json(parameters()) -> json_utils:json_map().
parameters_to_json(#space_support_parameters{data_write = DW, metadata_replication = MR}) ->
    DWStr = case DW of
        global -> <<"global">>;
        none -> <<"none">>
    end,
    MRStr = case MR of
        eager -> <<"eager">>;
        lazy -> <<"lazy">>;
        none -> <<"none">>
    end,
    #{<<"dataWrite">> => DWStr, <<"metadataReplication">> => MRStr}.


-spec parameters_from_json(json_utils:json_map()) -> parameters().
parameters_from_json(Parameters) ->
    DW = case maps:get(<<"dataWrite">>, Parameters, <<"global">>) of
        <<"global">> -> global;
        <<"none">> -> none
    end,
    MR = case maps:get(<<"metadataReplication">>, Parameters, <<"eager">>) of
        <<"eager">> -> eager;
        <<"lazy">> -> lazy;
        <<"none">> -> none
    end,
    #space_support_parameters{data_write = DW, metadata_replication = MR}.


-spec parameters_per_provider_to_json(parameters_per_provider()) -> json_utils:json_map().
parameters_per_provider_to_json(ParametersPerProvider) ->
    maps:map(fun(_ProviderId, Parameters) ->
        parameters_to_json(Parameters)
    end, ParametersPerProvider).


-spec parameters_per_provider_from_json(json_utils:json_map()) -> parameters_per_provider().
parameters_per_provider_from_json(JsonParametersPerProvider) ->
    maps:map(fun(_ProviderId, Parameters) ->
        parameters_from_json(Parameters)
    end, JsonParametersPerProvider).


-spec lookup_dbsync_state_for_provider(dbsync_state_per_provider(), provider_id()) ->
    {ok, dbsync_state()} | error.
lookup_dbsync_state_for_provider(DBSyncStatePerProvider, ProviderId) ->
    maps:find(ProviderId, DBSyncStatePerProvider).


-spec build_dbsync_state(time_utils:seconds(), seq_per_provider(), [provider_id()]) -> dbsync_state().
build_dbsync_state(Timestamp, SeqPerProvider, AllProviders) ->
    % Take only supporting providers from the map
    FilteredForExistingProviders = maps:with(AllProviders, SeqPerProvider),
    % Add supporting providers that were not in the map with default seq
    MissingKeys = lists:subtract(AllProviders, maps:keys(FilteredForExistingProviders)),
    Coalesced = lists:foldl(fun(MissingProvider, Acc) ->
        Acc#{MissingProvider => 0}
    end, FilteredForExistingProviders, MissingKeys),
    {Timestamp, Coalesced}.


%%--------------------------------------------------------------------
%% @doc
%% Inspects the SubjectProvider's knowledge of OtherProvider's dbsync sequences.
%% Returns two values (sequence numbers):
%%   Known - the highest sequence of OtherProvider known by SubjectProvider
%%   Latest - the current sequence of OtherProvider, as reported to Onezone
%% As reporting is done in intervals, for some time Known seq might be higher
%% than the one reported by OtherProvider - for that reason always the higher of
%% the two is returned as latest.
%% Can be called for the same provider, in such case will return the same value twice.
%% @end
%%--------------------------------------------------------------------
-spec inspect_dbsync_state_between(dbsync_state_per_provider(), provider_id(), provider_id()) ->
    {Known :: seq(), Latest :: seq()}.
inspect_dbsync_state_between(DBSyncStatePerProvider, SubjectProvider, OtherProvider) ->
    {_, #{OtherProvider := Known}} = maps:get(SubjectProvider, DBSyncStatePerProvider),
    {_, #{OtherProvider := Latest}} = maps:get(OtherProvider, DBSyncStatePerProvider),
    {Known, max(Known, Latest)}.


-spec update_dbsync_state_for_provider(dbsync_state_per_provider(), provider_id(), dbsync_state()) ->
    dbsync_state_per_provider().
update_dbsync_state_for_provider(DBSyncStatePerProvider, ProviderId, DBSyncState) ->
    DBSyncStatePerProvider#{ProviderId => DBSyncState}.


-spec dbsync_state_per_provider_to_json(dbsync_state_per_provider()) -> json_utils:json_map().
dbsync_state_per_provider_to_json(DBSyncStatePerProvider) ->
    maps:map(fun(_ProviderId, {Timestamp, SeqPerProvider}) ->
        #{<<"lastUpdate">> => Timestamp, <<"seqPerProvider">> => SeqPerProvider}
    end, DBSyncStatePerProvider).


-spec lookup_support_state_for_provider(support_state_per_provider(), provider_id()) ->
    {ok, support_state()} | error.
lookup_support_state_for_provider(SupportStatePerProvider, ProviderId) ->
    maps:find(ProviderId, SupportStatePerProvider).


-spec update_support_state_for_provider(support_state_per_provider(), provider_id(), support_state()) ->
    support_state_per_provider().
update_support_state_for_provider(SupportStatePerProvider, ProviderId, SupportState) ->
    SupportStatePerProvider#{ProviderId => SupportState}.


-spec support_state_per_provider_to_json(support_state_per_provider()) -> json_utils:json_map().
support_state_per_provider_to_json(ParametersPerProvider) ->
    maps:map(fun(_ProviderId, Parameters) ->
        serialize_support_state(Parameters)
    end, ParametersPerProvider).


-spec support_state_per_provider_from_json(json_utils:json_map()) -> support_state_per_provider().
support_state_per_provider_from_json(JsonParametersPerProvider) ->
    maps:map(fun(_ProviderId, Parameters) ->
        deserialize_support_state(Parameters)
    end, JsonParametersPerProvider).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec serialize_support_state(support_state()) -> binary().
serialize_support_state(joining) -> <<"joining">>;
serialize_support_state(active) -> <<"active">>;
serialize_support_state(desync) -> <<"desync">>;
serialize_support_state(remodelling) -> <<"remodelling">>;
serialize_support_state(retiring) -> <<"retiring">>;
serialize_support_state(retired) -> <<"retired">>.


%% @private
-spec deserialize_support_state(binary()) -> support_state().
deserialize_support_state(<<"joining">>) -> joining;
deserialize_support_state(<<"active">>) -> active;
deserialize_support_state(<<"desync">>) -> desync;
deserialize_support_state(<<"remodelling">>) -> remodelling;
deserialize_support_state(<<"retiring">>) -> retiring;
deserialize_support_state(<<"retired">>) -> retired.
