%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to space parameters - defining
%%% the rights of a provider to write data in the space and its policy
%%% concerning metadata synchronization.
%%% @end
%%%-------------------------------------------------------------------
-module(support_parameters).
-author("Lukasz Opiola").

-export([new_registry/0]).
-export([lookup_by_provider/2]).
-export([update_for_provider/3]).
-export([remove_for_provider/2]).
-export([build/2]).
-export([get_data_write/1, get_metadata_replication/1]).
-export([serialize/1, deserialize/1]).
-export([to_json/1, from_json/1]).
-export([registry_to_json/1, registry_from_json/1]).

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

% The registry object (one for a space) accumulates all information about
% support parameters of all supporting providers.
-type registry() :: #{onedata:provider_id() => parameters()}.
-export_type([registry/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new_registry() -> registry().
new_registry() ->
    #{}.


-spec lookup_by_provider(registry(), onedata:provider_id()) -> {ok, parameters()} | error.
lookup_by_provider(Registry, ProviderId) ->
    maps:find(ProviderId, Registry).


-spec update_for_provider(registry(), onedata:provider_id(), parameters()) -> registry().
update_for_provider(Registry, ProviderId, Parameters) ->
    Registry#{ProviderId => Parameters}.


-spec remove_for_provider(registry(), onedata:provider_id()) -> registry().
remove_for_provider(Registry, ProviderId) ->
    maps:remove(ProviderId, Registry).


-spec build(data_write(), metadata_replication()) -> parameters().
build(DataWrite, MetadataReplication) ->
    #space_support_parameters{data_write = DataWrite, metadata_replication = MetadataReplication}.


-spec get_data_write(parameters()) -> data_write().
get_data_write(#space_support_parameters{data_write = DW}) -> DW.


-spec get_metadata_replication(parameters()) -> metadata_replication().
get_metadata_replication(#space_support_parameters{metadata_replication = MR}) -> MR.


-spec serialize(parameters()) -> <<_:16>>.
serialize(#space_support_parameters{data_write = DW, metadata_replication = MR}) ->
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


-spec deserialize(<<_:16>>) -> parameters().
deserialize(<<SerializedDW:1/binary, SerializedMR/binary>>) ->
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


-spec to_json(parameters()) -> json_utils:json_map().
to_json(#space_support_parameters{data_write = DW, metadata_replication = MR}) ->
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


-spec from_json(json_utils:json_map()) -> parameters().
from_json(Parameters) ->
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


-spec registry_to_json(registry()) -> json_utils:json_map().
registry_to_json(Registry) ->
    maps:map(fun(_ProviderId, Parameters) ->
        to_json(Parameters)
    end, Registry).


-spec registry_from_json(json_utils:json_map()) -> registry().
registry_from_json(RegistryJson) ->
    maps:map(fun(_ProviderId, Parameters) ->
        from_json(Parameters)
    end, RegistryJson).
