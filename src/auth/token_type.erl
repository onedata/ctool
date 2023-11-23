%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This modules encapsulates definitions and logic related to token types,
%%% including decoding and encoding to different formats.
%%% @end
%%%--------------------------------------------------------------------
-module(token_type).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("errors.hrl").

% Type of the token as recognized across Onedata components
-type type() :: #access_token_typespec{} | #identity_token_typespec{} | #invite_token_typespec{}.
% Subtypes of invite tokens
-type invite_type() :: ?USER_JOIN_GROUP | ?GROUP_JOIN_GROUP
| ?USER_JOIN_SPACE | ?GROUP_JOIN_SPACE | ?SUPPORT_SPACE | ?HARVESTER_JOIN_SPACE
| ?USER_JOIN_CLUSTER | ?GROUP_JOIN_CLUSTER
| ?REGISTER_ONEPROVIDER
| ?USER_JOIN_HARVESTER | ?GROUP_JOIN_HARVESTER | ?SPACE_JOIN_HARVESTER
| ?USER_JOIN_ATM_INVENTORY | ?GROUP_JOIN_ATM_INVENTORY.
% TODO VFS-9587 Parameters specific for given invite type - this field will be used in the
% future versions when data write and metadata replication levels are supported
-type invite_parameters() :: undefined.

-export_type([type/0, invite_type/0, invite_parameters/0]).

%% API
-export([sanitize/1, sanitize_invite_type/1]).
-export([to_json/1, from_json/1]).
-export([serialize/1, deserialize/1]).
-export([invite_type_to_str/1, invite_type_from_str/1]).
-export([to_printable/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec sanitize(type() | json_utils:json_term()) -> {true, type()} | false.
sanitize(Type = ?ACCESS_TOKEN(undefined)) ->
    {true, Type};
sanitize(Type = ?ACCESS_TOKEN(SessionId)) when size(SessionId) > 0 ->
    {true, Type};
sanitize(Type = ?IDENTITY_TOKEN) ->
    {true, Type};
sanitize(Type = ?INVITE_TOKEN) ->
    try
        Type = deserialize(serialize(Type)),
        {true, Type}
    catch _:_ ->
        false
    end;
sanitize(AsJson) when is_map(AsJson) ->
    try
        sanitize(from_json(AsJson))
    catch _:_ ->
        false
    end;
sanitize(_) ->
    false.


-spec sanitize_invite_type(invite_type() | binary()) ->
    {true, invite_type()} | false.
sanitize_invite_type(InviteType) when is_atom(InviteType) ->
    case lists:member(InviteType, invite_types()) of
        true -> {true, InviteType};
        false -> false
    end;
sanitize_invite_type(InviteType) when is_binary(InviteType) ->
    try
        sanitize_invite_type(invite_type_from_str(InviteType))
    catch _:_ ->
        false
    end;
sanitize_invite_type(_) ->
    false.


-spec to_json(type()) -> json_utils:json_term().
to_json(?ACCESS_TOKEN(undefined)) ->
    #{<<"accessToken">> => #{}};
to_json(?ACCESS_TOKEN(SessionId)) when is_binary(SessionId) ->
    #{<<"accessToken">> => #{
        <<"sessionId">> => SessionId
    }};
to_json(?IDENTITY_TOKEN) ->
    #{<<"identityToken">> => #{}};
to_json(?INVITE_TOKEN(InviteType, EntityId, Parameters)) ->
    TargetJsonKey = invite_target_json_key(InviteType),
    #{<<"inviteToken">> => #{
        <<"inviteType">> => invite_type_to_str(InviteType),
        TargetJsonKey => EntityId,
        <<"parameters">> => invite_parameters_to_json(InviteType, Parameters)
    }}.


-spec from_json(json_utils:json_term()) -> type().
from_json(#{<<"accessToken">> := #{<<"sessionId">> := SessionId}}) when is_binary(SessionId) ->
    ?ACCESS_TOKEN(SessionId);
from_json(#{<<"accessToken">> := EmptyMap}) when map_size(EmptyMap) == 0 ->
    ?ACCESS_TOKEN(undefined);
from_json(#{<<"identityToken">> := _}) ->
    ?IDENTITY_TOKEN;
from_json(#{<<"inviteToken">> := InviteTokenTypeData = #{<<"inviteType">> := InviteTypeStr}}) ->
    InviteType = invite_type_from_str(InviteTypeStr),
    TargetJsonKey = invite_target_json_key(InviteType),
    EntityId = maps:get(TargetJsonKey, InviteTokenTypeData),
    ParametersJson = maps:get(<<"parameters">>, InviteTokenTypeData, #{}),
    Parameters = invite_parameters_from_json(InviteType, ParametersJson),
    ?INVITE_TOKEN(InviteType, EntityId, Parameters).


-spec serialize(type()) -> binary().
serialize(?ACCESS_TOKEN(undefined)) ->
    <<"act">>;
serialize(?ACCESS_TOKEN(SessionId)) when is_binary(SessionId) ->
    <<"act:", SessionId/binary>>;
serialize(?IDENTITY_TOKEN) ->
    <<"idn">>;
serialize(?INVITE_TOKEN(InviteType, EntityId, Params)) ->
    <<
        (serialize_invite_type(InviteType))/binary,
        ":",
        EntityId/binary,
        ":",
        (serialize_invite_parameters(InviteType, Params))/binary
    >>;
serialize(_) ->
    error(badarg).


-spec deserialize(binary()) -> type().
deserialize(<<"act">>) ->
    ?ACCESS_TOKEN(undefined);
deserialize(<<"act:", SessionId/binary>>) when size(SessionId) > 0 ->
    ?ACCESS_TOKEN(SessionId);
deserialize(<<"idn">>) ->
    ?IDENTITY_TOKEN;
deserialize(SerializedInviteTokenType) ->
    try
        case binary:split(SerializedInviteTokenType, <<":">>, [global]) of
            [SerializedInviteType, EntityId, SerializedParams] when size(EntityId) > 0 ->
                InviteType = deserialize_invite_type(SerializedInviteType),
                ?INVITE_TOKEN(
                    InviteType,
                    EntityId,
                    deserialize_invite_parameters(InviteType, SerializedParams)
                )
        end
    catch _:_ ->
        error(badarg)
    end.


-spec invite_type_to_str(invite_type()) -> binary().
invite_type_to_str(?USER_JOIN_GROUP) -> <<"userJoinGroup">>;
invite_type_to_str(?GROUP_JOIN_GROUP) -> <<"groupJoinGroup">>;
invite_type_to_str(?USER_JOIN_SPACE) -> <<"userJoinSpace">>;
invite_type_to_str(?GROUP_JOIN_SPACE) -> <<"groupJoinSpace">>;
invite_type_to_str(?SUPPORT_SPACE) -> <<"supportSpace">>;
invite_type_to_str(?HARVESTER_JOIN_SPACE) -> <<"harvesterJoinSpace">>;
invite_type_to_str(?REGISTER_ONEPROVIDER) -> <<"registerOneprovider">>;
invite_type_to_str(?USER_JOIN_CLUSTER) -> <<"userJoinCluster">>;
invite_type_to_str(?GROUP_JOIN_CLUSTER) -> <<"groupJoinCluster">>;
invite_type_to_str(?USER_JOIN_HARVESTER) -> <<"userJoinHarvester">>;
invite_type_to_str(?GROUP_JOIN_HARVESTER) -> <<"groupJoinHarvester">>;
invite_type_to_str(?SPACE_JOIN_HARVESTER) -> <<"spaceJoinHarvester">>;
invite_type_to_str(?USER_JOIN_ATM_INVENTORY) -> <<"userJoinAtmInventory">>;
invite_type_to_str(?GROUP_JOIN_ATM_INVENTORY) -> <<"groupJoinAtmInventory">>.


-spec invite_type_from_str(binary()) -> invite_type().
invite_type_from_str(<<"userJoinGroup">>) -> ?USER_JOIN_GROUP;
invite_type_from_str(<<"groupJoinGroup">>) -> ?GROUP_JOIN_GROUP;
invite_type_from_str(<<"userJoinSpace">>) -> ?USER_JOIN_SPACE;
invite_type_from_str(<<"groupJoinSpace">>) -> ?GROUP_JOIN_SPACE;
invite_type_from_str(<<"supportSpace">>) -> ?SUPPORT_SPACE;
invite_type_from_str(<<"harvesterJoinSpace">>) -> ?HARVESTER_JOIN_SPACE;
invite_type_from_str(<<"registerOneprovider">>) -> ?REGISTER_ONEPROVIDER;
invite_type_from_str(<<"userJoinCluster">>) -> ?USER_JOIN_CLUSTER;
invite_type_from_str(<<"groupJoinCluster">>) -> ?GROUP_JOIN_CLUSTER;
invite_type_from_str(<<"userJoinHarvester">>) -> ?USER_JOIN_HARVESTER;
invite_type_from_str(<<"groupJoinHarvester">>) -> ?GROUP_JOIN_HARVESTER;
invite_type_from_str(<<"spaceJoinHarvester">>) -> ?SPACE_JOIN_HARVESTER;
invite_type_from_str(<<"userJoinAtmInventory">>) -> ?USER_JOIN_ATM_INVENTORY;
invite_type_from_str(<<"groupJoinAtmInventory">>) -> ?GROUP_JOIN_ATM_INVENTORY.


-spec to_printable(type()) -> string().
to_printable(?ACCESS_TOKEN(undefined)) ->
    "access token";
to_printable(?ACCESS_TOKEN(SessionId)) when is_binary(SessionId) ->
    str_utils:format("access token for session \"~s\"", [SessionId]);
to_printable(?IDENTITY_TOKEN) ->
    "identity token";
to_printable(?INVITE_TOKEN(?USER_JOIN_GROUP, GroupId)) ->
    str_utils:format("invite token for a user to join group \"~s\"", [GroupId]);
to_printable(?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupId)) ->
    str_utils:format("invite token for a group to join group \"~s\"", [GroupId]);
to_printable(?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceId)) ->
    str_utils:format("invite token for a user to join space \"~s\"", [SpaceId]);
to_printable(?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceId)) ->
    str_utils:format("invite token for a group to join space \"~s\"", [SpaceId]);
to_printable(?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId)) ->
    str_utils:format("invite token to grant support for space \"~s\"", [SpaceId]);
to_printable(?INVITE_TOKEN(?HARVESTER_JOIN_SPACE, SpaceId)) ->
    str_utils:format("invite token for a harvester to become a metadata sink for space \"~s\"", [SpaceId]);
to_printable(?INVITE_TOKEN(?REGISTER_ONEPROVIDER, AdminUserId)) ->
    str_utils:format("invite token to register a Oneprovider for admin user \"~s\"", [AdminUserId]);
to_printable(?INVITE_TOKEN(?USER_JOIN_CLUSTER, ClusterId)) ->
    str_utils:format("invite token for a user to join cluster \"~s\"", [ClusterId]);
to_printable(?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, ClusterId)) ->
    str_utils:format("invite token for a group to join cluster \"~s\"", [ClusterId]);
to_printable(?INVITE_TOKEN(?USER_JOIN_HARVESTER, HarvesterId)) ->
    str_utils:format("invite token for a user to join harvester \"~s\"", [HarvesterId]);
to_printable(?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, HarvesterId)) ->
    str_utils:format("invite token for a group to join harvester \"~s\"", [HarvesterId]);
to_printable(?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, HarvesterId)) ->
    str_utils:format("invite token for a space to become a metadata source for harvester \"~s\"", [HarvesterId]);
to_printable(?INVITE_TOKEN(?USER_JOIN_ATM_INVENTORY, AtmInventoryId)) ->
    str_utils:format("invite token for a user to join automation inventory \"~s\"", [AtmInventoryId]);
to_printable(?INVITE_TOKEN(?GROUP_JOIN_ATM_INVENTORY, AtmInventoryId)) ->
    str_utils:format("invite token for a group to join automation inventory \"~s\"", [AtmInventoryId]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec invite_types() -> [invite_type()].
invite_types() -> [
    ?USER_JOIN_GROUP,
    ?GROUP_JOIN_GROUP,
    ?USER_JOIN_SPACE,
    ?GROUP_JOIN_SPACE,
    ?SUPPORT_SPACE,
    ?HARVESTER_JOIN_SPACE,
    ?REGISTER_ONEPROVIDER,
    ?USER_JOIN_CLUSTER,
    ?GROUP_JOIN_CLUSTER,
    ?USER_JOIN_HARVESTER,
    ?GROUP_JOIN_HARVESTER,
    ?SPACE_JOIN_HARVESTER,
    ?USER_JOIN_ATM_INVENTORY,
    ?GROUP_JOIN_ATM_INVENTORY
].


%% @private
-spec serialize_invite_type(invite_type()) -> binary().
serialize_invite_type(?USER_JOIN_GROUP) -> <<"ujg">>;
serialize_invite_type(?GROUP_JOIN_GROUP) -> <<"gjg">>;
serialize_invite_type(?USER_JOIN_SPACE) -> <<"ujs">>;
serialize_invite_type(?GROUP_JOIN_SPACE) -> <<"gjs">>;
serialize_invite_type(?SUPPORT_SPACE) -> <<"ssp">>;
serialize_invite_type(?HARVESTER_JOIN_SPACE) -> <<"hjs">>;
serialize_invite_type(?REGISTER_ONEPROVIDER) -> <<"rop">>;
serialize_invite_type(?USER_JOIN_CLUSTER) -> <<"ujc">>;
serialize_invite_type(?GROUP_JOIN_CLUSTER) -> <<"gjc">>;
serialize_invite_type(?USER_JOIN_HARVESTER) -> <<"ujh">>;
serialize_invite_type(?GROUP_JOIN_HARVESTER) -> <<"gjh">>;
serialize_invite_type(?SPACE_JOIN_HARVESTER) -> <<"sjh">>;
serialize_invite_type(?USER_JOIN_ATM_INVENTORY) -> <<"uji">>;
serialize_invite_type(?GROUP_JOIN_ATM_INVENTORY) -> <<"gji">>.


%% @private
-spec deserialize_invite_type(binary()) -> invite_type().
deserialize_invite_type(<<"ujg">>) -> ?USER_JOIN_GROUP;
deserialize_invite_type(<<"gjg">>) -> ?GROUP_JOIN_GROUP;
deserialize_invite_type(<<"ujs">>) -> ?USER_JOIN_SPACE;
deserialize_invite_type(<<"gjs">>) -> ?GROUP_JOIN_SPACE;
deserialize_invite_type(<<"ssp">>) -> ?SUPPORT_SPACE;
deserialize_invite_type(<<"hjs">>) -> ?HARVESTER_JOIN_SPACE;
deserialize_invite_type(<<"rop">>) -> ?REGISTER_ONEPROVIDER;
deserialize_invite_type(<<"ujc">>) -> ?USER_JOIN_CLUSTER;
deserialize_invite_type(<<"gjc">>) -> ?GROUP_JOIN_CLUSTER;
deserialize_invite_type(<<"ujh">>) -> ?USER_JOIN_HARVESTER;
deserialize_invite_type(<<"gjh">>) -> ?GROUP_JOIN_HARVESTER;
deserialize_invite_type(<<"sjh">>) -> ?SPACE_JOIN_HARVESTER;
deserialize_invite_type(<<"uji">>) -> ?USER_JOIN_ATM_INVENTORY;
deserialize_invite_type(<<"gji">>) -> ?GROUP_JOIN_ATM_INVENTORY.


%% @private
-spec serialize_invite_parameters(invite_type(), invite_parameters()) -> binary().
serialize_invite_parameters(?SUPPORT_SPACE, _) ->
    % NOTE: this ensures backward compatibility of tokens
    %   * software version 20.02.* can decode such an invite token with
    %     default support params (data_write = global, metadata_replication = eager)
    %   * software version 21.02.* can decode such an invite token and
    %     simply ignores the support parameters, which are no longer used
    <<"ge">>;
serialize_invite_parameters(_, undefined) ->
    <<"">>.


%% @private
-spec deserialize_invite_parameters(invite_type(), binary()) -> invite_parameters().
deserialize_invite_parameters(_, _) ->
    undefined.


%% @private
-spec invite_parameters_to_json(invite_type(), invite_parameters()) -> json_utils:json_map().
invite_parameters_to_json(_, undefined) ->
    #{}.


%% @private
-spec invite_parameters_from_json(invite_type(), json_utils:json_map()) -> invite_parameters().
invite_parameters_from_json(_, _) ->
    undefined.


-spec invite_target_json_key(invite_type()) -> binary().
invite_target_json_key(?USER_JOIN_GROUP) -> <<"groupId">>;
invite_target_json_key(?GROUP_JOIN_GROUP) -> <<"groupId">>;
invite_target_json_key(?USER_JOIN_SPACE) -> <<"spaceId">>;
invite_target_json_key(?GROUP_JOIN_SPACE) -> <<"spaceId">>;
invite_target_json_key(?SUPPORT_SPACE) -> <<"spaceId">>;
invite_target_json_key(?HARVESTER_JOIN_SPACE) -> <<"spaceId">>;
invite_target_json_key(?REGISTER_ONEPROVIDER) -> <<"adminUserId">>;
invite_target_json_key(?USER_JOIN_CLUSTER) -> <<"clusterId">>;
invite_target_json_key(?GROUP_JOIN_CLUSTER) -> <<"clusterId">>;
invite_target_json_key(?USER_JOIN_HARVESTER) -> <<"harvesterId">>;
invite_target_json_key(?GROUP_JOIN_HARVESTER) -> <<"harvesterId">>;
invite_target_json_key(?SPACE_JOIN_HARVESTER) -> <<"harvesterId">>;
invite_target_json_key(?USER_JOIN_ATM_INVENTORY) -> <<"atmInventoryId">>;
invite_target_json_key(?GROUP_JOIN_ATM_INVENTORY) -> <<"atmInventoryId">>.