%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for Spaces management in OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_spaces).

-include("oz/oz_runner.hrl").
-include("oz/oz_users.hrl").
-include("oz/oz_spaces.hrl").
-include("oz/oz_groups.hrl").
-include("oz/oz_providers.hrl").

%% API
-export([create/2, remove/2, get_details/2, modify_details/3]).
-export([get_shares/2]).
-export([get_invite_user_token/2, get_invite_group_token/2, get_invite_provider_token/2]).
-export([remove_user/3, get_users/2, get_user_details/3, get_user_privileges/3,
    get_effective_user_privileges/3, set_user_privileges/4]).
-export([remove_group/3, get_groups/2, get_group_details/3, get_group_privileges/3,
    set_group_privileges/4]).
-export([remove_provider/3, get_providers/2, get_provider_details/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc For:
%% * user it creates new Space and makes him the only member and administrator
%%   of created Space. Parameters should contain: "name" of new Space.
%% * provider it creates new Space and makes provider support created Space.
%%   User/group that has given provider a token receives all privileges
%%   for new Space. Parameters should contain: "name" of new Space and
%%   "token" associated with user/group.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: oz_endpoint:auth(), Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/spaces",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        SpaceId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Removes Space.
%% @end
%%--------------------------------------------------------------------
-spec remove(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
remove(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about Space.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
get_details(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Props = json_utils:decode(ResponseBody),
        % Get default values of space_details record
        SpaceDetails = #space_details{
            id = lists_utils:key_get(<<"spaceId">>, Props),
            name = lists_utils:key_get(<<"name">>, Props),
            providers_supports = lists_utils:key_get(
                <<"providersSupports">>, Props, []),
            shares = lists_utils:key_get(<<"shares">>, Props, [])
        },
        {ok, SpaceDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about Space. Parameters may contain:
%% "name" of Space.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Auth, SpaceId, Parameters) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, patch, Body),
        ok
    end).


%%--------------------------------------------------------------------
%% @doc Returns share list of a Space.
%% @end
%%--------------------------------------------------------------------
-spec get_shares(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    {ok, ShareIds :: [binary()]} | {error, Reason :: term()}.
get_shares(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/shares",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Props = json_utils:decode(ResponseBody),
        ShareIds = lists_utils:key_get(<<"shares">>, Props),
        {ok, ShareIds}
    end).


%%--------------------------------------------------------------------
%% @doc Returns token that allows user to join Space.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_user_token(Auth :: oz_endpoint:auth(),
    SpaceId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_user_token(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows group to join Space.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_group_token(Auth :: oz_endpoint:auth(),
    SpaceId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_group_token(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows provider to support Space.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_provider_token(Auth :: oz_endpoint:auth(),
    SpaceId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_provider_token(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Removes user from Space.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    UserId :: binary()) -> ok | {error, Reason :: term()}.
remove_user(Auth, SpaceId, UserId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of users that belong to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_users(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserIds = lists_utils:key_get(<<"users">>, Proplist),
        {ok, UserIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about user that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_user_details(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    UserId :: binary()) ->
    {ok, UserDetails :: #user_details{}} | {error, Reason :: term()}.
get_user_details(Auth, SpaceId, UserId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserDetails = #user_details{
            id = lists_utils:key_get(<<"userId">>, Proplist),
            name = lists_utils:key_get(<<"name">>, Proplist)
        },
        {ok, UserDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of user that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    UserId :: binary()) ->
    {ok, Privileges :: [privileges:space_privilege()]} | {error, Reason :: term()}.
get_user_privileges(Auth, SpaceId, UserId) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
        binary_to_list(UserId) ++ "/privileges",
    get_privileges(Auth, URN).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of effective Space user.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_user_privileges(Auth :: oz_endpoint:auth(),
    GroupId :: binary(), UserId :: binary()) ->
    {ok, Privileges :: [privileges:space_privilege()]} | {error, Reason :: term()}.
get_effective_user_privileges(Auth, SpaceId, UserId) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
        binary_to_list(UserId) ++ "/privileges?effective",
    get_privileges(Auth, URN).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges for user that belongs to Space.
%% Parameters should contain: list of "privileges" of type privileges:space_privilege().
%% @end
%%--------------------------------------------------------------------
-spec set_user_privileges(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    UserId :: binary(), Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_user_privileges(Auth, SpaceId, UserId, Parameters) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
        binary_to_list(UserId) ++ "/privileges",
    set_privileges(Auth, URN, Parameters).

%%--------------------------------------------------------------------
%% @doc Removes group from Space.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    GroupId :: binary()) -> ok | {error, Reason :: term()}.
remove_group(Auth, SpaceId, GroupId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
            binary_to_list(GroupId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of groups that belong to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_groups(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupIds = lists_utils:key_get(<<"groups">>, Proplist),
        {ok, GroupIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about group that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_group_details(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    GroupId :: binary()) ->
    {ok, GroupDetails :: #group_details{}} | {error, Reason :: term()}.
get_group_details(Auth, SpaceId, GroupId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
            binary_to_list(GroupId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupDetails = #group_details{
            id = lists_utils:key_get(<<"groupId">>, Proplist),
            name = lists_utils:key_get(<<"name">>, Proplist),
            type = lists_utils:key_get(<<"type">>, Proplist)
        },
        {ok, GroupDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of group that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    GroupId :: binary()) ->
    {ok, Privileges :: [privileges:space_privilege()]} | {error, Reason :: term()}.
get_group_privileges(Auth, SpaceId, GroupId) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
        binary_to_list(GroupId) ++ "/privileges",
    get_privileges(Auth, URN).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges for group that belongs to Space.
%% Parameters should contain: list of "privileges" of type privileges:space_privilege().
%% @end
%%--------------------------------------------------------------------
-spec set_group_privileges(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    GroupId :: binary(), Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_group_privileges(Auth, SpaceId, GroupId, Parameters) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
        binary_to_list(GroupId) ++ "/privileges",
    set_privileges(Auth, URN, Parameters).

%%--------------------------------------------------------------------
%% @doc Makes provider stop supporting Space.
%% @end
%%--------------------------------------------------------------------
-spec remove_provider(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    ProviderId :: binary()) ->
    ok | {error, Reason :: term()}.
remove_provider(Auth, SpaceId, ProviderId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/" ++
            binary_to_list(ProviderId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of providers that supports Space.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    {ok, ProviderIds :: [binary()]} | {error, Reason :: term()}.
get_providers(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        ProviderIds = lists_utils:key_get(<<"providers">>, Proplist),
        {ok, ProviderIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about provider that supports Space.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_details(Auth :: oz_endpoint:auth(), SpaceId :: binary(),
    ProviderId :: binary()) ->
    {ok, ProviderDetails :: #provider_details{}} | {error, Reason :: term()}.
get_provider_details(Auth, SpaceId, ProviderId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/" ++
            binary_to_list(ProviderId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        ProviderDetails = #provider_details{
            id = lists_utils:key_get(<<"providerId">>, Proplist),
            name = lists_utils:key_get(<<"clientName">>, Proplist),
            domain = lists_utils:key_get(<<"domain">>, Proplist),
            latitude = lists_utils:key_get(<<"latitude">>, Proplist),
            longitude = lists_utils:key_get(<<"longitude">>, Proplist)
        },
        {ok, ProviderDetails}
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns list of privileges.
%% @end
%%--------------------------------------------------------------------
-spec get_privileges(Auth :: oz_endpoint:auth(), oz_endpoint:urn()) ->
    {ok, Privileges :: [privileges:space_privilege()]} | {error, Reason :: term()}.
get_privileges(Auth, URN) ->
    ?run(fun() ->
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Privileges = lists_utils:key_get(<<"privileges">>, Proplist),
        {ok, lists:map(fun(Binary) ->
            binary_to_atom(Binary, latin1) end, Privileges)}
    end).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges.
%% @end
%%--------------------------------------------------------------------
-spec set_privileges(Auth :: oz_endpoint:auth(), oz_endpoint:urn(),
    Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_privileges(Auth, URN, Parameters) ->
    ?run(fun() ->
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, put, Body),
        ok
    end).
