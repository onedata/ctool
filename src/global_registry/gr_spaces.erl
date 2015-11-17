%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for Spaces management in Global Registry.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_spaces).

-include("global_registry/gr_runner.hrl").
-include("global_registry/gr_users.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_groups.hrl").
-include("global_registry/gr_providers.hrl").

%% API
-export([create/2, remove/2, get_details/2, modify_details/3]).
-export([get_invite_user_token/2, get_invite_group_token/2, get_invite_provider_token/2]).
-export([remove_user/3, get_users/2, get_user_details/3, get_user_privileges/3,
    get_effective_user_privileges/3, set_user_privileges/4]).
-export([remove_group/3, get_groups/2, get_group_details/3, get_group_privileges/3,
    set_group_privileges/4]).
-export([remove_provider/3, get_providers/2, get_provider_details/3]).

%% User privileges with regards to Space management.
-type space_privilege() :: space_invite_user | space_remove_user |
space_invite_group | space_remove_group | space_set_privileges |
space_remove | space_add_provider | space_remove_provider |
space_change_data | space_view_data.

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
-spec create(Client :: gr_endpoint:client(), Parameters :: gr_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create(Client, Parameters) ->
    ?run(fun() ->
        URN = "/spaces",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> =
            proplists:get_value(<<"location">>, ResponseHeaders),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Removes Space.
%% @end
%%--------------------------------------------------------------------
-spec remove(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
remove(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about Space.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
get_details(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        SpaceDetails = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            size = proplists:get_value(<<"size">>, Proplist)
        },
        {ok, SpaceDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about Space. Parameters may contain:
%% "name" of Space.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Client :: gr_endpoint:client(), SpaceId :: binary(),
    Parameters :: gr_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Client, SpaceId, Parameters) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows user to join Space.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_user_token(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_user_token(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows group to join Space.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_group_token(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_group_token(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows provider to support Space.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_provider_token(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_provider_token(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Removes user from Space.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: gr_endpoint:client(), SpaceId :: binary(),
    UserId :: binary()) -> ok | {error, Reason :: term()}.
remove_user(Client, SpaceId, UserId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of users that belong to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_users(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserIds = proplists:get_value(<<"users">>, Proplist),
        {ok, UserIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about user that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_user_details(Client :: gr_endpoint:client(), SpaceId :: binary(),
    UserId :: binary()) ->
    {ok, UserDetails :: #user_details{}} | {error, Reason :: term()}.
get_user_details(Client, SpaceId, UserId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserDetails = #user_details{
            id = proplists:get_value(<<"userId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, UserDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of user that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: gr_endpoint:client(), GroupId :: binary(),
    UserId :: binary()) ->
    {ok, Privileges :: [space_privilege()]} | {error, Reason :: term()}.
get_user_privileges(Client, SpaceId, UserId) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
        binary_to_list(UserId) ++ "/privileges",
    get_privileges(Client, URN).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of effective Space user.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_user_privileges(Client :: gr_endpoint:client(),
    GroupId :: binary(), UserId :: binary()) ->
    {ok, Privileges :: [space_privilege()]} | {error, Reason :: term()}.
get_effective_user_privileges(Client, SpaceId, UserId) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
        binary_to_list(UserId) ++ "/privileges?effective",
    get_privileges(Client, URN).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges for user that belongs to Space.
%% Parameters should contain: list of "privileges" of type space_privilege().
%% @end
%%--------------------------------------------------------------------
-spec set_user_privileges(Client :: gr_endpoint:client(), SpaceId :: binary(),
    UserId :: binary(), Parameters :: gr_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_user_privileges(Client, SpaceId, UserId, Parameters) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++
        binary_to_list(UserId) ++ "/privileges",
    set_privileges(Client, URN, Parameters).

%%--------------------------------------------------------------------
%% @doc Removes group from Space.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Client :: gr_endpoint:client(), SpaceId :: binary(),
    GroupId :: binary()) -> ok | {error, Reason :: term()}.
remove_group(Client, SpaceId, GroupId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
            binary_to_list(GroupId),
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of groups that belong to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_groups(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupIds = proplists:get_value(<<"groups">>, Proplist),
        {ok, GroupIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about group that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_group_details(Client :: gr_endpoint:client(), SpaceId :: binary(),
    GroupId :: binary()) ->
    {ok, GroupDetails :: #group_details{}} | {error, Reason :: term()}.
get_group_details(Client, SpaceId, GroupId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
            binary_to_list(GroupId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupDetails = #group_details{
            id = proplists:get_value(<<"groupId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, GroupDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of group that belongs to Space.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Client :: gr_endpoint:client(), GroupId :: binary(),
    GroupId :: binary()) ->
    {ok, Privileges :: [space_privilege()]} | {error, Reason :: term()}.
get_group_privileges(Client, SpaceId, GroupId) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
        binary_to_list(GroupId) ++ "/privileges",
    get_privileges(Client, URN).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges for group that belongs to Space.
%% Parameters should contain: list of "privileges" of type space_privilege().
%% @end
%%--------------------------------------------------------------------
-spec set_group_privileges(Client :: gr_endpoint:client(), SpaceId :: binary(),
    GroupId :: binary(), Parameters :: gr_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_group_privileges(Client, SpaceId, GroupId, Parameters) ->
    URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++
        binary_to_list(GroupId) ++ "/privileges",
    set_privileges(Client, URN, Parameters).

%%--------------------------------------------------------------------
%% @doc Makes provider stop supporting Space.
%% @end
%%--------------------------------------------------------------------
-spec remove_provider(Client :: gr_endpoint:client(), SpaceId :: binary(),
    ProviderId :: binary()) ->
    ok | {error, Reason :: term()}.
remove_provider(Client, SpaceId, ProviderId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/" ++
            binary_to_list(ProviderId),
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of providers that supports Space.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_providers(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        ProviderIds = proplists:get_value(<<"providers">>, Proplist),
        {ok, ProviderIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about provider that supports Space.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_details(Client :: gr_endpoint:client(), SpaceId :: binary(),
    ProviderId :: binary()) ->
    {ok, ProviderDetails :: #provider_details{}} | {error, Reason :: term()}.
get_provider_details(Client, SpaceId, ProviderId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/" ++
            binary_to_list(ProviderId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        ProviderDetails = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            name = proplists:get_value(<<"clientName">>, Proplist),
            urls = proplists:get_value(<<"urls">>, Proplist),
            redirection_point = proplists:get_value(<<"redirectionPoint">>, Proplist)
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
-spec get_privileges(Client :: gr_endpoint:client(), gr_endpoint:urn()) ->
    {ok, Privileges :: [space_privilege()]} | {error, Reason :: term()}.
get_privileges(Client, URN) ->
    ?run(fun() ->
        {ok, 200, _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Privileges = proplists:get_value(<<"privileges">>, Proplist),
        {ok, Privileges}
    end).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges.
%% @end
%%--------------------------------------------------------------------
-spec set_privileges(Client :: gr_endpoint:client(), gr_endpoint:urn(),
    Parameters :: gr_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_privileges(Client, URN, Parameters) ->
    ?run(fun() ->
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, put, Body),
        ok
    end).