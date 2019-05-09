%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for groups management in OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_groups).

-include("oz/oz_runner.hrl").
-include("oz/oz_users.hrl").
-include("oz/oz_spaces.hrl").
-include("oz/oz_groups.hrl").

%% API
-export([create/2, remove/2, get_details/2, modify_details/3]).
-export([get_create_space_token/2, get_invite_user_token/2]).
-export([remove_user/3, get_users/2, get_user_details/3, get_user_privileges/3,
    set_user_privileges/4, get_effective_users/2, get_effective_user_privileges/3]).
-export([get_nested_privileges/3, set_nested_privileges/4, get_parents/2, get_nested/2,
    get_invite_group_token/2, join_group/3, leave_group/3]).
-export([create_space/3, join_space/3, leave_space/3, get_spaces/2,
    get_space_details/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates new group and makes user the only member and administrator
%% of created group. Parameters should contain: "name" of new group.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: oz_endpoint:auth(), Params :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/groups",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        GroupId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, GroupId}
    end).

%%--------------------------------------------------------------------
%% @doc Removes group.
%% @end
%%--------------------------------------------------------------------
-spec remove(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    ok | {error, Reason :: term()}.
remove(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about group.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    {ok, GroupDetails :: #group_details{}} | {error, Reason :: term()}.
get_details(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        GroupDetails = #group_details{
            id = lists_utils:key_get(<<"groupId">>, Proplist),
            name = lists_utils:key_get(<<"name">>, Proplist),
            type = binary_to_atom(lists_utils:key_get(<<"type">>, Proplist), utf8)
        },
        {ok, GroupDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about group. Parameters may contain:
%% "name" of group.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Auth, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId),
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows provider to create Space for group.
%% @end
%%--------------------------------------------------------------------
-spec get_create_space_token(Auth :: oz_endpoint:auth(),
    GroupId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
get_create_space_token(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows user to join group.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_user_token(Auth :: oz_endpoint:auth(),
    GroupId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_user_token(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Removes user from group.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    UserId :: binary()) -> ok | {error, Reason :: term()}.
remove_user(Auth, GroupId, UserId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of users that belong to group.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_users(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        UserIds = lists_utils:key_get(<<"users">>, Proplist),
        {ok, UserIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of users that belong to group.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_users(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_effective_users(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/effective_users",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        UserIds = lists_utils:key_get(<<"users">>, Proplist),
        {ok, UserIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of users that belong to group.
%% @end
%%--------------------------------------------------------------------
-spec get_parents(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    {ok, GroupIds :: [binary()]} | {error, Reason :: term()}.
get_parents(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/parent",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        UserIds = lists_utils:key_get(<<"parent_groups">>, Proplist),
        {ok, UserIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of users that belong to group.
%% @end
%%--------------------------------------------------------------------
-spec get_nested(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    {ok, GroupIds :: [binary()]} | {error, Reason :: term()}.
get_nested(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/nested",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        UserIds = lists_utils:key_get(<<"nested_groups">>, Proplist),
        {ok, UserIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about user that belongs to group.
%% @end
%%--------------------------------------------------------------------
-spec get_user_details(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    UserId :: binary()) -> {ok, UserDetails :: #user_details{}} | {error,
    Reason :: term()}.
get_user_details(Auth, GroupId, UserId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        UserDetails = #user_details{
            id = lists_utils:key_get(<<"userId">>, Proplist),
            full_name = lists_utils:key_get(<<"name">>, Proplist)
        },
        {ok, UserDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of user that belongs to group.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    UserId :: binary()) ->
    {ok, Privileges :: [privileges:group_privilege()]} | {error, Reason :: term()}.
get_user_privileges(Auth, GroupId, UserId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId) ++ "/privileges",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Privileges = lists_utils:key_get(<<"privileges">>, Proplist),
        {ok, lists:map(fun(Binary) ->
            binary_to_atom(Binary, latin1) end, Privileges)}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of user that belongs to group.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_user_privileges(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    UserId :: binary()) ->
    {ok, Privileges :: [privileges:group_privilege()]} | {error, Reason :: term()}.
get_effective_user_privileges(Auth, GroupId, UserId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/effective_users/" ++
            binary_to_list(UserId) ++ "/privileges",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Privileges = lists_utils:key_get(<<"privileges">>, Proplist),
        {ok, lists:map(fun(Binary) ->
            binary_to_atom(Binary, latin1) end, Privileges)}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of user that belongs to group.
%% @end
%%--------------------------------------------------------------------
-spec get_nested_privileges(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    NestedGroupId :: binary()) ->
    {ok, Privileges :: [privileges:group_privilege()]} | {error, Reason :: term()}.
get_nested_privileges(Auth, GroupId, NestedGroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/nested/" ++
            binary_to_list(NestedGroupId) ++ "/privileges",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Privileges = lists_utils:key_get(<<"privileges">>, Proplist),
        {ok, lists:map(fun(Binary) ->
            binary_to_atom(Binary, latin1) end, Privileges)}
    end).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges for user that belongs to group.
%% Parameters should contain: list of "privileges" of type privileges:group_privilege().
%% @end
%%--------------------------------------------------------------------
-spec set_user_privileges(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    UserId :: binary(), Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_user_privileges(Auth, GroupId, UserId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId) ++ "/privileges",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, put, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges for group that belongs to the other group.
%% Parameters should contain: list of "privileges" of type privileges:group_privilege().
%% @end
%%--------------------------------------------------------------------
-spec set_nested_privileges(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    NestedGroupId :: binary(), Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_nested_privileges(Auth, GroupId, NestedGroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/nested/" ++
            binary_to_list(NestedGroupId) ++ "/privileges",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, put, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows user to join group to another group.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_group_token(Auth :: oz_endpoint:auth(),
    GroupId :: binary()) -> {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_group_token(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/nested/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Makes group join group associated with token.
%% Parameters should contain: "token" associated with group.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    Parameters :: oz_endpoint:params()) ->
    {ok, NestedGroupId :: binary()} | {error, Reason :: term()}.
join_group(Auth, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/nested/join",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        NestedGroupId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, NestedGroupId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes group leave group.
%% @end
%%--------------------------------------------------------------------
-spec leave_group(Auth :: oz_endpoint:auth(), ParentGroupId :: binary(),
    GroupId :: binary()) -> ok | {error, Reason :: term()}.
leave_group(Auth, ParentGroupId, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(ParentGroupId) ++ "/nested/" ++
            binary_to_list(GroupId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Creates new Space and makes group the only member and administrator
%% of created Space. Parameters should contain: "name" of new Space.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create_space(Auth, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        SpaceId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes group join Space associated with token.
%% Parameters should contain: "token" associated with Space.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
join_space(Auth, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/join",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        SpaceId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes group leave Space.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    SpaceId :: binary()) -> ok | {error, Reason :: term()}.
leave_space(Auth, GroupId, SpaceId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++
            binary_to_list(SpaceId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of Spaces that group belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
get_spaces(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        SpaceIds = lists_utils:key_get(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about Space that group belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Auth :: oz_endpoint:auth(), GroupId :: binary(),
    SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
get_space_details(Auth, GroupId, SpaceId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++
            binary_to_list(SpaceId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        SpaceDetails = #space_details{
            id = lists_utils:key_get(<<"spaceId">>, Proplist),
            name = lists_utils:key_get(<<"name">>, Proplist),
            providers_supports = lists_utils:key_get(<<"providers">>, Proplist)
        },
        {ok, SpaceDetails}
    end).
