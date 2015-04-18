%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for groups management in Global Registry.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_groups).

-include("global_registry/gr_runner.hrl").
-include("global_registry/gr_users.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_groups.hrl").

%% API
-export([create/2, remove/2, get_details/2, modify_details/3]).
-export([get_create_space_token/2, get_invite_user_token/2]).
-export([remove_user/3, get_users/2, get_user_details/3, get_user_privileges/3,
    set_user_privileges/4]).
-export([create_space/3, join_space/3, leave_space/3, get_spaces/2,
    get_space_details/3]).

%% User privileges with regards to group management.
-type group_privilege() :: group_change_data | group_invite_user |
group_remove_user | group_join_space | group_create_space |
group_set_privileges | group_remove | group_leave_space |
group_view_data | group_create_space_token.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates new group and makes user the only member and administrator
%% of created group. Parameters should contain: "name" of new group.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: gr_endpoint:client(), Parameters :: gr_endpoint:parameters()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create(Client, Parameters) ->
    ?run(fun() ->
        URN = "/groups",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, post, Body),
        <<"/groups/", GroupId/binary>> =
            list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, GroupId}
    end).

%%--------------------------------------------------------------------
%% @doc Removes group.
%% @end
%%--------------------------------------------------------------------
-spec remove(Client :: gr_endpoint:client(), GroupId :: binary()) ->
    ok | {error, Reason :: term()}.
remove(Client, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId),
        {ok, "202", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about group.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Client :: gr_endpoint:client(), GroupId :: binary()) ->
    {ok, GroupDetails :: #group_details{}} | {error, Reason :: term()}.
get_details(Client, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId),
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        GroupDetails = #group_details{
            id = proplists:get_value(<<"groupId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, GroupDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about group. Parameters may contain:
%% "name" of group.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Client :: gr_endpoint:client(), GroupId :: binary(),
    Parameters :: gr_endpoint:parameters()) -> ok | {error, Reason :: term()}.
modify_details(Client, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId),
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows provider to create Space for group.
%% @end
%%--------------------------------------------------------------------
-spec get_create_space_token(Client :: gr_endpoint:client(), GroupId :: binary()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_create_space_token(Client, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/token",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows user to join group.
%% @end
%%--------------------------------------------------------------------
-spec get_invite_user_token(Client :: gr_endpoint:client(), GroupId :: binary()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_invite_user_token(Client, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/token",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Removes user from group.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: gr_endpoint:client(), GroupId :: binary(),
    UserId :: binary()) -> ok | {error, Reason :: term()}.
remove_user(Client, GroupId, UserId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, "202", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of users that belong to group.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: gr_endpoint:client(), GroupId :: binary()) ->
    {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
get_users(Client, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserIds = proplists:get_value(<<"users">>, Proplist),
        {ok, UserIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about user that belongs to group.
%% @end
%%--------------------------------------------------------------------
-spec get_user_details(Client :: gr_endpoint:client(), GroupId :: binary(),
    UserId :: binary()) -> {ok, UserDetails :: #user_details{}} | {error,
    Reason :: term()}.
get_user_details(Client, GroupId, UserId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId),
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserDetails = #user_details{
            id = proplists:get_value(<<"userId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, UserDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of privileges of user that belongs to group.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: gr_endpoint:client(), GroupId :: binary(),
    UserId :: binary()) ->
    {ok, Privileges :: [group_privilege()]} | {error, Reason :: term()}.
get_user_privileges(Client, GroupId, UserId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId) ++ "/privileges",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Privileges = proplists:get_value(<<"privileges">>, Proplist),
        {ok, Privileges}
    end).

%%--------------------------------------------------------------------
%% @doc Sets list of privileges for user that belongs to group.
%% Parameters should contain: list of "privileges" of type group_privilege().
%% @end
%%--------------------------------------------------------------------
-spec set_user_privileges(Client :: gr_endpoint:client(), GroupId :: binary(),
    UserId :: binary(), Parameters :: gr_endpoint:parameters()) ->
    ok | {error, Reason :: term()}.
set_user_privileges(Client, GroupId, UserId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++
            binary_to_list(UserId) ++ "/privileges",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, put, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Creates new Space and makes group the only member and administrator
%% of created Space. Parameters should contain: "name" of new Space.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Client :: gr_endpoint:client(), GroupId :: binary(),
    Parameters :: gr_endpoint:parameters()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create_space(Client, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> =
            list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes group join Space associated with token.
%% Parameters should contain: "token" associated with Space.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Client :: gr_endpoint:client(), GroupId :: binary(),
    Parameters :: gr_endpoint:parameters()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
join_space(Client, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/join",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, post, Body),
        GroupIdSize = size(GroupId),
        <<"/groups/", GroupId:GroupIdSize/binary, "/spaces/", SpaceId/binary>> =
            list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes group leave Space.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Client :: gr_endpoint:client(), GroupId :: binary(),
    SpaceId :: binary()) -> ok | {error, Reason :: term()}.
leave_space(Client, GroupId, SpaceId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++
            binary_to_list(SpaceId),
        {ok, "202", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of Spaces that group belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Client :: gr_endpoint:client(), GroupId :: binary()) ->
    {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
get_spaces(Client, GroupId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about Space that group belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Client :: gr_endpoint:client(), GroupId :: binary(),
    SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
get_space_details(Client, GroupId, SpaceId) ->
    ?run(fun() ->
        URN = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++
            binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceDetails = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            size = proplists:get_value(<<"size">>, Proplist)
        },
        {ok, SpaceDetails}
    end).