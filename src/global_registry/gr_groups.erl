%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module allows for groups management in Global Registry.
%% @end
%% ===================================================================

-module(gr_groups).

-include("global_registry/gr_types.hrl").
-include("global_registry/gr_users.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_groups.hrl").

%% API
-export([create/2, remove/2, get_details/2, modify_details/3]).
-export([get_create_space_token/2, get_invite_user_token/2]).
-export([remove_user/3, get_users/2, get_user_details/3, get_user_privileges/3, set_user_privileges/4]).
-export([create_space/3, join_space/3, leave_space/3, get_spaces/2, get_space_details/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% create/2
%% ====================================================================
%% @doc Creates new group and makes user the only member and administrator
%% of created group. Parameters should contain: "name" of new group.
-spec create(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create(Client, Parameters) ->
    try
        URI = "/groups",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, post, Body),
        <<"/groups/", GroupId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, GroupId}
    catch
        _:Reason -> {error, Reason}
    end.


%% remove/2
%% ====================================================================
%% @doc Removes group.
-spec remove(Client :: client(), GroupId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_details/2
%% ====================================================================
%% @doc Returns public details about group.
-spec get_details(Client :: client(), GroupId :: binary()) -> Result when
    Result :: {ok, GroupInfo :: #group_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_details(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        GroupInfo = #group_details{
            id = proplists:get_value(<<"groupId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, GroupInfo}
    catch
        _:Reason -> {error, Reason}
    end.


%% modify_details/3
%% ====================================================================
%% @doc Modifies public details about group. Parameters may contain:
%% "name" of group.
-spec modify_details(Client :: client(), GroupId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_details(Client, GroupId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId),
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, patch, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_create_space_token/2
%% ====================================================================
%% @doc Returns token that allows provider to create Space for group.
-spec get_create_space_token(Client :: client(), GroupId :: binary()) -> Result when
    Result :: {ok, Token :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_create_space_token(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_invite_user_token/2
%% ====================================================================
%% @doc Returns token that allows user to join group.
-spec get_invite_user_token(Client :: client(), GroupId :: binary()) -> Result when
    Result :: {ok, Token :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_invite_user_token(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    catch
        _:Reason -> {error, Reason}
    end.


%% remove_user/3
%% ====================================================================
%% @doc Removes user from group.
-spec remove_user(Client :: client(), GroupId :: binary(), UserId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_user(Client, GroupId, UserId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_users/2
%% ====================================================================
%% @doc Returns list of ids of users that belong to group.
-spec get_users(Client :: client(), GroupId :: binary()) -> Result when
    Result :: {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_users(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserIds = proplists:get_value(<<"users">>, Proplist),
        {ok, UserIds}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_user_details/3
%% ====================================================================
%% @doc Returns public details about user that belongs to group.
-spec get_user_details(Client :: client(), GroupId :: binary(), UserId :: binary()) -> Result when
    Result :: {ok, UserInfo :: #user_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_user_details(Client, GroupId, UserId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserInfo = #user_details{
            id = proplists:get_value(<<"userId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, UserInfo}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_user_privileges/3
%% ====================================================================
%% @doc Returns list of privileges of user that belongs to group.
-spec get_user_privileges(Client :: client(), GroupId :: binary(), UserId :: binary()) -> Result when
    Result :: {ok, Privileges :: [group_privilege()]} | {error, Reason :: term()}.
%% ====================================================================
get_user_privileges(Client, GroupId, UserId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId) ++ "/privileges",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Privileges = proplists:get_value(<<"privileges">>, Proplist),
        {ok, Privileges}
    catch
        _:Reason -> {error, Reason}
    end.


%% set_user_privileges/4
%% ====================================================================
%% @doc Sets list of privileges for user that belongs to group.
%% Parameters should contain: list of "privileges" of type group_privilege().
-spec set_user_privileges(Client :: client(), GroupId :: binary(), UserId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_user_privileges(Client, GroupId, UserId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId) ++ "/privileges",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, put, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% create_space/3
%% ====================================================================
%% @doc Creates new Space and makes group the only member and administrator
%% of created Space. Parameters should contain: "name" of new Space.
-spec create_space(Client :: client(), GroupId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_space(Client, GroupId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, post, Body),
        <<"/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% join_space/3
%% ====================================================================
%% @doc Makes group join Space associated with token.
%% Parameters should contain: "token" associated with Space.
-spec join_space(Client :: client(), GroupId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
join_space(Client, GroupId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/join",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, post, Body),
        GroupIdSize = size(GroupId),
        <<"/groups/", GroupId:GroupIdSize/binary, "/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% leave_space/3
%% ====================================================================
%% @doc Makes group leave Space.
-spec leave_space(Client :: client(), GroupId :: binary(), SpaceId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
leave_space(Client, GroupId, SpaceId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++ binary_to_list(SpaceId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_spaces/2
%% ====================================================================
%% @doc Returns list of ids of Spaces that group belongs to.
-spec get_spaces(Client :: client(), GroupId :: binary()) -> Result when
    Result :: {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_spaces(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_space_details/3
%% ====================================================================
%% @doc Returns public details about Space that group belongs to.
-spec get_space_details(Client :: client(), GroupId :: binary(), SpaceId :: binary()) -> Result when
    Result :: {ok, SpaceInfo :: #space_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_space_details(Client, GroupId, SpaceId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++ binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceInfo = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, SpaceInfo}
    catch
        _:Reason -> {error, Reason}
    end.