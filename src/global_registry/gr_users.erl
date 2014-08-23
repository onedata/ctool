%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module allows for users management in Global Registry.
%% @end
%% ===================================================================

-module(gr_users).

-include("global_registry/gr_types.hrl").
-include("global_registry/gr_users.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_groups.hrl").

%% API
-export([get_details/1, modify_details/2, merge_account/2]).
-export([get_create_space_token/1, get_merge_account_token/1]).
-export([create_space/2, join_space/2, leave_space/2, get_spaces/1, get_space_details/2]).
-export([create_group/2, join_group/2, leave_group/2, get_groups/1, get_group_details/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% get_details/1
%% ====================================================================
%% @doc Returns public details about user.
-spec get_details(Client :: client()) -> Result when
    Result :: {ok, UserInfo :: #user_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_details(Client) ->
    try
        URN = "/user",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserInfo = #user_details{
            id = proplists:get_value(<<"userId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, UserInfo}
    catch
        _:Reason -> {error, Reason}
    end.


%% modify_details/2
%% ====================================================================
%% @doc Modifies public details about user. Parameters may contain:
%% "name" of user.
-spec modify_details(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_details(Client, Parameters) ->
    try
        URN = "/user",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, patch, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% merge_account/2
%% ====================================================================
%% @doc Merges account, associated with token, with user's account.
%% Parameters should contain: "token" associated with account to be merged.
-spec merge_account(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
merge_account(Client, Parameters) ->
    try
        URN = "/user/merge",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_create_space_token/1
%% ====================================================================
%% @doc Returns token that allows provider to create Space for user.
-spec get_create_space_token(Client :: client()) -> Result when
    Result :: {ok, Token :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_create_space_token(Client) ->
    try
        URN = "/user/spaces/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_merge_account_token/1
%% ====================================================================
%% @doc Returns token that allows user to merge his account with other account.
-spec get_merge_account_token(Client :: client()) -> Result when
    Result :: {ok, Token :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_merge_account_token(Client) ->
    try
        URN = "/user/merge/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    catch
        _:Reason -> {error, Reason}
    end.


%% create_space/2
%% ====================================================================
%% @doc Creates new Space and makes user the only member and administrator
%% of created Space. Parameters should contain: "name" of new Space.
-spec create_space(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_space(Client, Parameters) ->
    try
        URN = "/user/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% join_space/2
%% ====================================================================
%% @doc Makes user join Space associated with token.
%% Parameters should contain: "token" associated with Space.
-spec join_space(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
join_space(Client, Parameters) ->
    try
        URN = "/user/spaces/join",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        <<"/user/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% leave_space/2
%% ====================================================================
%% @doc Makes user leave Space.
-spec leave_space(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
leave_space(Client, SpaceId) ->
    try
        URN = "/user/spaces/" ++ binary_to_list(SpaceId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_spaces/1
%% ====================================================================
%% @doc Returns list of ids of Spaces that user belongs to.
-spec get_spaces(Client :: client()) -> Result when
    Result :: {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_spaces(Client) ->
    try
        URN = "/user/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_space_details/2
%% ====================================================================
%% @doc Returns public details about Space that user belongs to.
-spec get_space_details(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, SpaceInfo :: #space_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_space_details(Client, SpaceId) ->
    try
        URN = "/user/spaces/" ++ binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceInfo = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, SpaceInfo}
    catch
        _:Reason -> {error, Reason}
    end.


%% create_group/2
%% ====================================================================
%% @doc Creates new group and makes user the only member and administrator
%% of created group. Parameters should contain: "name" of new group.
-spec create_group(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, GroupId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_group(Client, Parameters) ->
    try
        URN = "/user/groups",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        <<"/groups/", GroupId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, GroupId}
    catch
        _:Reason -> {error, Reason}
    end.


%% join_group/2
%% ====================================================================
%% @doc Makes user join group associated with token.
%% Parameters should contain: "token" associated with group.
-spec join_group(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, GroupId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
join_group(Client, Parameters) ->
    try
        URN = "/user/groups/join",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        <<"/user/groups/", GroupId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, GroupId}
    catch
        _:Reason -> {error, Reason}
    end.


%% leave_group/2
%% ====================================================================
%% @doc Makes user leave group.
-spec leave_group(Client :: client(), GroupId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
leave_group(Client, GroupId) ->
    try
        URN = "/user/groups/" ++ binary_to_list(GroupId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_groups/1
%% ====================================================================
%% @doc Returns list of ids of groups that user belongs to.
-spec get_groups(Client :: client()) -> Result when
    Result :: {ok, GroupIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_groups(Client) ->
    try
        URN = "/user/groups",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        GroupIds = proplists:get_value(<<"groups">>, Proplist),
        {ok, GroupIds}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_group_details/2
%% ====================================================================
%% @doc Returns public details about group that user belongs to.
-spec get_group_details(Client :: client(), GroupId :: binary()) -> Result when
    Result :: {ok, GroupInfo :: #group_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_group_details(Client, GroupId) ->
    try
        URN = "/user/groups/" ++ binary_to_list(GroupId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        GroupInfo = #group_details{
            id = proplists:get_value(<<"groupId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, GroupInfo}
    catch
        _:Reason -> {error, Reason}
    end.
