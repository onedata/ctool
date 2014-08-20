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

-include("global_registry/gr_users.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_groups.hrl").

%% API
-export([create/2, remove/2, get_info/2, modify_info/3]).
-export([get_create_space_token/2, get_invite_group_token/2]).
-export([remove_user/3, get_users/2, get_user_info/3, get_user_privileges/3, set_user_privileges/4]).
-export([create_space/3, join_space/3, leave_space/3, get_spaces/2, get_space_info/3]).

create(Client, Parameters) ->
    try
        URI = "/groups",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, post, Body),
        <<"/groups/", GroupId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, GroupId}
    catch
        _:Reason -> {error, Reason}
    end.


remove(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


get_info(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        GroupInfo = #group_info{
            id = proplists:get_value(<<"groupId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, GroupInfo}
    catch
        _:Reason -> {error, Reason}
    end.


modify_info(Client, GroupId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId),
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, patch, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


get_create_space_token(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    catch
        _:Reason -> {error, Reason}
    end.


get_invite_group_token(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    catch
        _:Reason -> {error, Reason}
    end.


remove_user(Client, GroupId, UserId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


get_users(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserIds = proplists:get_value(<<"users">>, Proplist),
        {ok, UserIds}
    catch
        _:Reason -> {error, Reason}
    end.


get_user_info(Client, GroupId, UserId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserInfo = #user_info{
            id = proplists:get_value(<<"userId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, UserInfo}
    catch
        _:Reason -> {error, Reason}
    end.


get_user_privileges(Client, GroupId, UserId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId) ++ "/privileges",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Privileges = proplists:get_value(<<"privileges">>, Proplist),
        {ok, Privileges}
    catch
        _:Reason -> {error, Reason}
    end.


set_user_privileges(Client, GroupId, UserId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/users/" ++ binary_to_list(UserId) ++ "/privileges",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, put, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


create_space(Client, GroupId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, post, Body),
        <<"/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


join_space(Client, GroupId, Parameters) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/join",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, post, Body),
        <<"/groups/", GroupId/binary, "/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


leave_space(Client, GroupId, SpaceId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++ binary_to_list(SpaceId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


get_spaces(Client, GroupId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    catch
        _:Reason -> {error, Reason}
    end.


get_space_info(Client, GroupId, SpaceId) ->
    try
        URI = "/groups/" ++ binary_to_list(GroupId) ++ "/spaces/" ++ binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceInfo = #space_info{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, SpaceInfo}
    catch
        _:Reason -> {error, Reason}
    end.