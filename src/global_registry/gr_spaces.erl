%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module allows for Spaces management in Global Registry.
%% @end
%% ===================================================================

-module(gr_spaces).

-include("global_registry/gr_runner.hrl").
-include("global_registry/gr_types.hrl").
-include("global_registry/gr_users.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_groups.hrl").
-include("global_registry/gr_providers.hrl").

%% API
-export([create/2, remove/2, get_details/2, modify_details/3]).
-export([get_invite_user_token/2, get_invite_group_token/2, get_invite_provider_token/2]).
-export([remove_user/3, get_users/2, get_user_details/3, get_user_privileges/3, set_user_privileges/4]).
-export([remove_group/3, get_groups/2, get_group_details/3, get_group_privileges/3, set_group_privileges/4]).
-export([remove_provider/3, get_providers/2, get_provider_details/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% create/2
%% ====================================================================
%% @doc For:
%% * user it creates new Space and makes him the only member and administrator
%%   of created Space. Parameters should contain: "name" of new Space.
%% * provider it creates new Space and makes provider support created Space.
%%   User/group that has given provider a token receives all privileges
%%   for new Space. Parameters should contain: "name" of new Space and
%%   "token" associated with user/group.
-spec create(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create(Client, Parameters) ->
    ?run(fun() ->
        URN = "/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    end).


%% remove/2
%% ====================================================================
%% @doc Removes Space.
-spec remove(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).


%% get_details/2
%% ====================================================================
%% @doc Returns public details about Space.
-spec get_details(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, SpaceInfo :: #space_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_details(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceInfo = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, SpaceInfo}
    end).


%% modify_details/3
%% ====================================================================
%% @doc Modifies public details about Space. Parameters may contain:
%% "name" of Space.
-spec modify_details(Client :: client(), SpaceId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_details(Client, SpaceId, Parameters) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId),
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, patch, Body),
        ok
    end).


%% get_invite_user_token/2
%% ====================================================================
%% @doc Returns token that allows user to join Space.
-spec get_invite_user_token(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, Token :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_invite_user_token(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).


%% get_invite_group_token/2
%% ====================================================================
%% @doc Returns token that allows group to join Space.
-spec get_invite_group_token(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, Token :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_invite_group_token(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).


%% get_invite_provider_token/2
%% ====================================================================
%% @doc Returns token that allows provider to support Space.
-spec get_invite_provider_token(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, Token :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_invite_provider_token(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/token",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).


%% remove_user/3
%% ====================================================================
%% @doc Removes user from Space.
-spec remove_user(Client :: client(), SpaceId :: binary(), UserId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_user(Client, SpaceId, UserId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++ binary_to_list(UserId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).


%% get_users/2
%% ====================================================================
%% @doc Returns list of IDs of users that belong to Space.
-spec get_users(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_users(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserIds = proplists:get_value(<<"users">>, Proplist),
        {ok, UserIds}
    end).


%% get_user_details/3
%% ====================================================================
%% @doc Returns public details about user that belongs to Space.
-spec get_user_details(Client :: client(), SpaceId :: binary(), UserId :: binary()) -> Result when
    Result :: {ok, UserInfo :: #user_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_user_details(Client, SpaceId, UserId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++ binary_to_list(UserId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        UserInfo = #user_details{
            id = proplists:get_value(<<"userId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, UserInfo}
    end).


%% get_user_privileges/3
%% ====================================================================
%% @doc Returns list of privileges of user that belongs to Space.
-spec get_user_privileges(Client :: client(), GroupId :: binary(), UserId :: binary()) -> Result when
    Result :: {ok, Privileges :: [space_privilege()]} | {error, Reason :: term()}.
%% ====================================================================
get_user_privileges(Client, SpaceId, UserId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++ binary_to_list(UserId) ++ "/privileges",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Privileges = proplists:get_value(<<"privileges">>, Proplist),
        {ok, Privileges}
    end).


%% set_user_privileges/4
%% ====================================================================
%% @doc Sets list of privileges for user that belongs to Space.
%% Parameters should contain: list of "privileges" of type space_privilege().
-spec set_user_privileges(Client :: client(), SpaceId :: binary(), UserId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_user_privileges(Client, SpaceId, UserId, Parameters) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/users/" ++ binary_to_list(UserId) ++ "/privileges",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, put, Body),
        ok
    end).


%% remove_group/3
%% ====================================================================
%% @doc Removes group from Space.
-spec remove_group(Client :: client(), SpaceId :: binary(), GroupId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_group(Client, SpaceId, GroupId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++ binary_to_list(GroupId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).


%% get_groups/2
%% ====================================================================
%% @doc Returns list of IDs of groups that belong to Space.
-spec get_groups(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_groups(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        GroupIds = proplists:get_value(<<"groups">>, Proplist),
        {ok, GroupIds}
    end).


%% get_group_details/3
%% ====================================================================
%% @doc Returns public details about group that belongs to Space.
-spec get_group_details(Client :: client(), SpaceId :: binary(), GroupId :: binary()) -> Result when
    Result :: {ok, GroupInfo :: #group_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_group_details(Client, SpaceId, GroupId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++ binary_to_list(GroupId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        GroupInfo = #group_details{
            id = proplists:get_value(<<"groupId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, GroupInfo}
    end).


%% get_group_privileges/3
%% ====================================================================
%% @doc Returns list of privileges of group that belongs to Space.
-spec get_group_privileges(Client :: client(), GroupId :: binary(), GroupId :: binary()) -> Result when
    Result :: {ok, Privileges :: [space_privilege()]} | {error, Reason :: term()}.
%% ====================================================================
get_group_privileges(Client, SpaceId, GroupId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++ binary_to_list(GroupId) ++ "/privileges",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        Privileges = proplists:get_value(<<"privileges">>, Proplist),
        {ok, Privileges}
    end).


%% set_group_privileges/4
%% ====================================================================
%% @doc Sets list of privileges for group that belongs to Space.
%% Parameters should contain: list of "privileges" of type space_privilege().
-spec set_group_privileges(Client :: client(), SpaceId :: binary(), GroupId :: binary(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_group_privileges(Client, SpaceId, GroupId, Parameters) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/groups/" ++ binary_to_list(GroupId) ++ "/privileges",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, put, Body),
        ok
    end).


%% remove_provider/3
%% ====================================================================
%% @doc Makes provider stop supporting Space.
-spec remove_provider(Client :: client(), SpaceId :: binary(), ProviderId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_provider(Client, SpaceId, ProviderId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/" ++ binary_to_list(ProviderId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).


%% get_providers/2
%% ====================================================================
%% @doc Returns list of IDs of providers that supports Space.
-spec get_providers(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, UserIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_providers(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderIds = proplists:get_value(<<"providers">>, Proplist),
        {ok, ProviderIds}
    end).


%% get_provider_details/3
%% ====================================================================
%% @doc Returns public details about provider that supports Space.
-spec get_provider_details(Client :: client(), SpaceId :: binary(), ProviderId :: binary()) -> Result when
    Result :: {ok, ProviderInfo :: #provider_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_provider_details(Client, SpaceId, ProviderId) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++ "/providers/" ++ binary_to_list(ProviderId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderInfo = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            urls = proplists:get_value(<<"urls">>, Proplist),
            redirection_point = proplists:get_value(<<"redirectionPoint">>, Proplist)
        },
        {ok, ProviderInfo}
    end).
