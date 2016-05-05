%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for users management in OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_users).

-include("oz/oz_runner.hrl").
-include("oz/oz_users.hrl").
-include("oz/oz_spaces.hrl").
-include("oz/oz_groups.hrl").

%% API
-export([authorize/1]).
-export([get_details/1, modify_details/2, merge_account/2]).
-export([get_create_space_token/1, get_merge_account_token/1]).
-export([create_space/2, join_space/2, leave_space/2, get_spaces/1,
    get_space_details/2, get_default_space/1, set_default_space/2]).
-export([create_group/2, join_group/2, leave_group/2, get_groups/1,
    get_group_details/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a discharge macaroon for given caveat ID.
%% @end
%%--------------------------------------------------------------------
-spec authorize(CaveatId :: binary()) ->
    {ok, DischMacaroon :: binary()} | {error, Reason :: term()}.
authorize(CaveatId) ->
    ?run(fun() ->
        URN = "/user/authorize",
        Body = jiffy:encode({[{<<"identifier">>, CaveatId}]}),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(provider, URN, post, Body),
        {ok, iolist_to_binary(ResponseBody)}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about user.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Client :: oz_endpoint:client()) ->
    {ok, UserDetails :: #user_details{}} | {error, Reason :: term()}.
get_details(Client) ->
    ?run(fun() ->
        URN = "/user",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserDetails = #user_details{
            id = proplists:get_value(<<"userId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            connected_accounts = proplists:get_value(<<"connectedAccounts">>, Proplist),
            alias = proplists:get_value(<<"alias">>, Proplist),
            email_list = proplists:get_value(<<"emailList">>, Proplist)
        },
        {ok, UserDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about user. Parameters may contain:
%% "name" of user.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Client, Parameters) ->
    ?run(fun() ->
        URN = "/user",
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Merges account, associated with token, with user's account.
%% Parameters should contain: "token" associated with account to be merged.
%% @end
%%--------------------------------------------------------------------
-spec merge_account(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
merge_account(Client, Parameters) ->
    ?run(fun() ->
        URN = "/user/merge",
        Body = json_utils:encode(Parameters),
        {ok, 201, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, post, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows provider to create Space for user.
%% @end
%%--------------------------------------------------------------------
-spec get_create_space_token(Client :: oz_endpoint:client()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_create_space_token(Client) ->
    ?run(fun() ->
        URN = "/user/spaces/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows user to merge his account with other account.
%% @end
%%--------------------------------------------------------------------
-spec get_merge_account_token(Client :: oz_endpoint:client()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_merge_account_token(Client) ->
    ?run(fun() ->
        URN = "/user/merge/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = proplists:get_value(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Creates new Space and makes user the only member and administrator
%% of created Space. Parameters should contain: "name" of new Space.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create_space(Client, Parameters) ->
    ?run(fun() ->
        URN = "/user/spaces",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> =
            proplists:get_value(<<"location">>, ResponseHeaders),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user join Space associated with token.
%% Parameters should contain: "token" associated with Space.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
join_space(Client, Parameters) ->
    ?run(fun() ->
        URN = "/user/spaces/join",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, post, Body),
        <<"/user/spaces/", SpaceId/binary>> =
            proplists:get_value(<<"location">>, ResponseHeaders),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user leave Space.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Client :: oz_endpoint:client(), SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
leave_space(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/user/spaces/" ++ binary_to_list(SpaceId),
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of Spaces that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Client :: oz_endpoint:client()) ->
    {ok, UserSpaces :: #user_spaces{}} | {error, Reason :: term()}.
get_spaces(Client) ->
    ?run(fun() ->
        URN = "/user/spaces",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserSpaces = #user_spaces{
            ids = proplists:get_value(<<"spaces">>, Proplist),
            default = proplists:get_value(<<"default">>, Proplist,
                <<"undefined">>)
        },
        {ok, UserSpaces}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about Space that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Client :: oz_endpoint:client(), SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
get_space_details(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/user/spaces/" ++ binary_to_list(SpaceId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        SpaceDetails = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            providers_supports = proplists:get_value(<<"providersSupports">>,
                Proplist)
        },
        {ok, SpaceDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns ID of default user's Space.
%% @end
%%--------------------------------------------------------------------
-spec get_default_space(Client :: oz_endpoint:client()) ->
    {ok, DefaultSpaceId :: undefined | binary()} | {error, Reason :: term()}.
get_default_space(Client) ->
    ?run(fun() ->
        URN = "/user/spaces/default",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        DefaultSpaceId = proplists:get_value(<<"spaceId">>, Proplist),
        {ok, DefaultSpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Sets default user's Space. Parameters should contain: "spaceId"
%% of Space to make user's default.
%% @end
%%--------------------------------------------------------------------
-spec set_default_space(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_default_space(Client, Parameters) ->
    ?run(fun() ->
        URN = "/user/spaces/default",
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, put, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Creates new group and makes user the only member and administrator
%% of created group. Parameters should contain: "name" of new group.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) ->
    {ok, GroupId :: binary()} | {error, Reason :: term()}.
create_group(Client, Parameters) ->
    ?run(fun() ->
        URN = "/user/groups",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, post, Body),
        <<"/groups/", GroupId/binary>> =
            proplists:get_value(<<"location">>, ResponseHeaders),
        {ok, GroupId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user join group associated with token.
%% Parameters should contain: "token" associated with group.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Client :: oz_endpoint:client(),
    Parameters :: oz_endpoint:params()) ->
    {ok, GroupId :: binary()} | {error, Reason :: term()}.
join_group(Client, Parameters) ->
    ?run(fun() ->
        URN = "/user/groups/join",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, post, Body),
        <<"/user/groups/", GroupId/binary>> =
            proplists:get_value(<<"location">>, ResponseHeaders),
        {ok, GroupId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user leave group.
%% @end
%%--------------------------------------------------------------------
-spec leave_group(Client :: oz_endpoint:client(), GroupId :: binary()) ->
    ok | {error, Reason :: term()}.
leave_group(Client, GroupId) ->
    ?run(fun() ->
        URN = "/user/groups/" ++ binary_to_list(GroupId),
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of groups that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Client :: oz_endpoint:client()) ->
    {ok, GroupIds :: [binary()]} | {error, Reason :: term()}.
get_groups(Client) ->
    ?run(fun() ->
        URN = "/user/groups",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupIds = proplists:get_value(<<"groups">>, Proplist),
        {ok, GroupIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about group that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_group_details(Client :: oz_endpoint:client(), GroupId :: binary()) ->
    {ok, GroupDetails :: #group_details{}} | {error, Reason :: term()}.
get_group_details(Client, GroupId) ->
    ?run(fun() ->
        URN = "/user/groups/" ++ binary_to_list(GroupId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Client, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupDetails = #group_details{
            id = proplists:get_value(<<"groupId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, GroupDetails}
    end).
