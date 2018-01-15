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
    get_group_details/2, get_effective_groups/1]).

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
            oz_endpoint:request(provider, URN, post, Body),
        {ok, iolist_to_binary(ResponseBody)}
    end).

%%--------------------------------------------------------------------
%% @doc Returns details about current user.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth()) ->
    {ok, UserDetails :: #user_details{}} | {error, Reason :: term()}.
get_details(Auth) ->
    ?run(fun() ->
        URN = "/user",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserDetails = #user_details{
            id = lists_utils:key_get(<<"userId">>, Proplist),
            name = lists_utils:key_get(<<"name">>, Proplist),
            connected_accounts = lists_utils:key_get(<<"linkedAccounts">>, Proplist),
            alias = lists_utils:key_get(<<"alias">>, Proplist),
            email_list = lists_utils:key_get(<<"emailList">>, Proplist)
        },
        {ok, UserDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about user. Parameters may contain:
%% "name" of user.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/user",
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Merges account, associated with token, with user's account.
%% Parameters should contain: "token" associated with account to be merged.
%% @end
%%--------------------------------------------------------------------
-spec merge_account(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
merge_account(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/user/merge",
        Body = json_utils:encode(Parameters),
        {ok, 201, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows provider to create Space for user.
%% @end
%%--------------------------------------------------------------------
-spec get_create_space_token(Auth :: oz_endpoint:auth()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_create_space_token(Auth) ->
    ?run(fun() ->
        URN = "/user/spaces/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Returns token that allows user to merge his account with other account.
%% @end
%%--------------------------------------------------------------------
-spec get_merge_account_token(Auth :: oz_endpoint:auth()) ->
    {ok, Token :: binary()} | {error, Reason :: term()}.
get_merge_account_token(Auth) ->
    ?run(fun() ->
        URN = "/user/merge/token",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        Token = lists_utils:key_get(<<"token">>, Proplist),
        {ok, Token}
    end).

%%--------------------------------------------------------------------
%% @doc Creates new Space and makes user the only member and administrator
%% of created Space. Parameters should contain: "name" of new Space.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create_space(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/user/spaces",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        SpaceId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user join Space associated with token.
%% Parameters should contain: "token" associated with Space.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
join_space(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/user/spaces/join",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        SpaceId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user leave Space.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
leave_space(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/user/spaces/" ++ binary_to_list(SpaceId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of Spaces that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Auth :: oz_endpoint:auth()) ->
    {ok, UserSpaces :: #user_spaces{}} | {error, Reason :: term()}.
get_spaces(Auth) ->
    ?run(fun() ->
        URN = "/user/spaces",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        UserSpaces = #user_spaces{
            ids = lists_utils:key_get(<<"spaces">>, Proplist),
            default = lists_utils:key_get(<<"default">>, Proplist,
                <<"undefined">>)
        },
        {ok, UserSpaces}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about Space that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
get_space_details(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/user/spaces/" ++ binary_to_list(SpaceId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        SpaceDetails = #space_details{
            id = lists_utils:key_get(<<"spaceId">>, Proplist),
            name = lists_utils:key_get(<<"name">>, Proplist),
            providers_supports = lists_utils:key_get(<<"providersSupports">>,
                Proplist)
        },
        {ok, SpaceDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns ID of default user's Space.
%% @end
%%--------------------------------------------------------------------
-spec get_default_space(Auth :: oz_endpoint:auth()) ->
    {ok, DefaultSpaceId :: undefined | binary()} | {error, Reason :: term()}.
get_default_space(Auth) ->
    ?run(fun() ->
        URN = "/user/spaces/default",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        DefaultSpaceId = lists_utils:key_get(<<"spaceId">>, Proplist),
        {ok, DefaultSpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Sets default user's Space. Parameters should contain: "spaceId"
%% of Space to make user's default.
%% @end
%%--------------------------------------------------------------------
-spec set_default_space(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
set_default_space(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/user/spaces/default",
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, put, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Creates new group and makes user the only member and administrator
%% of created group. Parameters should contain: "name" of new group.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, GroupId :: binary()} | {error, Reason :: term()}.
create_group(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/user/groups",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        GroupId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, GroupId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user join group associated with token.
%% Parameters should contain: "token" associated with group.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, GroupId :: binary()} | {error, Reason :: term()}.
join_group(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/user/groups/join",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        GroupId = http_utils:last_url_part(maps:get(<<"Location">>, ResponseHeaders)),
        {ok, GroupId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes user leave group.
%% @end
%%--------------------------------------------------------------------
-spec leave_group(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    ok | {error, Reason :: term()}.
leave_group(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/user/groups/" ++ binary_to_list(GroupId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of groups that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Auth :: oz_endpoint:auth()) ->
    {ok, GroupIds :: [binary()]} | {error, Reason :: term()}.
get_groups(Auth) ->
    ?run(fun() ->
        URN = "/user/groups",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupIds = lists_utils:key_get(<<"groups">>, Proplist),
        {ok, GroupIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of groups that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_groups(Auth :: oz_endpoint:auth()) ->
    {ok, GroupIds :: [binary()]} | {error, Reason :: term()}.
get_effective_groups(Auth) ->
    ?run(fun() ->
        URN = "/user/effective_groups",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        GroupIds = lists_utils:key_get(<<"effective_groups">>, Proplist),
        {ok, GroupIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about group that user belongs to.
%% @end
%%--------------------------------------------------------------------
-spec get_group_details(Auth :: oz_endpoint:auth(), GroupId :: binary()) ->
    {ok, GroupDetails :: #group_details{}} | {error, Reason :: term()}.
get_group_details(Auth, GroupId) ->
    ?run(fun() ->
        URN = "/user/groups/" ++ binary_to_list(GroupId),
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
