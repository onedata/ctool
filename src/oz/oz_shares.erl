%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for Shares management in OZ.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_shares).

-include("oz/oz_runner.hrl").
-include("oz/oz_shares.hrl").

%% API
-export([create/4, remove/2]).
-export([get_details/2, modify_details/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new share with given ID. Parameters should contain:
%% #   "name" - of new Share
%% #   "rootFileId" - GUID of root file of new Share
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: oz_endpoint:auth(), ShareId :: binary(),
    SpaceId :: binary(), Parameters :: oz_endpoint:params()) ->
    {ok, ShareId :: binary()} | {error, Reason :: term()}.
create(Auth, ShareId, SpaceId, Parameters) ->
    ?run(fun() ->
        URN = "/spaces/" ++ binary_to_list(SpaceId) ++
            "/shares/" ++ binary_to_list(ShareId),
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, put, Body),
        {ok, ShareId}
    end).


%%--------------------------------------------------------------------
%% @doc Removes a Share.
%% @end
%%--------------------------------------------------------------------
-spec remove(Auth :: oz_endpoint:auth(), ShareId :: binary()) ->
    ok | {error, Reason :: term()}.
remove(Auth, ShareId) ->
    ?run(fun() ->
        URN = "/shares/" ++ binary_to_list(ShareId),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
        ok
    end).


%%--------------------------------------------------------------------
%% @doc Returns public details about Share.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth(), ShareId :: binary()) ->
    {ok, ShareDetails :: #share_details{}} | {error, Reason :: term()}.
get_details(Auth, ShareId) ->
    ?run(fun() ->
        URN = "/shares/" ++ binary_to_list(ShareId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Props = json_utils:decode(ResponseBody),
        % Get default values of share_details record
        ShareDetails = #share_details{
            id = lists_utils:key_get(<<"shareId">>, Props),
            name = lists_utils:key_get(<<"name">>, Props),
            public_url = lists_utils:key_get(
                <<"publicUrl">>, Props, undefined),
            root_file = lists_utils:key_get(
                <<"rootFileId">>, Props, undefined),
            space = lists_utils:key_get(
                <<"spaceId">>, Props, undefined)
        },
        {ok, ShareDetails}
    end).


%%--------------------------------------------------------------------
%% @doc Modifies public details about Share. Parameters may contain:
%% "name" of Share.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Auth :: oz_endpoint:auth(), ShareId :: binary(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Auth, ShareId, Parameters) ->
    ?run(fun() ->
        URN = "/shares/" ++ binary_to_list(ShareId),
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, patch, Body),
        ok
    end).
