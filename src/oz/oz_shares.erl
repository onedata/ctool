%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for Spaces management in OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_shares).

-include("oz/oz_runner.hrl").
-include("oz/oz_shares.hrl").

%% API
-export([create/4]).
-export([get_details/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns public details about Space.
%% @end
%%--------------------------------------------------------------------

-spec get_details(Auth :: oz_endpoint:auth(), ShareId :: binary()) ->
    {ok, SpaceDetails :: #share_details{}} | {error, Reason :: term()}.
get_details(Auth, ShareId) ->
    ?run(fun() ->
        URN = "/shares/" ++ binary_to_list(ShareId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:auth_request(Auth, URN, get),
        Props = json_utils:decode(ResponseBody),
        % Get default values of space_details record
        ShareDetails = #share_details{
            id = proplists:get_value(<<"shareId">>, Props),
            name = proplists:get_value(<<"name">>, Props),
            public_url = proplists:get_value(
                <<"public_url">>, Props, undefined),
            root_file_id = proplists:get_value(
                <<"root_file_id">>, Props, undefined),
            parent_space = proplists:get_value(
                <<"parent_space">>, Props, undefined)
        },
        {ok, ShareDetails}
    end).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new share with given ID. Parameters should contain:
%% #   "name" - of new Share
%% #   "root_file_id" - GUID of root file of new Share
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
        {ok, 201, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Auth, URN, put, Body),
        {ok, ShareId}
    end).
