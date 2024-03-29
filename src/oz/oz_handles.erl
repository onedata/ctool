%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for handle management in OZ.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_handles).

-include("oz/oz_runner.hrl").
-include("oz/oz_handles.hrl").

%% API
-export([create/2, get_details/2, get_public_details/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle. Parameters should contain:
%% #   "handleServiceId" - id oh Handle Service where the handle will
%%          be registered
%% #   "resourceType" - type of resource to which the handle will be created
%% #   "resourceId" - id of the resource
%% #   "metadata" - metadata of this handle, typically dublin core xml
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: oz_endpoint:auth(), Parameters :: oz_endpoint:params()) ->
    {ok, HandleId :: binary()} | {error, Reason :: term()}.
create(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/handles",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        HandleId = http_utils:last_url_part(maps:get(?HDR_LOCATION, ResponseHeaders)),
        {ok, HandleId}
    end).


%%--------------------------------------------------------------------
%% @doc Returns public details about a handle.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth(), HandleId :: binary()) ->
    {ok, HandleDetails :: #handle_details{}} | {error, Reason :: term()}.
get_details(Auth, HandleId) ->
    ?run(fun() ->
        URN = "/handles/" ++ binary_to_list(HandleId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Props = json_utils:decode_deprecated(ResponseBody),
        % Get default values of share_details record
        HandleDetails = #handle_details{
            id = proplists:get_value(<<"handleId">>, Props),
            handle_service = proplists:get_value(
                <<"handleServiceId">>, Props),
            public_handle = proplists:get_value(
                <<"handle">>, Props, undefined),
            resource_type = proplists:get_value(
                <<"resourceType">>, Props, undefined),
            resource_id = proplists:get_value(
                <<"resourceId">>, Props, undefined),
            metadata = proplists:get_value(
                <<"metadata">>, Props, undefined),
            timestamp = time:iso8601_to_datetime(proplists:get_value(
                <<"timestamp">>, Props, undefined))
        },
        {ok, HandleDetails}
    end).


%%--------------------------------------------------------------------
%% @doc Returns public details about a handle.
%% @end
%%--------------------------------------------------------------------
-spec get_public_details(Auth :: oz_endpoint:auth(), HandleId :: binary()) ->
    {ok, HandleDetails :: #handle_details{}} | {error, Reason :: term()}.
get_public_details(Auth, HandleId) ->
    ?run(fun() ->
        URN = "/handles/" ++ binary_to_list(HandleId) ++ "/public",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Props = json_utils:decode_deprecated(ResponseBody),
        % Get default values of share_details record
        HandleDetails = #handle_details{
            id = proplists:get_value(<<"handleId">>, Props),
            public_handle = proplists:get_value(
                <<"handle">>, Props, undefined),
            metadata = proplists:get_value(
                <<"metadata">>, Props, undefined)
        },
        {ok, HandleDetails}
    end).
