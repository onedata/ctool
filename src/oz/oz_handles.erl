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
-export([create/2, get_details/2]).


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
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:auth_request(Auth, URN, post, Body),
        <<"/handles/", HandleId/binary>> =
            proplists:get_value(<<"location">>, ResponseHeaders),
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
            oz_endpoint:auth_request(Auth, URN, get),
        Props = json_utils:decode(ResponseBody),
        % Get default values of share_details record
        HandleDetails = #handle_details{
            handle_service_id = proplists:get_value(
                <<"handle_service_id">>, Props),
            public_handle = proplists:get_value(
                <<"public_handle">>, Props, undefined),
            resource_type = proplists:get_value(
                <<"resource_type">>, Props, undefined),
            resource_id = proplists:get_value(
                <<"resource_id">>, Props, undefined),
            metadata = proplists:get_value(
                <<"metadata">>, Props, undefined),
            timestamp = deserialize_timestamp(proplists:get_value(
                <<"timestamp">>, Props, [0, 0, 0, 0, 0, 0]))
        },
        {ok, HandleDetails}
    end).


%%-------------------------------------------------------------------
%% @doc
%% @private
%% Translates list of integers that come from OZ into
%% erlang datetime format.
%% @end
%%-------------------------------------------------------------------
-spec deserialize_timestamp([integer()]) -> calendar:datetime().
deserialize_timestamp([A, B, C, D, E, F]) ->
    {{A, B, C}, {D, E, F}}.
