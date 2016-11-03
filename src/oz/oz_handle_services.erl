%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for handle_service management in OZ.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_handle_services).

-include("oz/oz_runner.hrl").
-include("oz/oz_handle_services.hrl").

%% API
-export([get_details/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns public details about a handle_service.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth(), HandleServiceId :: binary()) ->
    {ok, HandleServiceDetails :: #handle_service_details{}} |
    {error, Reason :: term()}.
get_details(Auth, HandleServiceId) ->
    ?run(fun() ->
        URN = "/handle_services/" ++ binary_to_list(HandleServiceId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, get),
        Props = json_utils:decode(ResponseBody),
        % Get default values of share_details record
        HandleServiceDetails = #handle_service_details{
            id = lists_utils:key_get(<<"handleServiceId">>, Props),
            name = lists_utils:key_get(
                <<"name">>, Props, undefined),
            proxy_endpoint = lists_utils:key_get(
                <<"proxyEndpoint">>, Props, undefined),
            service_properties = lists_utils:key_get(
                <<"serviceProperties">>, Props, [])
        },
        {ok, HandleServiceDetails}
    end).