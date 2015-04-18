%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc This module allows for providers management in Global Registry.
%% @end
%% ===================================================================

-module(gr_providers).

-include("global_registry/gr_runner.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_providers.hrl").

%% API
-export([register/2, unregister/1, get_details/1, get_details/2, modify_details/2]).
-export([check_ip_address/1, check_port/4]).
-export([create_space/2, support_space/2, revoke_space_support/2, get_spaces/1,
    get_space_details/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% register/2
%% ====================================================================
%% @doc Registers provider in Global Registry. Parameters should contain:
%% "csr" that will be signed by Global Registry, "urls" to cluster nodes
%% "redirectionPoint" to provider's GUI and "gr_endpoint:client()Name".
%% @end
-spec register(Client :: gr_endpoint:client(), Parameters :: gr_endpoint:parameters()) ->
    {ok, ProviderId :: binary(), Cert :: binary()} | {error, Reason :: term()}.
%% ====================================================================
register(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:noauth_request(Client, URN, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderId = proplists:get_value(<<"providerId">>, Proplist),
        Cert = proplists:get_value(<<"certificate">>, Proplist),
        {ok, ProviderId, Cert}
    end).

%% unregister/1
%% ====================================================================
%% @doc Unregisters provider from Global Registry.
%% @end
-spec unregister(Client :: gr_endpoint:client()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
unregister(Client) ->
    ?run(fun() ->
        URN = "/provider",
        {ok, "202", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%% get_details/1
%% ====================================================================
%% @doc Returns public details about provider.
%% @end
-spec get_details(Client :: gr_endpoint:client()) ->
    {ok, ProviderDetails :: #provider_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_details(Client) ->
    ?run(fun() ->
        URN = "/provider",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderDetails = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            name = proplists:get_value(<<"clientName">>, Proplist),
            urls = proplists:get_value(<<"urls">>, Proplist),
            redirection_point = proplists:get_value(<<"redirectionPoint">>, Proplist)
        },
        {ok, ProviderDetails}
    end).

%% get_details/2
%% ====================================================================
%% @doc Returns public details about given provider.
%% @end
-spec get_details(Client :: gr_endpoint:client(), ProviderId :: binary()) ->
    {ok, ProviderDetails :: #provider_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_details(Client, ProviderId) ->
    ?run(fun() ->
        URN = "/provider/" ++ binary_to_list(ProviderId),
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderDetails = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            name = proplists:get_value(<<"clientName">>, Proplist),
            urls = proplists:get_value(<<"urls">>, Proplist),
            redirection_point = proplists:get_value(<<"redirectionPoint">>, Proplist)
        },
        {ok, ProviderDetails}
    end).

%% modify_details/2
%% ====================================================================
%% @doc Modifies public details about provider. Parameters may contain:
%% "urls" to cluster nodes and "redirectionPoint" to provider's GUI.
%% @end
-spec modify_details(Client :: gr_endpoint:client(),
    Parameters :: gr_endpoint:parameters()) -> ok | {error, Reason :: term()}.
%% ====================================================================
modify_details(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, patch, Body),
        ok
    end).

%% check_ip_address/2
%% ====================================================================
%% @doc Returns ip address that is visible for Global Registry.
%% @end
-spec check_ip_address(Client :: gr_endpoint:client()) ->
    {ok, IpAddress :: binary()} | {error, Reason :: term()}.
%% ====================================================================
check_ip_address(Client) ->
    ?run(fun() ->
        URN = "/provider/test/check_my_ip",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:noauth_request(Client, URN, get, []),
        IpAddress = mochijson2:decode(ResponseBody),
        {ok, IpAddress}
    end).

%% check_port/4
%% ====================================================================
%% @doc Checks port availability for Global Registry.
%% @end
-spec check_port(Client :: gr_endpoint:client(), IpAddress :: binary(),
    Port :: integer(), Type :: binary()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
check_port(Client, IpAddress, Port, Type) ->
    ?run(fun() ->
        URN = "/provider/test/check_my_ports",
        Resource = case Type of
                       <<"gui">> -> <<"/connection_check">>;
                       <<"rest">> -> <<"/rest/latest/connection_check">>
                   end,
        CheckURL = <<"https://", IpAddress/binary, ":",
        (integer_to_binary(Port))/binary, Resource/binary>>,
        Body = iolist_to_binary(mochijson2:encode([{Type, CheckURL}])),
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:noauth_request(Client, URN, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        <<"ok">> = proplists:get_value(CheckURL, Proplist),
        ok
    end).

%% create_space/2
%% ====================================================================
%% @doc Creates new Space and makes provider support created Space.
%% User/group that has given provider a token receives all privileges
%% for new Space. Parameters should contain: "name" of new Space,
%% "token" associated with user/group and "size" in bytes the provider
%% intends to give for the Space.
%% @end
-spec create_space(Client :: gr_endpoint:client(),
    Parameters :: gr_endpoint:parameters()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_space(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> =
            list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    end).

%% support_space/2
%% ====================================================================
%% @doc Makes provider support user's/group's Space that has given him a token.
%% Parameters should contain: "token" associated with user/group and
%% "size" in bytes the provider intends to give for the Space.
%% @end
-spec support_space(Client :: gr_endpoint:client(),
    Parameters :: gr_endpoint:parameters()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
support_space(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider/spaces/support",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, post, Body),
        <<"/provider/spaces/", SpaceId/binary>> =
            list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    end).

%% revoke_space_support/2
%% ====================================================================
%% @doc Makes provider stop supporting Space.
%% @end
-spec revoke_space_support(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
revoke_space_support(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, "202", _ResponseHeaders, _ResponseBody} =
            gr_endpoint:auth_request(Client, URN, delete),
        ok
    end).

%% get_spaces/1
%% ====================================================================
%% @doc Returns list of IDs of Spaces supported by provider.
%% @end
-spec get_spaces(Client :: gr_endpoint:client()) ->
    {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_spaces(Client) ->
    ?run(fun() ->
        URN = "/provider/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} =
            gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    end).

%% get_space_details/2
%% ====================================================================
%% @doc Returns public details about Space supported by provider.
%% @end
-spec get_space_details(Client :: gr_endpoint:client(), SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_space_details(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:auth_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceDetails = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            size = proplists:get_value(<<"size">>, Proplist)
        },
        {ok, SpaceDetails}
    end).
