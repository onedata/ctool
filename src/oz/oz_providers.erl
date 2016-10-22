%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for providers management in OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_providers).

-include("oz/oz_runner.hrl").
-include("oz/oz_spaces.hrl").
-include("oz/oz_providers.hrl").
-include("oz/oz_openid.hrl").

%% API
-export([register/2, register_with_uuid/2, unregister/1]).
-export([get_details/1, get_details/2, modify_details/2]).
-export([get_token_issuer/2]).
-export([check_ip_address/1, check_port/4]).
-export([create_space/2, support_space/2, revoke_space_support/2, get_spaces/1,
    get_space_details/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Registers provider in OZ. Parameters should contain:
%% "csr" that will be signed by OZ, "urls" to cluster nodes
%% "redirectionPoint" to provider's GUI and "clientName".
%% @end
%%--------------------------------------------------------------------
-spec register(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, ProviderId :: binary(), Cert :: binary()} |
    {error, Reason :: term()}.
register(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/provider",
        Body = json_utils:encode(Parameters),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        Proplist = json_utils:decode(ResponseBody),
        ProviderId = lists_utils:key_get(<<"providerId">>, Proplist),
        Cert = lists_utils:key_get(<<"certificate">>, Proplist),
        {ok, ProviderId, Cert}
    end).

%%--------------------------------------------------------------------
%% @doc Registers provider in OZ with given UUI.
%% This is used mainly for tests. Parameters should contain:
%% "csr" that will be signed by OZ, "urls" to cluster nodes
%% "redirectionPoint" to provider's GUI, "clientName" and "uuid".
%% @end
%%--------------------------------------------------------------------
-spec register_with_uuid(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, ProviderId :: binary(), Cert :: binary()} |
    {error, Reason :: term()}.
register_with_uuid(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/provider_dev",
        Body = json_utils:encode(Parameters),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        Proplist = json_utils:decode(ResponseBody),
        ProviderId = lists_utils:key_get(<<"providerId">>, Proplist),
        Cert = lists_utils:key_get(<<"certificate">>, Proplist),
        {ok, ProviderId, Cert}
    end).

%%--------------------------------------------------------------------
%% @doc Unregisters provider from OZ.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Auth :: oz_endpoint:auth()) ->
    ok | {error, Reason :: term()}.
unregister(Auth) ->
    ?run(fun() ->
        URN = "/provider",
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth()) ->
    {ok, ProviderDetails :: #provider_details{}} | {error, Reason :: term()}.
get_details(Auth) ->
    ?run(fun() ->
        URN = "/provider",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        ProviderDetails = #provider_details{
            id = lists_utils:key_get(<<"providerId">>, Proplist),
            name = lists_utils:key_get(<<"clientName">>, Proplist),
            urls = lists_utils:key_get(<<"urls">>, Proplist),
            redirection_point = lists_utils:key_get(<<"redirectionPoint">>, Proplist),
            latitude = lists_utils:key_get(<<"latitude">>, Proplist),
            longitude = lists_utils:key_get(<<"longitude">>, Proplist)
        },
        {ok, ProviderDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: oz_endpoint:auth(), ProviderId :: binary()) ->
    {ok, ProviderDetails :: #provider_details{}} | {error, Reason :: term()}.
get_details(Auth, ProviderId) ->
    ?run(fun() ->
        URN = "/provider/" ++ binary_to_list(ProviderId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        ProviderDetails = #provider_details{
            id = lists_utils:key_get(<<"providerId">>, Proplist),
            name = lists_utils:key_get(<<"clientName">>, Proplist),
            urls = lists_utils:key_get(<<"urls">>, Proplist),
            redirection_point = lists_utils:key_get(<<"redirectionPoint">>, Proplist),
            latitude = lists_utils:key_get(<<"latitude">>, Proplist),
            longitude = lists_utils:key_get(<<"longitude">>, Proplist)
        },
        {ok, ProviderDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about provider. Parameters may contain:
%% "urls" to cluster nodes and "redirectionPoint" to provider's GUI.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/provider",
        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns token issuer.
%% @end
%%--------------------------------------------------------------------
-spec get_token_issuer(Auth :: oz_endpoint:auth(), Token :: binary()) ->
    {ok, TokenData :: #token_issuer{}} | {error, Reason :: term()}.
get_token_issuer(Auth, Token) ->
    ?run(fun() ->
        URN = "/provider/token/" ++ binary_to_list(Token),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        TokenIssuer = #token_issuer{
            client_type = lists_utils:key_get(<<"clientType">>, Proplist),
            client_id = lists_utils:key_get(<<"clientId">>, Proplist)
        },
        {ok, TokenIssuer}
    end).

%%--------------------------------------------------------------------
%% @doc Returns ip address that is visible for OZ.
%% @end
%%--------------------------------------------------------------------
-spec check_ip_address(Auth :: oz_endpoint:auth()) ->
    {ok, IpAddress :: binary()} | {error, Reason :: term()}.
check_ip_address(Auth) ->
    ?run(fun() ->
        URN = "/provider/test/check_my_ip",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get, <<>>),
        IpAddress = json_utils:decode(ResponseBody),
        {ok, IpAddress}
    end).

%%--------------------------------------------------------------------
%% @doc Checks port availability for OZ.
%% @end
%%--------------------------------------------------------------------
-spec check_port(Auth :: oz_endpoint:auth(), IpAddress :: binary(),
    Port :: integer(), Type :: binary()) ->
    ok | {error, Reason :: term()}.
check_port(Auth, IpAddress, Port, Type) ->
    ?run(fun() ->
        URN = "/provider/test/check_my_ports",
        Resource = case Type of
            <<"gui">> -> <<"/connection_check">>;
            <<"rest">> -> <<"/rest/latest/connection_check">>
        end,
        CheckURL = <<"https://", IpAddress/binary, ":",
            (integer_to_binary(Port))/binary, Resource/binary>>,
        Body = json_utils:encode([{Type, CheckURL}]),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        Proplist = json_utils:decode(ResponseBody),
        <<"ok">> = lists_utils:key_get(CheckURL, Proplist),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Creates new Space and makes provider support created Space.
%% User/group that has given provider a token receives all privileges
%% for new Space. Parameters should contain: "name" of new Space,
%% "token" associated with user/group and "size" in bytes the provider
%% intends to give for the Space.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
create_space(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/provider/spaces",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> =
            lists_utils:key_get(<<"location">>, ResponseHeaders),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes provider support user's/group's Space that has given him a token.
%% Parameters should contain: "token" associated with user/group and
%% "size" in bytes the provider intends to give for the Space.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: term()}.
support_space(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/provider/spaces/support",
        Body = json_utils:encode(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, post, Body),
        <<"/provider/spaces/", SpaceId/binary>> =
            lists_utils:key_get(<<"location">>, ResponseHeaders),
        {ok, SpaceId}
    end).

%%--------------------------------------------------------------------
%% @doc Makes provider stop supporting Space.
%% @end
%%--------------------------------------------------------------------
-spec revoke_space_support(Auth :: oz_endpoint:auth(),
    SpaceId :: binary()) -> ok | {error, Reason :: term()}.
revoke_space_support(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, 202, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, delete),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns list of IDs of Spaces supported by provider.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Auth :: oz_endpoint:auth()) ->
    {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
get_spaces(Auth) ->
    ?run(fun() ->
        URN = "/provider/spaces",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        SpaceIds = lists_utils:key_get(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    end).

%%--------------------------------------------------------------------
%% @doc Returns public details about Space supported by provider.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Auth :: oz_endpoint:auth(), SpaceId :: binary()) ->
    {ok, SpaceDetails :: #space_details{}} | {error, Reason :: term()}.
get_space_details(Auth, SpaceId) ->
    ?run(fun() ->
        URN = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, get),
        Proplist = json_utils:decode(ResponseBody),
        SpaceDetails = #space_details{
            id = lists_utils:key_get(<<"spaceId">>, Proplist),
            name = lists_utils:key_get(<<"name">>, Proplist),
            providers_supports = lists_utils:key_get(
                <<"providersSupports">>, Proplist)
        },
        {ok, SpaceDetails}
    end).
