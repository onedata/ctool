%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'
%% @end
%% ===================================================================
%% @doc: This module allows for providers management in Global Registry.
%% @end
%% ===================================================================

-module(gr_providers).

-include("global_registry/gr_runner.hrl").
-include("global_registry/gr_types.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_providers.hrl").

%% API
-export([register/2, unregister/1, get_details/1, get_details/2, modify_details/2]).
-export([check_ip_address/2, check_port/4]).
-export([create_space/2, support_space/2, cancel_space_support/2, get_spaces/1, get_space_details/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% register/2
%% ====================================================================
%% @doc Registers provider in Global Registry. Parameters should contain:
%% "csr" that will be signed by Global Registry, "urls" to cluster nodes
%% and "redirectionPoint" to provider's GUI.
-spec register(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, ProviderId :: binary(), Cert :: binary()} | {error, Reason :: term()}.
%% ====================================================================
register(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:insecure_request(Client, URN, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderId = proplists:get_value(<<"providerId">>, Proplist),
        Cert = proplists:get_value(<<"certificate">>, Proplist),
        {ok, ProviderId, Cert}
    end).


%% unregister/1
%% ====================================================================
%% @doc Unregisters provider from Global Registry.
-spec unregister(Client :: client()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
unregister(Client) ->
    ?run(fun() ->
        URN = "/provider",
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, delete),
        ok
    end).


%% get_details/1
%% ====================================================================
%% @doc Returns public details about provider.
-spec get_details(Client :: client()) -> Result when
    Result :: {ok, ProviderInfo :: #provider_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_details(Client) ->
    ?run(fun() ->
        URN = "/provider",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderInfo = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            urls = proplists:get_value(<<"urls">>, Proplist),
            redirection_point = proplists:get_value(<<"redirectionPoint">>, Proplist)
        },
        {ok, ProviderInfo}
    end).


%% get_details/2
%% ====================================================================
%% @doc Returns public details about given provider.
-spec get_details(Client :: client(), ProviderId :: binary()) -> Result when
    Result :: {ok, ProviderInfo :: #provider_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_details(Client, ProviderId) ->
    ?run(fun() ->
        URN = "/provider/" ++ binary_to_list(ProviderId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderInfo = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            urls = proplists:get_value(<<"urls">>, Proplist),
            redirection_point = proplists:get_value(<<"redirectionPoint">>, Proplist)
        },
        {ok, ProviderInfo}
    end).


%% modify_details/2
%% ====================================================================
%% @doc Modifies public details about provider. Parameters may contain:
%% "urls" to cluster nodes and "redirectionPoint" to provider's GUI.
-spec modify_details(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_details(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, patch, Body),
        ok
    end).


%% check_ip_address/2
%% ====================================================================
%% @doc Returns ip address that is visible for Global Registry.
%% @end
-spec check_ip_address(Client :: client(), ConnectTimeout :: integer()) -> Result when
    Result :: {ok, IpAddress :: binary()} | {error, Reason :: term()}.
%% ====================================================================
check_ip_address(Client, ConnectTimeout) ->
    ?run(fun() ->
        URN = "/provider/test/check_my_ip",
        Options = [{connect_timeout, ConnectTimeout}],
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:insecure_request(Client, URN, get, [], Options),
        IpAddress = mochijson2:decode(ResponseBody),
        {ok, IpAddress}
    end).


%% check_port/4
%% ====================================================================
%% @doc Checks port availability for Global Registry.
%% @end
-spec check_port(Client :: client(), IpAddress :: binary(), Port :: integer(), Type :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
check_port(Client, IpAddress, Port, Type) ->
    ?run(fun() ->
        URN = "/provider/test/check_my_ports",
        Resource = case Type of
                       <<"gui">> -> <<"/connection_check">>;
                       <<"rest">> -> <<"/rest/latest/connection_check">>
                   end,
        CheckURL = <<"https://", IpAddress/binary, ":", (integer_to_binary(Port))/binary, Resource/binary>>,
        Body = iolist_to_binary(mochijson2:encode([{Type, CheckURL}])),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:insecure_request(Client, URN, get, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        <<"ok">> = proplists:get_value(CheckURL, Proplist),
        ok
    end).


%% create_space/2
%% ====================================================================
%% @doc Creates new Space and makes provider support created Space.
%% User/group that has given provider a token receives all privileges
%% for new Space. Parameters should contain: "name" of new Space and
%% "token" associated with user/group.
-spec create_space(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_space(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        <<"/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    end).


%% support_space/2
%% ====================================================================
%% @doc Makes provider support user's/group's Space that has given him a token.
%% Parameters should contain: "token" associated with user/group.
-spec support_space(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
support_space(Client, Parameters) ->
    ?run(fun() ->
        URN = "/provider/spaces/support",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, post, Body),
        <<"/provider/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    end).


%% cancel_space_support/2
%% ====================================================================
%% @doc Makes provider stop supporting Space.
-spec cancel_space_support(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
cancel_space_support(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:secure_request(Client, URN, delete),
        ok
    end).


%% get_spaces/1
%% ====================================================================
%% @doc Returns list of IDs of Spaces supported by provider.
-spec get_spaces(Client :: client()) -> Result when
    Result :: {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_spaces(Client) ->
    ?run(fun() ->
        URN = "/provider/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    end).


%% get_space_details/2
%% ====================================================================
%% @doc Returns public details about Space supported by provider.
-spec get_space_details(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, SpaceInfo :: #space_details{}} | {error, Reason :: term()}.
%% ====================================================================
get_space_details(Client, SpaceId) ->
    ?run(fun() ->
        URN = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:secure_request(Client, URN, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceInfo = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, SpaceInfo}
    end).
