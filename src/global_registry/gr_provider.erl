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

-module(gr_provider).

-include("global_registry/gr_space.hrl").
-include("global_registry/gr_provider.hrl").

%% API
-export([register/2, unregister/1, get_info/1, modify_info/2]).
-export([create_space/2, support_space/2, cancel_support/2, get_spaces/1, get_space_info/2]).


%% register/2
%% ====================================================================
%% @doc Registers provider in Global Registry. Parameters should contain:
%% "csr" that will be signed by Global Registry, "urls" to cluster nodes
%% and "redirectionPoint" to provider's GUI.
-spec register(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, ProviderId :: binary(), Cert :: binary()} | {error, Reason :: term()}.
%% ====================================================================
register(Client, Parameters) ->
    try
        URI = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderId = proplists:get_value(<<"providerId">>, Proplist),
        Cert = proplists:get_value(<<"certificate">>, Proplist),
        {ok, ProviderId, Cert}
    catch
        _:Reason -> {error, Reason}
    end.


%% unregister/1
%% ====================================================================
%% @doc Unregisters provider from Global Registry.
-spec unregister(Client :: client()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
unregister(Client) ->
    try
        URI = "/provider",
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_info/1
%% ====================================================================
%% @doc Returns public information about provider.
-spec get_info(Client :: client()) -> Result when
    Result :: {ok, ProviderInfo :: #provider_info{}} | {error, Reason :: term()}.
%% ====================================================================
get_info(Client) ->
    try
        URI = "/provider",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderInfo = #provider_info{
            id = proplists:get_value(<<"providerId">>, Proplist),
            urls = proplists:get_value(<<"urls">>, Proplist),
            redirection_point = proplists:get_value(<<"redirectionPoint">>, Proplist)
        },
        {ok, ProviderInfo}
    catch
        _:Reason -> {error, Reason}
    end.


%% modify_info/2
%% ====================================================================
%% @doc Modify public information about provider. Parameters may contain:
%% "urls" to cluster nodes and "redirectionPoint" to provider's GUI.
-spec modify_info(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_info(Client, Parameters) ->
    try
        URI = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, patch, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% create_space/2
%% ====================================================================
%% @doc Creates new space and makes provider support created space.
%% User/group that has given provider a token receives all privileges
%% for new space. Parameters should contain: "name" of new space and
%% "token" associated with user/group.
-spec create_space(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_space(Client, Parameters) ->
    try
        URI = "/provider/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, post, Body),
        <<"/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% support_space/2
%% ====================================================================
%% @doc Makes provider support user's/group's space that has given him a token.
%% Parameters should contain: "token" associated with user/group.
-spec support_space(Client :: client(), Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
support_space(Client, Parameters) ->
    try
        URI = "/provider/spaces/support",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, post, Body),
        <<"/provider/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% cancel_support/2
%% ====================================================================
%% @doc Makes provider stop supporting given space.
-spec cancel_support(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
cancel_support(Client, SpaceId) ->
    try
        URI = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:request(Client, URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_spaces/1
%% ====================================================================
%% @doc Returns list of ids of spaces supported by provider.
-spec get_spaces(Client :: client()) -> Result when
    Result :: {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_spaces(Client) ->
    try
        URI = "/provider/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_space_info/2
%% ====================================================================
%% @doc Returns public information about space supported by provider.
-spec get_space_info(Client :: client(), SpaceId :: binary()) -> Result when
    Result :: {ok, SpaceInfo :: #space_info{}} | {error, Reason :: term()}.
%% ====================================================================
get_space_info(Client, SpaceId) ->
    try
        URI = "/provider/spaces/" ++ binary_to_list(SpaceId),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:request(Client, URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceInfo = #space_info{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, SpaceInfo}
    catch
        _:Reason -> {error, Reason}
    end.
