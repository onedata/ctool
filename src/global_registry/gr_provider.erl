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
-export([register/1, unregister/0, get_info/0, modify_info/1]).
-export([create_space/1, support_space/1, cancel_support/1, get_spaces/0, get_space_info/1]).


%% register/1
%% ====================================================================
%% @doc Registers provider in Global Registry. Parameters should contain:
%% "csr" that will be signed by Global Registry, "urls" to cluster nodes
%% and "redirectionPoint" to provider's GUI.
-spec register(Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, ProviderId :: binary(), Cert :: binary()} | {error, Reason :: term()}.
%% ====================================================================
register(Parameters) ->
    try
        URI = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:provider_request(URI, post, Body),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        ProviderId = proplists:get_value(<<"providerId">>, Proplist),
        Cert = proplists:get_value(<<"certificate">>, Proplist),
        {ok, ProviderId, Cert}
    catch
        _:Reason -> {error, Reason}
    end.


%% unregister/0
%% ====================================================================
%% @doc Unregisters provider from Global Registry.
-spec unregister() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
unregister() ->
    try
        URI = "/provider",
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:provider_request(URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_info/0
%% ====================================================================
%% @doc Returns public information about provider.
-spec get_info() -> Result when
    Result :: {ok, ProviderInfo :: #provider_info{}} | {error, Reason :: term()}.
%% ====================================================================
get_info() ->
    try
        URI = "/provider",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:provider_request(URI, get),
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


%% modify_info/1
%% ====================================================================
%% @doc Modify public information about provider. Parameters may contain:
%% "urls" to cluster nodes and "redirectionPoint" to provider's GUI.
-spec modify_info(Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
modify_info(Parameters) ->
    try
        URI = "/provider",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "200", _ResponseHeaders, _ResponseBody} = gr_endpoint:provider_request(URI, patch, Body),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% create_space/1
%% ====================================================================
%% @doc Creates new space and makes provider support created space.
%% User/group that has given provider a token receives all privileges
%% for new space. Parameters should contain: "name" of new space and
%% "token" associated with user/group.
-spec create_space(Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_space(Parameters) ->
    try
        URI = "/provider/spaces",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_endpoint:provider_request(URI, post, Body),
        <<"/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% create_space/1
%% ====================================================================
%% @doc Makes provider support user's/group's space that has given him a token.
%% Parameters should contain: "token" associated with user/group.
-spec create_space(Parameters :: [{Key :: binary(), Value :: binary()}]) -> Result when
    Result :: {ok, SpaceId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
support_space(Parameters) ->
    try
        URI = "/provider/spaces/support",
        Body = iolist_to_binary(mochijson2:encode(Parameters)),
        {ok, "201", ResponseHeaders, _ResponseBody} = gr_utils:send_req(URI, post, Body),
        <<"/provider/spaces/", SpaceId/binary>> = list_to_binary(proplists:get_value("location", ResponseHeaders)),
        {ok, SpaceId}
    catch
        _:Reason -> {error, Reason}
    end.


%% cancel_support/1
%% ====================================================================
%% @doc Makes provider stop supporting given space.
-spec cancel_support(SpaceId :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
cancel_support(SpaceId) ->
    try
        URI = <<"/provider/spaces/", SpaceId>>,
        {ok, "204", _ResponseHeaders, _ResponseBody} = gr_endpoint:provider_request(URI, delete),
        ok
    catch
        _:Reason -> {error, Reason}
    end.


%% get_spaces/0
%% ====================================================================
%% @doc Returns list of space ids supported by provider.
-spec get_spaces() -> Result when
    Result :: {ok, SpaceIds :: [binary()]} | {error, Reason :: term()}.
%% ====================================================================
get_spaces() ->
    try
        URI = "/provider/spaces",
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:provider_request(URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
        {ok, SpaceIds}
    catch
        _:Reason -> {error, Reason}
    end.


%% get_space_info/1
%% ====================================================================
%% @doc Returns public information about space supported by provider.
-spec get_space_info(SpaceId :: binary()) -> Result when
    Result :: {ok, SpaceInfo :: #space_info{}} | {error, Reason :: term()}.
%% ====================================================================
get_space_info(SpaceId) ->
    try
        URI = <<"/provider/spaces/", SpaceId>>,
        {ok, "200", _ResponseHeaders, ResponseBody} = gr_endpoint:provider_request(URI, get),
        Proplist = mochijson2:decode(ResponseBody, [{format, proplist}]),
        SpaceInfo = #space_info{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist)
        },
        {ok, SpaceInfo}
    catch
        _:Reason -> {error, Reason}
    end.
