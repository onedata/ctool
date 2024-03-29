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

-include("logging.hrl").
-include("oz/oz_runner.hrl").
-include("oz/oz_spaces.hrl").
-include("oz/oz_providers.hrl").
-include("errors.hrl").

%% API
-export([register/2, register_with_uuid/2, unregister/1]).
-export([get_details/1, get_details/2, modify_details/2]).
-export([get_zone_time/1]).
-export([check_ip_address/1]).
-export([create_space/2, support_space/2, revoke_space_support/2, get_spaces/1,
    get_space_details/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Registers provider in OZ. Parameters should contain:
%% "name", "csr" that will be signed by OZ and "subdomainDelegation".
%% Depending on "subdomainDelegation" parameters should contain
%% "domain" (if delegation is "false")
%% or "subdomain" and "ipList" (if delegation is "true")
%% @end
%%--------------------------------------------------------------------
-spec register(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, map()} | errors:error().
register(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/providers",
        Body = json_utils:encode_deprecated(Parameters),

        case oz_endpoint:request(
            Auth, URN, post, Body, [{endpoint, rest_no_auth}]
        ) of
            {ok, 200, _ResponseHeaders, ResponseBody} ->
                {ok, json_utils:decode(ResponseBody)};

            {ok, 400, _ResponseHeaders, ErrorBody} ->
                #{<<"error">> := Error} = json_utils:decode(ErrorBody),
                errors:from_json(Error)
        end
    end).

%%--------------------------------------------------------------------
%% @doc Registers provider in OZ with given UUI.
%% This is used mainly for tests. Parameters should contain:
%% "name", "uuid", "csr" that will be signed by OZ and "subdomainDelegation".
%% Depending on "subdomainDelegation" parameters should contain
%% "domain" (if delegation is "false")
%% or "subdomain" and "ipList" (if delegation is "true")
%% @end
%%--------------------------------------------------------------------
-spec register_with_uuid(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    {ok, map()} |
    {error, Reason :: term()}.
register_with_uuid(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/providers/dev",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 200, _ResponseHeaders, ResponseBody} = oz_endpoint:request(
            Auth, URN, post, Body, [{endpoint, rest_no_auth}]
        ),
        {ok, json_utils:decode(ResponseBody)}
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
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
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
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        ProviderDetails = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            domain = proplists:get_value(<<"domain">>, Proplist),
            latitude = proplists:get_value(<<"latitude">>, Proplist, 0.0),
            longitude = proplists:get_value(<<"longitude">>, Proplist, 0.0)
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
        URN = "/providers/" ++ binary_to_list(ProviderId),
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        ProviderDetails = #provider_details{
            id = proplists:get_value(<<"providerId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            domain = proplists:get_value(<<"domain">>, Proplist),
            latitude = proplists:get_value(<<"latitude">>, Proplist, 0.0),
            longitude = proplists:get_value(<<"longitude">>, Proplist, 0.0)
        },
        {ok, ProviderDetails}
    end).

%%--------------------------------------------------------------------
%% @doc Modifies public details about provider. Parameters may contain:
%% and "domain" of provider.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) -> ok | {error, Reason :: term()}.
modify_details(Auth, Parameters) ->
    ?run(fun() ->
        URN = "/provider",
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Returns current clock time of Onezone, in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec get_zone_time(Auth :: oz_endpoint:auth()) ->
    {ok, time:millis()} | {error, Reason :: term()}.
get_zone_time(Auth) ->
    ?run(fun() ->
        URN = "/provider/public/get_current_time",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get, <<>>),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        Timestamp = proplists:get_value(<<"timeMillis">>, Proplist),
        true = Timestamp /= undefined,
        {ok, Timestamp}
    end).

%%--------------------------------------------------------------------
%% @doc Returns ip address that is visible for OZ.
%% @end
%%--------------------------------------------------------------------
-spec check_ip_address(Auth :: oz_endpoint:auth()) ->
    {ok, IpAddress :: binary()} | {error, Reason :: term()}.
check_ip_address(Auth) ->
    ?run(fun() ->
        URN = "/provider/public/check_my_ip",
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get, <<>>, [{endpoint, rest_no_auth}]),
        IpAddress = json_utils:decode_deprecated(ResponseBody),
        {ok, IpAddress}
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
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        SpaceId = http_utils:last_url_part(maps:get(?HDR_LOCATION, ResponseHeaders)),
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
        Body = json_utils:encode_deprecated(Parameters),
        {ok, 201, ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        SpaceId = http_utils:last_url_part(maps:get(?HDR_LOCATION, ResponseHeaders)),
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
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, delete),
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
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        SpaceIds = proplists:get_value(<<"spaces">>, Proplist),
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
            oz_endpoint:request(Auth, URN, get),
        Proplist = json_utils:decode_deprecated(ResponseBody),
        SpaceDetails = #space_details{
            id = proplists:get_value(<<"spaceId">>, Proplist),
            name = proplists:get_value(<<"name">>, Proplist),
            providers_supports = proplists:get_value(
                <<"providers">>, Proplist)
        },
        {ok, SpaceDetails}
    end).
