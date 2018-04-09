%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of oz_handle_services module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_handle_services_tests).

-ifdef(TEST).

-include("oz/oz_handle_services.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

oz_handle_services_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"get details", fun should_get_details/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_endpoint),
    meck:expect(oz_endpoint, request, fun
        (client, "/handle_services/handleServiceId", get) ->
            {ok, 200, response_headers, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(oz_endpoint)),
    ok = meck:unload(oz_endpoint).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_get_details() ->
    meck:new(json_utils, [passthrough]),
    meck:expect(json_utils, decode_deprecated, fun(response_body) ->
        [
            {<<"handleServiceId">>, <<"handleServiceId">>},
            {<<"name">>, <<"val_name">>},
            {<<"proxyEndpoint">>, <<"val_proxy_endpoint">>},
            {<<"serviceProperties">>, <<"val_service_properties">>}
        ]
    end),

    Answer = oz_handle_services:get_details(client, <<"handleServiceId">>),
    ?assertEqual({ok, #handle_service_details{
        id = <<"handleServiceId">>,
        name = <<"val_name">>,
        proxy_endpoint = <<"val_proxy_endpoint">>,
        service_properties = <<"val_service_properties">>
    }}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

-endif.
