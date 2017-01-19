%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of oz_handles module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_handles_tests).

-ifdef(TEST).

-include("oz/oz_handles.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

oz_handles_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"create", fun should_create/0},
            {"get details", fun should_get_details/0},
            {"get public details", fun should_get_public_details/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_endpoint),
    meck:expect(oz_endpoint, provider_request, fun
        (client, "/handles/handleId", get) ->
            {ok, 200, response_headers, response_body}
    end),
    meck:expect(oz_endpoint, request, fun
        (client, "/handles/handleId/public", get) ->
            {ok, 200, response_headers, response_body}
    end),
    meck:expect(oz_endpoint, provider_request, fun
        (client, "/handles", post, <<"body">>) ->
            {ok, 201, #{<<"location">> => <<"/handles/handleId">>}, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(oz_endpoint)),
    ok = meck:unload(oz_endpoint).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_create() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_handles:create(client, parameters),
    ?assertEqual({ok, <<"handleId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_details() ->
    Datestamp = <<"2016-01-01T10:00:00Z">>,
    Datetime = timestamp_utils:datestamp_to_datetime(Datestamp),
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [
            {<<"handleId">>, <<"handleId">>},
            {<<"handleServiceId">>, <<"val_handle_service_id">>},
            {<<"handle">>, <<"val_public_handle">>},
            {<<"resourceType">>, <<"val_resource_type">>},
            {<<"resourceId">>, <<"val_resource_id">>},
            {<<"metadata">>, <<"val_metadata">>},
            {<<"timestamp">>, Datestamp}
        ]
    end),

    Answer = oz_handles:get_details(client, <<"handleId">>),
    ?assertEqual({ok, #handle_details{
        id = <<"handleId">>,
        handle_service = <<"val_handle_service_id">>,
        public_handle = <<"val_public_handle">>,
        resource_type = <<"val_resource_type">>,
        resource_id = <<"val_resource_id">>,
        metadata = <<"val_metadata">>,
        timestamp = Datetime
    }}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_public_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [
            {<<"handleId">>, <<"handleId">>},
            {<<"handle">>, <<"val_public_handle">>},
            {<<"metadata">>, <<"val_metadata">>}
        ]
    end),

    Answer = oz_handles:get_public_details(client, <<"handleId">>),
    ?assertEqual({ok, #handle_details{
        id = <<"handleId">>,
        handle_service = undefined,
        public_handle = <<"val_public_handle">>,
        resource_type = undefined,
        resource_id = undefined,
        metadata = <<"val_metadata">>,
        timestamp = undefined
    }}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

-endif.
