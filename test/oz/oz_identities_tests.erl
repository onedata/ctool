%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of oz_identities module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_identities_tests).

-ifdef(TEST).

-include("oz/oz_spaces.hrl").
-include("oz/oz_providers.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

oz_providers_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"register provider", fun should_register/0},
            {"set public key", fun should_set/0},
            {"get public key", fun should_get/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_endpoint),
    meck:expect(oz_endpoint, noauth_request, fun
        (client, "/provider_data/test", post, <<"body">>) ->
            {ok, 200, response_headers, response_body};
        (client, "/publickey/id", post, <<"body">>) ->
            {ok, 204, response_headers, response_body};
        (client, "/publickey/id", get, <<>>) ->
            {ok, 200, response_headers, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(oz_endpoint)),
    ok = meck:unload(oz_endpoint).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_register() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun([{<<"ID">>, <<"test">>}]) -> <<"body">> end),

    Answer = oz_identities:register_provider(client, [{<<"ID">>, <<"test">>}]),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

should_set() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun([{<<"publicKey">>, <<"pk">>}]) -> <<"body">> end),

    Answer = oz_identities:set_public_key(client, <<"id">>, <<"pk">>),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

should_get() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) -> [{<<"publicKey">>, <<"pk">>}] end),

    Answer = oz_identities:get_public_key(client, <<"id">>),
    ?assertEqual({ok, <<"pk">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

-endif.
