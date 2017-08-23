%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module manages identity info in OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_identities).

-include("oz/oz_runner.hrl").
-include("oz/oz_spaces.hrl").
-include("oz/oz_providers.hrl").
-include("oz/oz_openid.hrl").
-include("logging.hrl").

%% API
-export([get_public_key/2, update_own_public_key/3, register_provider/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Retrieves identity info (public key) from OZ for given ID.
%% This info can be used to verify identity basing on certificate info.
%% @end
%%--------------------------------------------------------------------
-spec get_public_key(Auth :: oz_endpoint:auth(), ID :: binary()) ->
    {ok, EncodedPublicKey :: binary()} |
    {error, Reason :: term()}.
get_public_key(Auth, ID) ->
    ?run(fun() ->
        EncodedID = binary_to_list(http_utils:url_encode(ID)),
        URN = "/publickey/" ++ EncodedID,
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:request(Auth, URN, get, <<>>),
        Data = json_utils:decode(ResponseBody),
        EncodedPublicKey = lists_utils:key_get(<<"publicKey">>, Data),
        {ok, EncodedPublicKey}
    end).

%%--------------------------------------------------------------------
%% @doc Setups identity info (public key) in OZ for given ID.
%% @end
%%--------------------------------------------------------------------
-spec update_own_public_key(Auth :: oz_endpoint:auth(), ID :: binary(),
    EncodedPublicKey :: binary()) ->
    ok | {error, Reason :: term()}.
update_own_public_key(Auth, ID, EncodedPublicKey) ->
    ?run(fun() ->
        EncodedID = binary_to_list(http_utils:url_encode(ID)),
        URN = "/publickey/" ++ EncodedID,
        Body = json_utils:encode([{<<"publicKey">>, EncodedPublicKey}]),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:provider_request(Auth, URN, patch, Body),
        ok
    end).

%%--------------------------------------------------------------------
%% @doc Registers provider in OZ. Parameters should contain:
%% "id" extracted from identity cert,
%% "publicKey" extracted from identity cert,
%% "domain" of provider
%% @end
%%--------------------------------------------------------------------
-spec register_provider(Auth :: oz_endpoint:auth(),
    Parameters :: oz_endpoint:params()) ->
    ok | {error, Reason :: term()}.
register_provider(Auth, Parameters) ->
    ?run(fun() ->
        ID = lists_utils:key_get(<<"id">>, Parameters),
        EncodedID = binary_to_list(http_utils:url_encode(ID)),
        URN = "/provider_data/" ++ EncodedID,

        Body = json_utils:encode(Parameters),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:request(Auth, URN, post, Body),
        ok
    end).

