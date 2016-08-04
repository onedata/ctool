%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows identity info retrieval from OZ.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_identities).

-include("oz/oz_runner.hrl").
-include("oz/oz_spaces.hrl").
-include("oz/oz_providers.hrl").
-include("oz/oz_openid.hrl").
-include("logging.hrl").

%% API
-export([get_public_key/2, set_public_key/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Retrieves identity info (public key) from OZ for given ID.
%% This info can be used to verify identity basing on certificate info.
%% @end
%%--------------------------------------------------------------------
-spec get_public_key(Auth :: oz_endpoint:auth(), ID :: identity:id()) ->
    {ok, identity:public_key()} |
    {error, Reason :: term()}.
get_public_key(Auth, ID) ->
    ?run(fun() ->
        EncodedID = binary_to_list(http_utils:url_encode(ID)),
        URN = "/publickey/" ++ EncodedID,
        {ok, 200, _ResponseHeaders, ResponseBody} =
            oz_endpoint:noauth_request(Auth, URN, get, [], [insecure]),
        Data = json_utils:decode(ResponseBody),
        EncodedPublicKey = proplists:get_value(<<"publicKey">>, Data),
        PublicKey = binary_to_term(base64:decode(EncodedPublicKey)),
        {ok, PublicKey}
    end).

%%--------------------------------------------------------------------
%% @doc Setups identity info (public key) in OZ for given ID.
%% @end
%%--------------------------------------------------------------------
-spec set_public_key(Auth :: oz_endpoint:auth(), ID :: identity:id(),
    PublicKey :: identity:public_key()) ->
    ok | {error, Reason :: term()}.
set_public_key(Auth, ID, PublicKey) ->
    ?run(fun() ->
        EncodedID = binary_to_list(http_utils:url_encode(ID)),
        URN = "/publickey/" ++ EncodedID,
        Encoded = base64:encode(term_to_binary(PublicKey)),
        Body = json_utils:encode([{<<"publicKey">>, Encoded}]),
        {ok, 204, _ResponseHeaders, _ResponseBody} =
            oz_endpoint:noauth_request(Auth, URN, post, Body, [insecure]),
        ok
    end).

