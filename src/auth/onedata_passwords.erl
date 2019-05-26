%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for hashing and verifying
%%% user passwords.
%%% @end
%%%--------------------------------------------------------------------
-module(onedata_passwords).
-author("Lukasz Opiola").

-define(SALT_LENGTH, 32).
-define(HASH_LENGTH, 32).
-define(JOIN_CHAR, $.).
-define(PBKDF2_ITERATIONS, 2048).
-define(ALG, {hmac, sha512}).


%% API
-export([create_hash/1, verify/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new hash of given password and returns a string that includes
%% used random salt and password hash. It can be placed directly into database.
%% @end
%%--------------------------------------------------------------------
-spec create_hash(Password :: binary()) -> SaltAndPasswdHash :: binary().
create_hash(Password) ->
    Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
    {ok, Hash} = pbkdf2:pbkdf2(?ALG, Password, Salt, ?PBKDF2_ITERATIONS, ?HASH_LENGTH),
    <<Salt/binary, ?JOIN_CHAR, Hash/binary>>.


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given password is correct based on provided hash obtained via
%% create_hash/1 function. The hash is a concatenation of salt used for its
%% computation and derived password hash.
%% @end
%%--------------------------------------------------------------------
-spec verify(Password :: binary(), SaltAndPasswdHash :: binary()) -> boolean().
verify(Password, SaltAndPasswdHash) ->
    <<Salt:?SALT_LENGTH/binary, ?JOIN_CHAR, PasswordHash/binary>> = SaltAndPasswdHash,
    case pbkdf2:pbkdf2(?ALG, Password, Salt, ?PBKDF2_ITERATIONS, ?HASH_LENGTH) of
        {ok, PasswordHash} -> true;
        _ -> false
    end.

