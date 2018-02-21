%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of onedata_passwords module.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_passwords_tests).
-author("Bartosz Walkowicz").

-include_lib("eunit/include/eunit.hrl").
-include_lib("api_errors.hrl").

-define(SALT_LENGTH, 32).
-define(JOIN_CHAR, $.).
-define(PASSWORD1, <<"secret1">>).
-define(PASSWORD2, <<"secret2">>).


verify_hashed_password_test() ->
    PasswordHash1 = onedata_passwords:create_hash(?PASSWORD1),
    ?assert(onedata_passwords:verify(?PASSWORD1, PasswordHash1)).


fail_verification_of_wrong_hashed_password_test() ->
    PasswordHash1 = onedata_passwords:create_hash(?PASSWORD1),
    ?assertNot(onedata_passwords:verify(?PASSWORD2, PasswordHash1)).


generate_different_salt_test() ->
    <<Salt1:?SALT_LENGTH/binary, ?JOIN_CHAR, PasswordHash1/binary>> =
        onedata_passwords:create_hash(?PASSWORD1),
    <<Salt2:?SALT_LENGTH/binary, ?JOIN_CHAR, PasswordHash2/binary>> =
        onedata_passwords:create_hash(?PASSWORD1),

    ?assertNotEqual(Salt1, Salt2),
    ?assertNotEqual(PasswordHash1, PasswordHash2).
