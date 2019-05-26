%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility SSL functions.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl_utils).
-author("Lukasz Opiola").

%% API
-export([safe_ciphers/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the list of safe ciphers (without TLS RSA Encryption).
%% @end
%%--------------------------------------------------------------------
-spec safe_ciphers() -> list().
safe_ciphers() ->
    [Suite || Suite <- ssl:cipher_suites(), element(1, Suite) =/= rsa].
