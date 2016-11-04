%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility SSL functions.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl_utils).
-author("Krzysztof Trzepla").

%% API
-export([weak_ciphers/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns list of weak ciphers.
%% @end
%%--------------------------------------------------------------------
-spec weak_ciphers() -> list().
weak_ciphers() ->
    [{dhe_rsa, des_cbc, sha}, {rsa, des_cbc, sha}].