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
-export([allowed_server_ciphers/0]).

% must return the ciphers in Erlang API format (@see ssl_cipher_format.erl)
-define(ALLOWED_CIPHERS_MFA, ctool:get_env(ssl_allowed_server_ciphers_mfa, {ssl, cipher_suites, [default, 'tlsv1.3']})).
% must contain a list of ciphers as RFC strings
-define(EXTRA_CIPHERS, ctool:get_env(ssl_extra_server_ciphers, [])).
% must contain a list of ciphers as RFC strings
-define(BLACKLISTED_CIPHERS, ctool:get_env(ssl_blacklisted_server_ciphers, [])).


%%%===================================================================
%%% API functions
%%%===================================================================

-spec allowed_server_ciphers() -> list().
allowed_server_ciphers() ->
    {Module, Function, Args} = ?ALLOWED_CIPHERS_MFA,
    Ciphers = erlang:apply(Module, Function, Args),
    ExtraCiphers = [ssl_cipher_format:suite_str_to_map(S) || S <- ?EXTRA_CIPHERS],
    BlacklistedCiphers = [ssl_cipher_format:suite_str_to_map(S) || S <- ?BLACKLISTED_CIPHERS],
    lists_utils:subtract(lists_utils:union(Ciphers, ExtraCiphers), BlacklistedCiphers).
