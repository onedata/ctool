%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of the ssl_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl_utils_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


% ciphers affected by known vulnerabilities
-define(UNSAFE_CIPHERS, [
    "TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA",
    "TLS_DHE_RSA_WITH_DES_CBC_SHA",
    "TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA",
    "TLS_ECDHE_RSA_WITH_RC4_128_SHA",
    "TLS_RSA_PSK_WITH_RC4_128_SHA"
]).

-define(get_allowed_ciphers(),
    [ssl_cipher_format:suite_map_to_str(S) || S <- ssl_utils:allowed_server_ciphers()]
).

% Using the 'all' option (instead of 'default') causes the unsafe ciphers to be
% included (it returns all ciphers that are defined rather than the ones that are recommended).
-define(MFA_WITH_UNSAFE_CIPHERS, {ssl, cipher_suites, [all, 'tlsv1.3']}).

-define(containsAll(Set, Subset),
    lists:sort(Subset) == lists_utils:intersect(Set, Subset)
).
-define(containsNone(Set, Subset),
    [] == lists_utils:intersect(Set, Subset)
).


unsafe_ciphers_are_not_returned_test() ->
    ctool:unset_env(ssl_allowed_server_ciphers_mfa),
    ?assert(?containsNone(?get_allowed_ciphers(), ?UNSAFE_CIPHERS)).


unsafe_ciphers_can_be_enabled_if_you_are_reckless_test() ->
    ctool:set_env(ssl_allowed_server_ciphers_mfa, ?MFA_WITH_UNSAFE_CIPHERS),
    ?assert(?containsAll(?get_allowed_ciphers(), ?UNSAFE_CIPHERS)).


extra_ciphers_can_be_added_test() ->
    ctool:unset_env(ssl_allowed_server_ciphers_mfa),
    ExtraCiphers = [
        "TLS_DHE_RSA_WITH_DES_CBC_SHA",
        "TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA"
    ],
    ctool:set_env(ssl_extra_server_ciphers, ExtraCiphers),
    ?assert(?containsAll(?get_allowed_ciphers(), ExtraCiphers)),
    ?assertNot(?containsAll(?get_allowed_ciphers(), ?UNSAFE_CIPHERS)).


ciphers_can_be_blacklisted_test() ->
    ctool:set_env(ssl_allowed_server_ciphers_mfa, ?MFA_WITH_UNSAFE_CIPHERS),
    ctool:set_env(ssl_blacklisted_server_ciphers, ?UNSAFE_CIPHERS),
    ?assert(?containsNone(?get_allowed_ciphers(), ?UNSAFE_CIPHERS)).


all_envs_regarding_ciphers_can_be_combined_test() ->
    ctool:set_env(ssl_allowed_server_ciphers_mfa, ?MFA_WITH_UNSAFE_CIPHERS),
    % if a cipher is added in both opts, the blacklisting should prevail
    CipherSpecifiedInBothOpts = "TLS_ECDHE_RSA_WITH_RC4_128_SHA",
    ExtraCiphers = [
        "TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA",
        "TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA",
        CipherSpecifiedInBothOpts
    ],
    BlacklistedCiphers = [
        "TLS_DHE_RSA_WITH_DES_CBC_SHA",
        CipherSpecifiedInBothOpts,
        "TLS_RSA_PSK_WITH_RC4_128_SHA"
    ],
    ctool:set_env(ssl_extra_server_ciphers, ExtraCiphers),
    ctool:set_env(ssl_blacklisted_server_ciphers, BlacklistedCiphers),
    ?assert(?containsAll(?get_allowed_ciphers(), ExtraCiphers -- [CipherSpecifiedInBothOpts])),
    ?assert(?containsNone(?get_allowed_ciphers(), BlacklistedCiphers)).


-endif.