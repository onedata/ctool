%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Definitions related to rest_api_samples_test_utils.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(REST_API_SAMPLES_TEST_UTILS_HRL).
-define(REST_API_SAMPLES_TEST_UTILS_HRL, 1).

-include("./test_utils.hrl").


-define(DEFAULT_SETUP_FUN, fun(Context) -> Context end).
-define(DEFAULT_SUBSTITUTE_PLACEHOLDER_FUN, fun(_, P) -> error({unexpected_placeholder, P}) end).
-define(DEFAULT_VERIFY_FUN, fun(_, _) -> error(not_implemented) end).


-record(sample_test_spec, {
    % samples with lower values are tested first
    testing_priority = 0 :: non_neg_integer(),
    setup_fun = ?DEFAULT_SETUP_FUN :: rest_api_samples_test_utils:sample_test_setup_fun(),
    substitute_placeholder_fun = ?DEFAULT_SUBSTITUTE_PLACEHOLDER_FUN :: rest_api_samples_test_utils:sample_test_substitute_placeholder_fun(),
    verify_fun = ?DEFAULT_VERIFY_FUN  :: rest_api_samples_test_utils:sample_test_severity_fun()
}).


-endif.
