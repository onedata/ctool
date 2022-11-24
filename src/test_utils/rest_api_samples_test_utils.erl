%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper functions for testing REST api samples. All functions accept a
%%% JSON-decoded payload returned by the backend: {"rest": RestApiSamplesObject :: {...}}.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_api_samples_test_utils).
-author("Lukasz Opiola").

-include("api_samples/common.hrl").
-include("test/rest_api_samples_test_utils.hrl").

-export([verify_structure/3]).
-export([test_samples/5]).

% opaque structure relevant fot the calling module that will be passed to every callback
-type context() :: term().
-type operation_listing() :: [{rest_api_request_sample:method(), rest_api_request_sample:swagger_operation_id(), rest_api_request_sample:name()}].
-export_type([context/0, operation_listing/0]).

-type sample_test_setup_fun() :: fun((context()) -> context()).
-type sample_test_substitute_placeholder_fun() :: fun((context(), binary()) -> binary()).
-type sample_test_severity_fun() :: fun((context(), http_client:body()) -> ok).
-export_type([sample_test_setup_fun/0, sample_test_substitute_placeholder_fun/0, sample_test_severity_fun/0]).

-type sample_test_spec() :: #sample_test_spec{}.
-type build_sample_test_spec_fun() :: fun((rest_api_request_sample:name()) -> sample_test_spec()).
-export_type([sample_test_spec/0, build_sample_test_spec_fun/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec verify_structure(json_utils:json_term(), http_client:url(), operation_listing()) -> ok.
verify_structure(#{<<"rest">> := RestApiSamplesJson}, ExpectedApiRoot, ExpectedOperationListing) ->
    Samples = ?assertMatch(#rest_api_samples{}, jsonable_record:from_json(RestApiSamplesJson, rest_api_samples)),
    ApiRoot = Samples#rest_api_samples.api_root,
    ?assertEqual(ApiRoot, ExpectedApiRoot),

    OperationListing = lists:map(fun(Sample) ->
        ?assertMatch(#rest_api_request_sample{}, Sample),
        ?assertNot(string:is_empty(Sample#rest_api_request_sample.description)),
        Method = Sample#rest_api_request_sample.method,
        OperationId = Sample#rest_api_request_sample.swagger_operation_id,
        Name = Sample#rest_api_request_sample.name,
        {Method, OperationId, Name}
    end, Samples#rest_api_samples.samples),

    ?assertEqual(ExpectedOperationListing, OperationListing).


-spec test_samples(
    json_utils:json_term(),
    undefined | tokens:serialized(),
    http_client:opts(),
    context(),
    build_sample_test_spec_fun()
) -> ok.
test_samples(#{<<"rest">> := RestApiSamplesJson}, AccessToken, HttpClientOpts, Context, BuildSampleTestSpecFun) ->
    Samples = ?assertMatch(#rest_api_samples{}, jsonable_record:from_json(RestApiSamplesJson, rest_api_samples)),
    ApiRoot = Samples#rest_api_samples.api_root,

    SamplesAndTestSpecs = lists:map(fun(Sample) ->
        {Sample, BuildSampleTestSpecFun(Sample#rest_api_request_sample.name)}
    end, Samples#rest_api_samples.samples),

    PrioritizedSamplesAndTestSpecs = lists:sort(fun({_, SpecA}, {_, SpecB}) ->
        SpecA#sample_test_spec.testing_priority < SpecB#sample_test_spec.testing_priority
    end, SamplesAndTestSpecs),

    lists:foreach(fun({Sample, TestSpec}) ->
        test_sample(Sample, ApiRoot, Context, AccessToken, HttpClientOpts, TestSpec)
    end, PrioritizedSamplesAndTestSpecs).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec test_sample(
    rest_api_request_sample:record(),
    http_client:url(),
    context(),
    undefined | tokens:serialized(),
    http_client:opts(),
    sample_test_spec()
) -> ok.
test_sample(#rest_api_request_sample{
    method = Method,
    path = PathWithPlaceholders,
    headers = Headers,
    data = Data,
    placeholders = Placeholders,
    requires_authorization = RequiresAuthorization
} = Sample, ApiRoot, Context, AccessToken, HttpClientOpts, #sample_test_spec{
    setup_fun = SetupFun,
    substitute_placeholder_fun = SubstitutePlaceholderFun,
    verify_fun = VerifyFun
}) ->
    try
        UpdatedContext = SetupFun(Context),

        AuthHeader = case {RequiresAuthorization, AccessToken} of
            {false, _} -> #{};
            {true, Serialized} when is_binary(Serialized) -> #{<<"x-auth-token">> => Serialized};
            {true, Invalid} -> error({invalid_access_token, Invalid})
        end,

        FullPathWithPlaceholders = <<ApiRoot/binary, PathWithPlaceholders/binary>>,
        FinalPath = maps:fold(fun(Placeholder, _, PathAcc) ->
            Value = SubstitutePlaceholderFun(UpdatedContext, Placeholder),
            binary:replace(PathAcc, Placeholder, http_utils:url_encode(Value), [global])
        end, FullPathWithPlaceholders, Placeholders),

        Body = maps:fold(fun(Placeholder, _, BodyAcc) ->
            Value = SubstitutePlaceholderFun(UpdatedContext, Placeholder),
            binary:replace(BodyAcc, Placeholder, Value, [global])
        end, utils:ensure_defined(Data, <<"">>), Placeholders),

        {ok, Code, _, ResultBody} = ?assertMatch({ok, _, _, _}, http_client:request(
            normalize_method(Method),
            FinalPath,
            maps:merge(AuthHeader, Headers),
            Body,
            [{follow_redirect, true} | HttpClientOpts]
        )),
        case Code > 300 of
            true ->
                ct:pal("Code: ~p~nResponse: ~p~n~nMethod: ~p~nPath: ~p~nBody: ~p~nHeaders: ~p~n",
                    [Code, ResultBody, Method, FinalPath, Body, Headers]),
                error(fail);
            false ->
                ok
        end,
        VerifyFun(UpdatedContext, ResultBody)
    catch Class:Reason:Stacktrace ->
        ct:pal(
            "API sample testing failed!~n"
            "Sample: ~p~n"
            "Context: ~p~n"
            "Error: ~w:~p~n"
            "Stacktrace: ~s~n", [
                Sample,
                Context,
                Class, Reason,
                lager:pr_stacktrace(Stacktrace)
            ]
        ),
        error(fail)
    end.


%% @private
-spec normalize_method('GET' | 'PATCH' | 'POST' | 'PUT' | 'DELETE') -> http_client:method().
normalize_method('GET') -> get;
normalize_method('PATCH') -> patch;
normalize_method('POST') -> post;
normalize_method('PUT') -> put;
normalize_method('DELETE') -> delete.
