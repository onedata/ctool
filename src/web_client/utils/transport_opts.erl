%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Util functions for modification of ssl options.
%%% @end
%%%--------------------------------------------------------------------
-module(transport_opts).
-author("Tomasz Lichon").

%% API
-export([expand_hackney_verify_cacert_opts/1, expand_ssl_verify_cacert_opts/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Expands ssl options for peer verification, assuming that if no 'insecure'
%% option is given, the client requests full ca chain verification using provided
%% cacerts together with system ca bundle.
%% @end
%%--------------------------------------------------------------------
-spec expand_hackney_verify_cacert_opts(Opts :: http_client:hackney_opts()) ->
    NewOpts :: http_client:hackney_opts().
expand_hackney_verify_cacert_opts(Opts) ->
    case lists:member(insecure, Opts) of
        true ->
            Opts;
        false ->
            SslOpts = proplists:get_value(ssl_options, Opts, []),
            OptsWithoutSsl = proplists:delete(ssl_options, Opts),
            ExpandedSslOpts = expand_ssl_verify_cacert_opts(SslOpts),
            [{ssl_options, ExpandedSslOpts} | OptsWithoutSsl]
    end.

%%--------------------------------------------------------------------
%% @doc
%% Expands ssl options adding verify_peer and extending given cacerts
%% with system bundle
%% @end
%%--------------------------------------------------------------------
-spec expand_ssl_verify_cacert_opts([ssl:tls_option()]) -> [ssl:tls_option()].
expand_ssl_verify_cacert_opts(SslOpts) ->
    Verify = proplists:get_value(verify, SslOpts),
    Cacerts = proplists:get_value(cacerts, SslOpts, []),
    CacertsWithBundle = certifi:cacerts() ++ Cacerts,
    SslOptsWithoutCacerts = proplists:delete(cacerts, SslOpts),
    SslOptsWithoutCacertsAndVerify = proplists:delete(verify, SslOptsWithoutCacerts),

    case Verify of
        undefined ->
            [{depth, 99}, {verify, verify_peer}, {cacerts, CacertsWithBundle} | SslOptsWithoutCacertsAndVerify];
        Verify ->
            [{depth, 99}, {verify, Verify}, {cacerts, CacertsWithBundle} | SslOptsWithoutCacertsAndVerify]
    end.
