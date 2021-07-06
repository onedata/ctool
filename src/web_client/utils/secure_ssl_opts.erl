%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Util functions for setting ssl options based on security requirements.
%%% @end
%%%--------------------------------------------------------------------
-module(secure_ssl_opts).
-author("Lukasz Opiola").

%% API
-export([expand/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Expands ssl options for peer verification based on desired level of security:
%%      {secure, true} will verify peercert and hostname
%%      {secure, only_verify_peercert} will only verify peercert
%%      {secure, false} will NOT PERFORM ANY validation.
%% The force_insecure_connections env variable can be used to override whatever
%% is passed as secure flag and skip any verification.
%% @end
%%--------------------------------------------------------------------
-spec expand([http_client:ssl_opt()]) -> [http_client:ssl_opt()].
expand(SslOpts) ->
    %% @TODO VFS-7795 it would be better to have 'default_http_client_security' option that
    %% would be taken if no secure flag is given
    ForceInsecure = ctool:get_env(force_insecure_connections, false),
    SecureFlag = proplists:get_value(secure, SslOpts, true),
    NewOpts = case ForceInsecure orelse SecureFlag =:= false of
        true ->
            [{verify, verify_none}];
        false ->
            CustomCaCerts = proplists:get_value(cacerts, SslOpts, []),
            CaCerts = CustomCaCerts ++ certifi:cacerts(),

            VerifyFunOpt = case SecureFlag of
                true ->
                    [];
                only_verify_peercert ->
                    [{verify_fun, {fun peercert_only_verify_fun/3, []}}]
            end,

            ServerNameIndicationOpt = case proplists:get_value(hostname, SslOpts) of
                undefined ->
                    [];
                HostnameBin ->
                    [{server_name_indication, string:strip(binary_to_list(HostnameBin), right, $.)}]
            end,

            VerifyFunOpt ++ ServerNameIndicationOpt ++ [
                {verify, verify_peer},
                {cacerts, CaCerts},
                {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
            ]
    end,
    NewOpts ++ proplists:delete(cacerts,
        proplists:delete(secure,
            proplists:delete(hostname, SslOpts))).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec peercert_only_verify_fun(term(), term(), term()) ->
    {fail | unknown | valid, term()}.
peercert_only_verify_fun(_, {bad_cert, hostname_check_failed}, UserState) ->
    {valid, UserState};
peercert_only_verify_fun(_, {bad_cert, _} = Reason, _) ->
    {fail, Reason};
peercert_only_verify_fun(_, {extension, _}, UserState) ->
    {unknown, UserState};
peercert_only_verify_fun(_, valid, UserState) ->
    {valid, UserState};
peercert_only_verify_fun(_Cert, valid_peer, UserState) ->
    {valid, UserState}.
