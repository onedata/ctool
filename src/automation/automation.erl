%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% fixme
%%% @end
%%%-------------------------------------------------------------------
-module(automation).
-author("Lukasz Opiola").

% Arbitrary name, relevant for users, given to an automation inventory or lambda.
-type name() :: binary().

% A short summary of what a lambda function does, in plaintext.
-type lambda_summary() :: binary().
% A short summary of what a lambda function does, in plaintext.
-type lambda_summary() :: binary().

-export_type([name/0, lambda_summary/0]).

%%% API
-export([root_auth/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec root_auth() -> auth().
root_auth() ->
    ?ROOT.

