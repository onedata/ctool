%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to automation that are common
%%% for all Onedata services.
%%% @end
%%%-------------------------------------------------------------------
-module(automation).
-author("Lukasz Opiola").

-include("automation/automation.hrl").

% Arbitrary name, relevant for users, given to an automation-related model.
-type name() :: binary().
% A short summary assigned to an automation-related model, in plaintext.
-type summary() :: binary().
% An extended description assigned to an automation-related model, in markdown format.
-type description() :: binary().

% Reference to the operation relevant for a specific atm_lambda_engine_type:type()
%   onedata_function -> function identifier, eg. "calculate_checksum"
%   openfaas        -> docker image, eg. "onedata/openfaas_virus_checker:v19"
%   atm_workflow     -> Id of an atm_workflow to be run, eg. "ee1154e75e664dd888fb8959cf5e245b74b190d6"
%   user_form        -> Id of a user form that should be manually filled by a user to complete the lambda execution.
-type lambda_operation_ref() :: binary().

-type store_type() :: single_value | list | map | forest | range | histogram.

-export_type([name/0, summary/0, description/0]).
-export_type([lambda_operation_ref/0]).
-export_type([store_type/0]).
