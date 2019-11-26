%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common definition concerning token caveats.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(CAVEATS_HRL).
-define(CAVEATS_HRL, 1).

-record(cv_time, {
    valid_until = 0 :: time_utils:seconds()
}).

-record(cv_authorization_none, {}).

-record(cv_audience, {
    % Special audience id <<"*">> can be used to match any id, for example
    % #audience{type = ?OP_WORKER, id = <<"*">>} will match any op-worker service
    whitelist = [] :: [aai:audience()]
}).

-record(cv_ip, {
    whitelist = [] :: [ip_utils:mask()]
}).

-record(cv_asn, {
    whitelist = [] :: [ip_utils:asn()]
}).

-record(cv_country, {
    type = whitelist :: whitelist | blacklist,
    list = [] :: [ip_utils:country_code()]
}).

-record(cv_region, {
    type = whitelist :: whitelist | blacklist,
    list = [] :: [ip_utils:region()]
}).

-record(cv_interface, {
    interface :: cv_interface:interface()
}).

-record(cv_api, {
    whitelist = [] :: [cv_api:matchspec()]
}).

-record(cv_data_readonly, {}).

-record(cv_data_path, {
    whitelist = [] :: [data_access_caveats:canonical_path()]
}).

-record(cv_data_objectid, {
    whitelist = [] :: [data_access_caveats:objectid()]
}).

-endif.
