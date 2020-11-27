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

-define(ID_WILDCARD, <<"*">>).
% Special wildcard id <<"*">> can be used to match any id in a service or
% consumer caveat, for example #service_spec{type = ?OP_WORKER, id = <<"*">>} will
% match any op-worker service and #subject{type = user, id = <<"*">>} will match
% any user.

-record(cv_time, {
    valid_until = 0 :: time:seconds()
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

%% @todo VFS-6098 deprecated, kept for backward compatibility
-record(cv_scope, {
    scope = identity_token :: identity_token
}).

-record(cv_service, {
    whitelist = [] :: [aai:service_spec()]
}).

-record(cv_consumer, {
    whitelist = [] :: [aai:consumer_spec()]
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
