%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This behaviour defines API for a DNS query handler.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_query_handler_behaviour).

%%--------------------------------------------------------------------
%% @doc Callbacks below handle specific types od DNS queries, in accordance to RFC1035:
%% {@link https://tools.ietf.org/html/rfc1035#section-3.2.2}
%% The argument in every function is Domain that was queried for, as a lower-case string.
%% On success, the callback must return {ok, List}, where List consists of terms created with functions:
%% dns_server:answer_record/2, dns_server:authority_record/2, dns_server:additional_record/2, dns_server:authoritative_answer_flag/1.
%% Those terms will be put in proper sections of DNS response.
%% See {@link dns.hrl} for reference and data types that should be returned for specific types of queries.
%% @end
%%--------------------------------------------------------------------
-callback handle_a(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_ns(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_cname(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_soa(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_wks(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_ptr(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_hinfo(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_minfo(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_mx(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().

-callback handle_txt(Domain :: string()) ->
    {dns_server:reply_type(), dns_server:dns_query_handler_reponse()} | dns_server:reply_type().



