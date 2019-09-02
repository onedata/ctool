%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module includes some common functions and types related to AAI in Onedata.
%%% AAI stands for Authentication and Authorization Infrastructure.
%%% AAI in Onedata assumes the approach of IBAC (Identity Based Access Control).
%%% This means that authorization to perform certain operations is based on
%%% the client's identity (represented by #subject{} in Onedata) and other
%%% attributes of the resources (privileges, existing relations).
%%%
%%% Onedata uses the concept of audience (#audience{}) to confine authorization
%%% to a specific requesting party (e.g. a specific op_worker service). In such
%%% case, the requesting party must prove its identity during the request by
%%% presenting a proper access token (aka audience token). In REST, it is done
%%% using the following headers:
%%%     X-Auth-Token               <- subject's access token
%%%                                   (Authorization: Bearer $TOKEN) is also supported
%%%     X-Onedata-Audience-Token   <- requesting party's access token (aka audience token)
%%% In GraphSync, the audience is inferred from the owner of GraphSync session.
%%%
%%% Tokens in Onedata have the following characteristics:
%%%     * each token carries the subject's identity
%%%     * delegated tokens allow to perform operation on behalf of the subject (token issuer)
%%%     * tokens can have contextual confinements which all must be satisfied during verification
%%%     * tokens are implemented using the macaroons library
%%%
%%% Tokens and audience tokens (which are essentially a tuple: {AudienceType, Token})
%%% are represented by #auth_token{} and #audience_token{} records.
%%% @end
%%%-------------------------------------------------------------------
-module(aai).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("onedata.hrl").

-type subject() :: #subject{}.
% root is allowed to do anything, it must be used with caution
% (should not be used in any kind of external API)
-type subject_type() :: nobody | root | user | ?ONEPROVIDER.
% Applicable in case of user or ?ONEPROVIDER type
-type subject_id() :: undefined | binary().

-type audience() :: #audience{}.
-type audience_type() :: user | group | onedata:service().
-type audience_id() :: binary().

% Can be undefined if the auth object is not related to any session
-type session_id() :: undefined | binary().

-type auth() :: #auth{}.
-type auth_ctx() :: #auth_ctx{}.

-export_type([subject/0, subject_type/0, subject_id/0]).
-export_type([audience/0, audience_type/0, audience_id/0]).
-export_type([session_id/0]).
-export_type([auth/0, auth_ctx/0]).

%%% API
-export([root_auth/0, nobody_auth/0]).
-export([subject_to_json/1, json_to_subject/1]).
-export([serialize_audience_type/1, deserialize_audience_type/1]).
-export([serialize_audience/1, deserialize_audience/1]).
-export([auth_to_printable/1]).
-export([audience_to_printable/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the Auth object representing ROOT authorization
%% (allowed to perform all operations).
%% @end
%%--------------------------------------------------------------------
-spec root_auth() -> auth().
root_auth() ->
    ?ROOT.


%%--------------------------------------------------------------------
%% @doc
%% Returns the Auth object representing NOBODY authorization
%% (allowed only to perform publicly available operations).
%% @end
%%--------------------------------------------------------------------
-spec nobody_auth() -> auth().
nobody_auth() ->
    ?NOBODY.


-spec subject_to_json(aai:subject()) -> json_utils:json_term().
subject_to_json(?SUB(nobody)) -> <<"nobody">>;
subject_to_json(?SUB(user, UserId)) -> #{<<"user">> => UserId};
subject_to_json(?SUB(?ONEPROVIDER, PrId)) -> #{<<"provider">> => PrId}.


-spec json_to_subject(json_utils:json_term()) -> aai:subject().
json_to_subject(<<"nobody">>) -> ?SUB(nobody);
json_to_subject(#{<<"user">> := UserId}) -> ?SUB(user, UserId);
json_to_subject(#{<<"provider">> := PrId}) -> ?SUB(?ONEPROVIDER, PrId).


-spec serialize_audience(audience()) -> binary().
serialize_audience(?AUD(Type, Id)) ->
    <<(serialize_audience_type(Type))/binary, "-", Id/binary>>.


-spec deserialize_audience(binary()) -> audience().
deserialize_audience(<<Type:3/binary, "-", Id/binary>>) ->
    ?AUD(deserialize_audience_type(Type), Id);
deserialize_audience(_) ->
    error(badarg).


-spec serialize_audience_type(audience_type()) -> <<_:24>>.
serialize_audience_type(user) -> <<"usr">>;
serialize_audience_type(group) -> <<"grp">>;
serialize_audience_type(Service) -> onedata:service_shortname(Service).


-spec deserialize_audience_type(<<_:24>>) -> audience_type().
deserialize_audience_type(<<"usr">>) -> user;
deserialize_audience_type(<<"grp">>) -> group;
deserialize_audience_type(Shortname) -> onedata:service_by_shortname(Shortname).


-spec auth_to_printable(auth()) -> string().
auth_to_printable(?NOBODY) -> "nobody (unauthenticated client)";
auth_to_printable(?ROOT) -> "root";
auth_to_printable(?USER(UId)) -> str_utils:format("user:~s", [UId]);
auth_to_printable(?PROVIDER(PId)) -> str_utils:format("provider:~s", [PId]).


-spec audience_to_printable(audience()) -> string().
audience_to_printable(?AUD(Type, Id)) -> str_utils:format("~s:~s", [Type, Id]).
