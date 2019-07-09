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
-type audience_type() :: user | onedata:service().
-type audience_id() :: binary().

% Can be undefined if the auth object is not related to any session
-type session_id() :: undefined | binary().

-type auth() :: #auth{}.

-export_type([subject/0, subject_type/0, subject_id/0]).
-export_type([audience/0, audience_type/0, audience_id/0]).
-export_type([session_id/0]).
-export_type([auth/0]).

%%% API
-export([root_auth/0, nobody_auth/0]).
-export([auth_to_string/1]).
-export([encode_audience_type/1, decode_audience_type/1]).
-export([encode_audience/1]).

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


%%--------------------------------------------------------------------
%% @doc
%% Returns a readable string representing the subject of provided Auth.
%% @end
%%--------------------------------------------------------------------
-spec auth_to_string(auth()) -> string().
auth_to_string(?NOBODY) -> "nobody (unauthenticated client)";
auth_to_string(?ROOT) -> "root";
auth_to_string(?USER(UId)) -> str_utils:format("user:~s", [UId]);
auth_to_string(?PROVIDER(PId)) -> str_utils:format("provider:~s", [PId]).


-spec encode_audience_type(audience_type()) -> <<_:24>>.
encode_audience_type(user) -> <<"usr">>;
encode_audience_type(?OZ_WORKER) -> <<"ozw">>;
encode_audience_type(?OZ_PANEL) -> <<"ozp">>;
encode_audience_type(?OP_WORKER) -> <<"opw">>;
encode_audience_type(?OP_PANEL) -> <<"opp">>;
encode_audience_type(_) -> error(badarg).


-spec decode_audience_type(<<_:24>>) -> audience_type().
decode_audience_type(<<"usr">>) -> user;
decode_audience_type(<<"ozw">>) -> ?OZ_WORKER;
decode_audience_type(<<"ozp">>) -> ?OZ_PANEL;
decode_audience_type(<<"opw">>) -> ?OP_WORKER;
decode_audience_type(<<"opp">>) -> ?OP_PANEL;
decode_audience_type(_) -> error(badarg).


-spec encode_audience(audience()) -> binary().
encode_audience(?AUD(Type, Id)) ->
    <<(encode_audience_type(Type))/binary, "-", Id/binary>>.
