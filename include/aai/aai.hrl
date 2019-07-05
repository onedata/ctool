%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains caveat macros used in macaroon building and verifying.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(AAI_HRL).
-define(AAI_HRL, 1).

-include("../onedata.hrl").
-include("macaroons.hrl").

-record(subject, {
    % root is allowed to do anything, it must be used with caution
    % (should not be used in any kind of external API!)
    type = nobody :: aai:subject_type(),
    id = undefined :: aai:subject_id()
}).

-record(audience, {
    type = user :: aai:audience_type(),
    id :: aai:audience_id()
}).

-record(auth, {
    subject = #subject{} :: aai:subject(),
    audience = undefined :: undefined | aai:audience(),
    % Can be undefined if the auth object is not related to any session
    session_id = undefined :: aai:session_id()
}).


% Current (newest) version of tokens used in Onedata
-define(CURRENT_TOKEN_VERSION, 2).

% @todo VFS-5524 to be extended
-define(ACCESS_TOKEN, access_token).
-define(GUI_TOKEN(SessionId), {gui_token, SessionId}).

% @todo VFS-5524 rename to #token{} after refactoring Onezone invite tokens
-record(auth_token, {
    version = ?CURRENT_TOKEN_VERSION :: tokens:version(),
    onezone_domain :: tokens:onezone_domain(),
    nonce :: tokens:nonce(),
    persistent :: boolean(),
    subject = #subject{} :: aai:subject(),
    type = ?ACCESS_TOKEN :: tokens:type(),
    macaroon = undefined :: undefined | macaroon:macaroon()
}).

-record(audience_token, {
    audience_type :: aai:audience_type(),
    token :: tokens:token()
}).

% Convenience macros for concise code
-define(SUB(Type), #subject{type = Type}).
-define(SUB(Type, Id), #subject{type = Type, id = Id}).

-define(AUD(Type, Id), #audience{type = Type, id = Id}).

-define(NOBODY, #auth{subject = #subject{type = nobody}}).
-define(ROOT, #auth{subject = #subject{type = root}}).
-define(USER, #auth{subject = #subject{type = user}}).
-define(USER(Id), #auth{subject = #subject{type = user, id = Id}}).
-define(PROVIDER, #auth{subject = #subject{type = ?ONEPROVIDER}}).
-define(PROVIDER(Id), #auth{subject = #subject{type = ?ONEPROVIDER, id = Id}}).

-endif.
