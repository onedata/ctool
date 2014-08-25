%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Global Registry definition of OpenID tokens.
%% @end
%% ===================================================================

-ifndef(GR_OPENID_HRL).
-define(GR_OPENID_HRL, 1).

%% @doc client_token record contains following fields:
%% * access_id   - unique access ID authorized by client
%% * client_name - client name associated with token
%% @end
-record(client_token, {
    access_id :: binary(),
    client_name :: binary()
}).

%% @doc id_token record contains following fields:
%% * iss   - Issuer Identifier for the Issuer of the response
%% * sub   - Subject Identifier
%% * aud   - Audience(s) that this ID Token is intended for
%% * name  - End-User's full name in displayable form including all name parts
%% * email - list of End-User's emails
%% * exp   - Expiration time on or after which the ID Token MUST NOT be accepted for processing
%% * iat   - Time at which the JWT was issued
%% For more details see: <a href="http://openid.net/specs/openid-connect-core-1_0.html#IDToken">OpenID IDToken</a>
%% @end
-record(id_token, {
    iss :: binary(),
    sub :: binary(),
    aud :: binary(),
    name :: binary(),
    email :: [binary()],
    exp :: binary(),
    iat :: binary()
}).

%% @doc grant_token record contains following fields:
%% * access_token  - the access token issued by the authorization server
%% * token_type    - the type of the token issued
%% * expires_in    - the lifetime in seconds of the access token
%% * refresh_token - the token, which can be used to obtain new access tokens using the same authorization grant
%% * scope         - the scope of the access token
%% * id_token      - ID Token value associated with the authenticated session
%% For more details see: <a href="http://tools.ietf.org/html/rfc6749#section-4.1.4">Access Token Response</a>
%% @end
-record(grant_token, {
    access_token :: binary(),
    token_type :: binary(),
    expires_in :: integer(),
    refresh_token :: binary(),
    scope :: binary(),
    id_token :: #id_token{}
}).

-endif.