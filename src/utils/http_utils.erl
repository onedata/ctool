%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains useful functions loosely connected with HTTP:
%%%     - URL decoding, encoding
%%%     - base64url decoding, encoding
%%%     - JS and HTML escaping
%%% @end
%%%-------------------------------------------------------------------
-module(http_utils).

-define(EMAIL_VALIDATION_REGEXP,
    <<"^[a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-]+(?:\\.[a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-]+)*@(?"
    ":[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?\\.)+[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?$">>
).

% According to RFC 5321
-define(EMAIL_MAX_LENGTH, 254).

% URL encoding/decoding
-export([url_encode/1]).
-export([last_url_part/1]).

% base64url encoding/decoding
-export([base64url_encode/1, base64url_decode/1]).

% Miscellaneous convenience functions
-export([encode_http_parameters/1, append_url_parameters/2]).
-export([validate_email/1, normalize_email/1]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Performs safe URL encoding
%% @end
%%--------------------------------------------------------------------
-spec url_encode(Data :: binary() | string()) -> binary().
url_encode(Data) ->
    hackney_url:urlencode(Data).


%%--------------------------------------------------------------------
%% @doc Splits an URL on '/' and returns the last element.
%% @end
%%--------------------------------------------------------------------
-spec last_url_part(URL :: binary()) -> binary().
last_url_part(URL) ->
    lists:last(binary:split(URL, <<"/">>, [global, trim_all])).


%%--------------------------------------------------------------------
%% @doc Performs URL-safe encoding to base64 (RFC 4648).
%% This differs from base64 in the following ways:
%% '-' is used in place of '+' (62),
%% '_' is used in place of '/' (63),
%% padding is implicit rather than explicit ('=').
%% @end
%%--------------------------------------------------------------------
-spec base64url_encode(binary()) -> binary().
base64url_encode(Data) ->
    mochiweb_base64url:encode(Data).


%%--------------------------------------------------------------------
%% @doc Decodes back from URL-safe base64 (RFC 4648).
%% This differs from base64 in the following ways:
%% '-' is used in place of '+' (62),
%% '_' is used in place of '/' (63),
%% padding is implicit rather than explicit ('=').
%% @end
%%--------------------------------------------------------------------
-spec base64url_decode(binary()) -> binary().
base64url_decode(Data) ->
    mochiweb_base64url:decode(Data).


%%--------------------------------------------------------------------
%% @doc
%% Converts key-value pairs to a single x-www-urlencoded binary. Can be used for
%% GET (url) or POST (request body). Performs url encoding on both keys and values.
%% @end
%%--------------------------------------------------------------------
-spec encode_http_parameters(#{binary() => binary()}) -> binary().
encode_http_parameters(Params) ->
    maps:fold(fun
        (Key, Val, <<"">>) ->
            <<(url_encode(Key))/binary, "=", (url_encode(Val))/binary>>;
        (Key, Val, Acc) ->
            <<Acc/binary, "&", (url_encode(Key))/binary, "=", (url_encode(Val))/binary>>
    end, <<"">>, Params).


%%--------------------------------------------------------------------
%% @doc
%% Appends query string parameters to a URL. If parameters are given as map
%% (Key, Value pairs), they are encoded and converted to a query string.
%% @end
%%--------------------------------------------------------------------
-spec append_url_parameters(binary(), binary() | #{binary() => binary()}) -> binary().
append_url_parameters(Url, Params) when is_map(Params) ->
    append_url_parameters(Url, encode_http_parameters(Params));
append_url_parameters(Url, <<"">>) ->
    Url;
append_url_parameters(Url, Params) ->
    case string:find(Url, <<"?">>) of
        nomatch -> <<Url/binary, "?", Params/binary>>;
        _ -> <<Url/binary, "&", Params/binary>>
    end.



%%--------------------------------------------------------------------
%% @doc
%% Returns true if the given string is a valid email address according to RFC.
%% @end
%%--------------------------------------------------------------------
-spec validate_email(binary()) -> boolean().
validate_email(Email) ->
    case re:run(Email, ?EMAIL_VALIDATION_REGEXP, [{capture, none}]) of
        match -> byte_size(Email) =< ?EMAIL_MAX_LENGTH;
        _ -> false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs gmail email normalization by removing all the dots in the local part.
%% @end
%%--------------------------------------------------------------------
-spec normalize_email(binary()) -> binary().
normalize_email(Email) ->
    case binary:split(Email, <<"@">>, [global]) of
        [Account, <<"gmail.com">>] ->
            NoDots = binary:replace(Account, <<".">>, <<"">>, [global]),
            <<NoDots/binary, "@gmail.com">>;
        _ ->
            Email
    end.