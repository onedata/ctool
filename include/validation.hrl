%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common macros used for validating input data.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(VALIDATION_HRL).
-define(VALIDATION_HRL, 1).

% Regexp to validate domain (domain, subdomain or IP)
% Domain consists of some number of parts delimited by single dot characters.
% Each part must start and end with an lowercase alphanum
% and may contain a hyphen '-'.
-define(DOMAIN_VALIDATION_REGEXP,
    <<"^(([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])\\.)*([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])$">>).

-define(MAX_DOMAIN_LENGTH, 253).

-define(SUBDOMAIN_VALIDATION_REGEXP,
    <<"^([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])$">>).


-define(NAME_REQUIREMENTS_DESCRIPTION, <<
    "Name must be 2-128 characters long and composed only of UTF-8 letters, digits, brackets and underscores. "
    "Dashes, spaces and dots are allowed (but not at the beginning or the end)."
>>).
-define(NAME_FIRST_CHARS_ALLOWED, <<")(\\w_">>).
-define(NAME_MIDDLE_CHARS_ALLOWED, <<")(\\w_ .-">>).
-define(NAME_LAST_CHARS_ALLOWED, ?NAME_FIRST_CHARS_ALLOWED).
-define(NAME_MAXIMUM_LENGTH, 128).


-define(FULL_NAME_REQUIREMENTS_DESCRIPTION, <<
    "Full name must be 2-128 characters long and composed only of UTF-8 letters and digits. "
    "Dashes, spaces, dots, commas and apostrophes are allowed (but not at the beginning or the end)."
>>).
-define(FULL_NAME_FIRST_CHARS_ALLOWED, <<"\\pL\\pNd">>).
-define(FULL_NAME_MIDDLE_CHARS_ALLOWED, <<"\\pL\\pNd ',.-">>).
-define(FULL_NAME_LAST_CHARS_ALLOWED, <<"\\pL\\pNd.">>).
-define(FULL_NAME_MAXIMUM_LENGTH, 128).
-define(DEFAULT_FULL_NAME, <<"Unnamed User">>).


-define(USERNAME_REQUIREMENTS_DESCRIPTION, <<
    "Username must be 2-32 characters long and composed only of letters and digits. "
    "Dashes and underscores are allowed (but not at the beginning or the end)."
>>).
-define(USERNAME_FIRST_CHARS_ALLOWED, <<"a-z0-9A-Z">>).
-define(USERNAME_MIDDLE_CHARS_ALLOWED, <<"a-z0-9A-Z._-">>).
-define(USERNAME_LAST_CHARS_ALLOWED, ?USERNAME_FIRST_CHARS_ALLOWED).
-define(USERNAME_MAXIMUM_LENGTH, 32).


-define(PASSWORD_REQUIREMENTS_DESCRIPTION, <<
    "Password must be at least 8 characters long."
>>).
-define(PASSWORD_MIN_LENGTH, 8).

-endif.
