%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015, ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains declarations of errors used across the project.
%% @end
%% ===================================================================

-ifndef(ERRORS_HRL).
-define(ERRORS_HRL, 1).

%% List of all codes that can be present in status message sent to FUSE client.
-define(OK, 'VOK').
-define(ENOENT, 'VENOENT').
-define(EACCES, 'VEACCES').
-define(EEXIST, 'VEEXIST').
-define(EIO, 'VEIO').
-define(ENOTSUP, 'VENOTSUP').
-define(ENOTEMPTY, 'VENOTEMPTY').
-define(EAGAIN, 'VEAGAIN').
-define(EPERM, 'VEPERM').
-define(EINVAL, 'VEINVAL').
-define(EDQUOT, 'VEDQUOT').
-define(ENOATTR, 'VENOATTR').
-define(ECOMM, 'VECOMM').

-type code() :: ?OK |
                ?ENOENT |
                ?EACCES |
                ?EEXIST |
                ?EIO |
                ?ENOTSUP |
                ?ENOTEMPTY |
                ?EAGAIN |
                ?EPERM |
                ?EINVAL |
                ?EDQUOT |
                ?ENOATTR |
                ?ECOMM.

%% This macro shall return all errors from above.
-define(ERROR_CODES, [
    ?ENOENT, ?EACCES, ?EEXIST, ?EIO, ?ENOTSUP, ?ENOTEMPTY, ?EAGAIN,
    ?EPERM, ?EINVAL, ?EDQUOT, ?ENOATTR, ?ECOMM
]).

-endif.