%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines od_error record interface - implemented for onedata 
%%% errors that are to be returned via API calls. Each such error should have a
%%% dedicated module implementing the callbacks.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error).

-include("errors.hrl").


-type http_code() :: 400 | 401 | 403 | 404 | 409 | 500 | 501 | 503.

-type errno() :: 
    ?OK | ?E2BIG | ?EACCES | ?EADDRINUSE | ?EADDRNOTAVAIL |
    ?EAFNOSUPPORT | ?EAGAIN | ?EALREADY | ?EBADF | ?EBADMSG | ?EBUSY |
    ?ECANCELED | ?ECHILD | ?ECONNABORTED | ?ECONNREFUSED | ?ECONNRESET |
    ?EDEADLK | ?EDESTADDRREQ | ?EDOM | ?EEXIST | ?EFAULT | ?EFBIG |
    ?EHOSTUNREACH | ?EIDRM | ?EILSEQ | ?EINPROGRESS | ?EINTR | ?EINVAL | ?EIO |
    ?EISCONN | ?EISDIR | ?EKEYEXPIRED | ?ELOOP | ?EMFILE | ?EMLINK | ?EMSGSIZE |
    ?ENAMETOOLONG | ?ENETDOWN | ?ENETRESET | ?ENETUNREACH | ?ENFILE | ?ENOBUFS |
    ?ENODATA | ?ENODEV | ?ENOENT | ?ENOEXEC | ?ENOLCK | ?ENOLINK | ?ENOMEM |
    ?ENOMSG | ?ENOPROTOOPT | ?ENOSPC | ?ENOSR | ?ENOSTR | ?ENOSYS | ?ENOTCONN |
    ?ENOTDIR | ?ENOTEMPTY | ?ENOTRECOVERABLE | ?ENOTSOCK | ?ENOTSUP | ?ENOTTY |
    ?ENXIO | ?EOPNOTSUPP | ?EOVERFLOW | ?EOWNERDEAD | ?EPERM | ?EPIPE |
    ?EPROTO | ?EPROTONOSUPPORT | ?EPROTOTYPE | ?ERANGE | ?EROFS | ?ESPIPE |
    ?ESRCH | ?ETIME | ?ETIMEDOUT | ?ETXTBSY | ?EWOULDBLOCK | ?EXDEV.

-type t() :: #od_error{}.

-export_type([http_code/0, errno/0, t/0]).


%%%===================================================================
%%% od_error behaviour definition
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Encodes an error JSON object.
%% @end
%%--------------------------------------------------------------------
-callback to_json(t()) -> json_utils:json_map().


%%--------------------------------------------------------------------
%% @doc
%% Decodes an error from a JSON object.
%% @end
%%--------------------------------------------------------------------
-callback from_json(json_utils:json_map()) -> t().


%%--------------------------------------------------------------------
%% @doc
%% Returns HTTP code to be returned in REST response.
%% @end
%%--------------------------------------------------------------------
-callback to_http_code(t()) -> http_code().
