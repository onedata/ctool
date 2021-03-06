%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%% @doc
%% This header file contains macros for POSIX errors.
%% @end
%%%-------------------------------------------------------------------

-ifndef(ERRNO_HRL).
-define(ERRNO_HRL, 1).

%% List of all codes that can be present in status message sent to FUSE client.
-define(OK, ok).
-define(E2BIG, e2big).
-define(EACCES, eacces).
-define(EADDRINUSE, eaddrinuse).
-define(EADDRNOTAVAIL, eaddrnotavail).
-define(EAFNOSUPPORT, eafnosupport).
-define(EAGAIN, eagain).
-define(EALREADY, ealready).
-define(EBADF, ebadf).
-define(EBADMSG, ebadmsg).
-define(EBUSY, ebusy).
-define(ECANCELED, ecanceled).
-define(ECHILD, echild).
-define(ECONNABORTED, econnaborted).
-define(ECONNREFUSED, econnrefused).
-define(ECONNRESET, econnreset).
-define(EDEADLK, edeadlk).
-define(EDESTADDRREQ, edestaddrreq).
-define(EDOM, edom).
-define(EEXIST, eexist).
-define(EFAULT, efault).
-define(EFBIG, efbig).
-define(EHOSTUNREACH, ehostunreach).
-define(EIDRM, eidrm).
-define(EILSEQ, eilseq).
-define(EINPROGRESS, einprogress).
-define(EINTR, eintr).
-define(EINVAL, einval).
-define(EIO, eio).
-define(EISCONN, eisconn).
-define(EISDIR, eisdir).
-define(EKEYEXPIRED, ekeyexpired).
-define(ELOOP, eloop).
-define(EMFILE, emfile).
-define(EMLINK, emlink).
-define(EMSGSIZE, emsgsize).
-define(ENAMETOOLONG, enametoolong).
-define(ENETDOWN, enetdown).
-define(ENETRESET, enetreset).
-define(ENETUNREACH, enetunreach).
-define(ENFILE, enfile).
-define(ENOATTR, ?ENODATA).
-define(ENOBUFS, enobufs).
-define(ENODATA, enodata).
-define(ENODEV, enodev).
-define(ENOENT, enoent).
-define(ENOEXEC, enoexec).
-define(ENOLCK, enolck).
-define(ENOLINK, enolink).
-define(ENOMEM, enomem).
-define(ENOMSG, enomsg).
-define(ENOPROTOOPT, enoprotoopt).
-define(ENOSPC, enospc).
-define(ENOSR, enosr).
-define(ENOSTR, enostr).
-define(ENOSYS, enosys).
-define(ENOTCONN, enotconn).
-define(ENOTDIR, enotdir).
-define(ENOTEMPTY, enotempty).
-define(ENOTRECOVERABLE, enotrecoverable).
-define(ENOTSOCK, enotsock).
-define(ENOTSUP, enotsup).
-define(ENOTTY, enotty).
-define(ENXIO, enxio).
-define(EOPNOTSUPP, eopnotsupp).
-define(EOVERFLOW, eoverflow).
-define(EOWNERDEAD, eownerdead).
-define(EPERM, eperm).
-define(EPIPE, epipe).
-define(EPROTO, eproto).
-define(EPROTONOSUPPORT, eprotonosupport).
-define(EPROTOTYPE, eprototype).
-define(ERANGE, erange).
-define(EROFS, erofs).
-define(ESPIPE, espipe).
-define(ESRCH, esrch).
-define(ETIME, etime).
-define(ETIMEDOUT, etimedout).
-define(ETXTBSY, etxtbsy).
-define(EWOULDBLOCK, ewouldblock).
-define(EXDEV, exdev).

-type code() :: ?OK | ?E2BIG | ?EACCES | ?EADDRINUSE | ?EADDRNOTAVAIL
    | ?EAFNOSUPPORT | ?EAGAIN | ?EALREADY | ?EBADF | ?EBADMSG | ?EBUSY
    | ?ECANCELED | ?ECHILD | ?ECONNABORTED | ?ECONNREFUSED | ?ECONNRESET
    | ?EDEADLK | ?EDESTADDRREQ | ?EDOM | ?EEXIST | ?EFAULT | ?EFBIG
    | ?EHOSTUNREACH | ?EIDRM | ?EILSEQ | ?EINPROGRESS | ?EINTR | ?EINVAL | ?EIO
    | ?EISCONN | ?EISDIR | ?EKEYEXPIRED | ?ELOOP | ?EMFILE | ?EMLINK | ?EMSGSIZE
    | ?ENAMETOOLONG | ?ENETDOWN | ?ENETRESET | ?ENETUNREACH | ?ENFILE | ?ENOBUFS
    | ?ENODATA | ?ENODEV | ?ENOENT | ?ENOEXEC | ?ENOLCK | ?ENOLINK | ?ENOMEM
    | ?ENOMSG | ?ENOPROTOOPT | ?ENOSPC | ?ENOSR | ?ENOSTR | ?ENOSYS | ?ENOTCONN
    | ?ENOTDIR | ?ENOTEMPTY | ?ENOTRECOVERABLE | ?ENOTSOCK | ?ENOTSUP | ?ENOTTY
    | ?ENXIO | ?EOPNOTSUPP | ?EOVERFLOW | ?EOWNERDEAD | ?EPERM | ?EPIPE
    | ?EPROTO | ?EPROTONOSUPPORT | ?EPROTOTYPE | ?ERANGE | ?EROFS | ?ESPIPE
    | ?ESRCH | ?ETIME | ?ETIMEDOUT | ?ETXTBSY | ?EWOULDBLOCK | ?EXDEV.

%% This macro shall return all errors from above.
-define(ERROR_CODES, [
    ?E2BIG, ?EACCES, ?EADDRINUSE, ?EADDRNOTAVAIL, ?EAFNOSUPPORT, ?EAGAIN,
    ?EALREADY, ?EBADF, ?EBADMSG, ?EBUSY, ?ECANCELED, ?ECHILD, ?ECONNABORTED,
    ?ECONNREFUSED, ?ECONNRESET, ?EDEADLK, ?EDESTADDRREQ, ?EDOM, ?EEXIST,
    ?EFAULT, ?EFBIG, ?EHOSTUNREACH, ?EIDRM, ?EILSEQ, ?EINPROGRESS, ?EINTR,
    ?EINVAL, ?EIO, ?EISCONN, ?EISDIR, ?EKEYEXPIRED, ?ELOOP, ?EMFILE, ?EMLINK, ?EMSGSIZE,
    ?ENAMETOOLONG, ?ENETDOWN, ?ENETRESET, ?ENETUNREACH, ?ENFILE, ?ENOBUFS,
    ?ENODATA, ?ENODEV, ?ENOENT, ?ENOEXEC, ?ENOLCK, ?ENOLINK, ?ENOMEM, ?ENOMSG,
    ?ENOPROTOOPT, ?ENOSPC, ?ENOSR, ?ENOSTR, ?ENOSYS, ?ENOTCONN, ?ENOTDIR,
    ?ENOTEMPTY, ?ENOTRECOVERABLE, ?ENOTSOCK, ?ENOTSUP, ?ENOTTY, ?ENXIO,
    ?EOPNOTSUPP, ?EOVERFLOW, ?EOWNERDEAD, ?EPERM, ?EPIPE, ?EPROTO,
    ?EPROTONOSUPPORT, ?EPROTOTYPE, ?ERANGE, ?EROFS, ?ESPIPE, ?ESRCH, ?ETIME,
    ?ETIMEDOUT, ?ETXTBSY, ?EWOULDBLOCK, ?EXDEV
]).

-endif.
