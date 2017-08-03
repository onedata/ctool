%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%%--------------------------------------------------------------------
%%% @doc
%%% XDR integer macros
%%% Based on
%%% http://erlang.org/pipermail/erlang-questions/2000-August/001544.html
%%% @end
%%%-------------------------------------------------------------------


-define(enc_unsigned_int(X),
    list_to_binary([((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
        ((X) bsr 8) band 16#ff, (X) band 16#ff])).

-define(enc_int(X), ?enc_unsigned_int(X)).

-define(enc_unsigned_hyper(X),
    list_to_binary([((X) bsr 56) band 16#ff, ((X) bsr 48) band 16#ff,
        ((X) bsr 40) band 16#ff, ((X) bsr 32) band 16#ff,
        ((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
        ((X) bsr 8) band 16#ff, (X) band 16#ff])).

-define(enc_hyper(X), ?enc_unsigned_hyper(X)).

-define(enc_bool(X),
    if (X) == true -> list_to_binary([0,0,0,1]);
        (X) == false -> list_to_binary([0,0,0,0])
    end).