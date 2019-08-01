%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Dummy module that impersonates the hamcrest lib to avoid dialyzer's
%%% unknown type errors (hamcrest:matchspec/0 is referenced in meck).
%%% @end
%%%--------------------------------------------------------------------
-module(hamcrest).
-author("Lukasz Opiola").

-type matchspec() :: term().
-export_type([matchspec/0]).