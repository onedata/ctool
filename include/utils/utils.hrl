%%%-------------------------------------------------------------------
%%% @author Michal Wrona
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module stores utility definitions for use in other modules.
%%% @end
%%%-------------------------------------------------------------------
-author("Michal Wrona").

%%--------------------------------------------------------------------
%% @doc
%% Converts record to list in format [{field, value}].
%% @end
%%--------------------------------------------------------------------
-define(record_to_list(Record, RecordTuple),
    Keys = record_info(fields, Record),
    lists:zip(Keys, tl(tuple_to_list(RecordTuple)))
).