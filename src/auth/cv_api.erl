%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module includes definitions related to cv_api caveat.
%%% @end
%%%--------------------------------------------------------------------
-module(cv_api).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("errors.hrl").

-type cv_api() :: #cv_api{}.
-type service_pattern() :: all | onedata:service().
-type operation() :: create | get | update | delete.
-type operation_pattern() :: all | operation().
-type matchspec() :: {service_pattern(), operation_pattern(), gri:gri_pattern()}.
-export_type([cv_api/0, operation/0, matchspec/0]).

%% API
-export([verify/4]).
-export([serialize_matchspec/1, deserialize_matchspec/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Verifies given cv_api caveat. Should not be used directly - use the
%% api_caveats module that encapsulates all API related verification logic.
%% @end
%%--------------------------------------------------------------------
-spec verify(cv_api(), onedata:service(), operation(), gri:gri()) ->
    ok | errors:error().
verify(#cv_api{whitelist = Whitelist} = Cv, Service, Operation, GRI) ->
    case is_operation_whitelisted(Whitelist, Service, Operation, GRI) of
        true -> ok;
        false -> ?ERROR_TOKEN_CAVEAT_UNVERIFIED(Cv)
    end.


-spec serialize_matchspec(matchspec()) -> binary().
serialize_matchspec({Service, Operation, GRI}) ->
    <<
        (serialize_service(Service))/binary,
        "/",
        (serialize_operation(Operation))/binary,
        "/",
        (gri:serialize_pattern(GRI))/binary
    >>.


-spec deserialize_matchspec(binary()) -> matchspec().
deserialize_matchspec(Serialized) ->
    [Service, Operation, GRI] = binary:split(Serialized, <<"/">>, [global]),
    {deserialize_service(Service), deserialize_operation(Operation), gri:deserialize_pattern(GRI)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec is_operation_whitelisted([matchspec()], onedata:service(), operation(), gri:gri()) ->
    boolean().
is_operation_whitelisted(Whitelist, Service, Operation, GRI) ->
    lists:any(fun({ServicePattern, OperationPattern, GRIPattern}) ->
        match_service(Service, ServicePattern) andalso
            match_operation(Operation, OperationPattern) andalso
            gri:matches(GRI, GRIPattern)
    end, Whitelist).


%% @private
-spec serialize_service(service_pattern()) -> <<_:24>>.
serialize_service(all) -> <<"all">>;
serialize_service(Service) -> onedata:service_shortname(Service).


%% @private
-spec deserialize_service(<<_:24>>) -> service_pattern().
deserialize_service(<<"all">>) -> all;
deserialize_service(Shortname) -> onedata:service_by_shortname(Shortname).


%% @private
-spec match_service(onedata:service(), service_pattern()) -> boolean().
match_service(_, all) -> true;
match_service(Service, Service) -> true;
match_service(_, _) -> false.


%% @private
-spec serialize_operation(operation_pattern()) -> binary().
serialize_operation(create) -> <<"create">>;
serialize_operation(get) -> <<"get">>;
serialize_operation(update) -> <<"update">>;
serialize_operation(delete) -> <<"delete">>;
serialize_operation(all) -> <<"all">>;
serialize_operation(_) -> error(badarg).


%% @private
-spec deserialize_operation(binary()) -> operation_pattern().
deserialize_operation(<<"create">>) -> create;
deserialize_operation(<<"get">>) -> get;
deserialize_operation(<<"update">>) -> update;
deserialize_operation(<<"delete">>) -> delete;
deserialize_operation(<<"all">>) -> all;
deserialize_operation(_) -> error(badarg).


%% @private
-spec match_operation(operation(), operation_pattern()) -> boolean().
match_operation(_, all) -> true;
match_operation(Operation, Operation) -> true;
match_operation(_, _) -> false.
