%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module offers common macaroon manipulation API with predefined caveats
%%% recognizable by Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_macaroons).

-include("auth/onedata_macaroons.hrl").
-include("api_errors.hrl").
-include("global_definitions.hrl").
-include("onedata.hrl").
-include("logging.hrl").

-type location() :: Domain :: binary().

-type time_caveat() :: {time, CurrentTimestamp :: non_neg_integer(), MaxTtl :: non_neg_integer() | infinity}.
-type authorization_none_caveat() :: authorization_none.
-type session_id_caveat() :: {session_id_caveat, SessionId :: binary()}.
-type session_id_verifier() :: {session_id_verifier, fun((SessionId :: binary()) -> boolean())}.
-type cluster_type_caveat() :: {cluster_type, onedata:cluster_type()}.
-type service_id_caveat() :: {service_id, ServiceId :: binary()}.
%% @formatter:off
-type caveat() ::          time_caveat() |
                           authorization_none_caveat() |
                           session_id_caveat() |
                           cluster_type_caveat() |
                           service_id_caveat().

-type caveat_verifier() :: time_caveat() |
                           authorization_none_caveat() |
                           session_id_verifier() |
                           cluster_type_caveat() |
                           service_id_caveat().
%% @formatter:on

-export_type([location/0, caveat/0]).

-export([create/4, verify/4, add_caveat/2]).
-export([serialize/1, deserialize/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a macaroon.
%% Available caveats are specified as macros in auth/onedata_macaroons.hrl.
%% @end
%%--------------------------------------------------------------------
-spec create(location(), Secret :: binary(), Identifier :: binary(), [caveat()]) ->
    macaroon:macaroon().
create(Location, Secret, Identifier, Caveats) ->
    Macaroon = macaroon:create(Location, Secret, Identifier),
    lists:foldl(fun(Caveat, MAcc) ->
        add_caveat(MAcc, Caveat)
    end, Macaroon, Caveats).


%%--------------------------------------------------------------------
%% @doc
%% Verifies a macaroon.
%% Available verifiers are specified as macros in auth/onedata_macaroons.hrl.
%% Some verifiers use the same macros as caveats.
%% @end
%%--------------------------------------------------------------------
-spec verify(macaroon:macaroon(), Secret :: binary(),
    DischargeMacaroons :: [macaroon:macaroon()], [caveat_verifier()]) ->
    ok | {error, term()}.
verify(Macaroon, Secret, DischargeMacaroons, CaveatVerifiers) ->
    Verifier = build_verifier(CaveatVerifiers),
    try macaroon_verifier:verify(Verifier, Macaroon, Secret, DischargeMacaroons) of
        ok -> ok;
        _ -> ?ERROR_MACAROON_INVALID
    catch
        throw:{error, _} = Error -> Error;
        _:_ -> ?ERROR_MACAROON_INVALID
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds a caveat to an existing macaroon.
%% Available caveats are specified as macros in auth/onedata_macaroons.hrl.
%% @end
%%--------------------------------------------------------------------
-spec add_caveat(macaroon:macaroon(), caveat()) -> macaroon:macaroon().
add_caveat(Macaroon, Caveat) ->
    macaroon:add_first_party_caveat(Macaroon, caveat_to_binary(Caveat)).


%%--------------------------------------------------------------------
%% @doc
%% Serializes a macaroon from its internal format to portable binary.
%% @end
%%--------------------------------------------------------------------
-spec serialize(Macaroon :: macaroon:macaroon()) ->
    {ok, binary()} | {error, term()}.
serialize(M) ->
    try macaroon:serialize(M) of
        {ok, Token64} -> {ok, base64_to_62(Token64)};
        _ -> ?ERROR_BAD_MACAROON
    catch
        _:_ -> ?ERROR_BAD_MACAROON
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deserializes a macaroon from portable binary to its internal format.
%% @end
%%--------------------------------------------------------------------
-spec deserialize(Macaroon :: binary()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
deserialize(<<>>) -> ?ERROR_BAD_MACAROON;
deserialize(Macaroon) ->
    try macaroon:deserialize(base62_to_64(Macaroon)) of
        {ok, M} -> {ok, M};
        _ -> ?ERROR_BAD_MACAROON
    catch
        _:_ -> ?ERROR_BAD_MACAROON
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec build_verifier([caveat_verifier()]) -> macaroon_verifier:verifier().
build_verifier(CaveatVerifiers) ->
    Verifier = macaroon_verifier:create(),
    lists:foldl(fun(CaveatVerifier, VerifierAcc) ->
        case build_caveat_verifier(CaveatVerifier) of
            Bin when is_binary(Bin) ->
                macaroon_verifier:satisfy_exact(VerifierAcc, Bin);
            Fun when is_function(Fun) ->
                macaroon_verifier:satisfy_general(VerifierAcc, Fun)
        end
    end, Verifier, CaveatVerifiers).


-spec caveat_to_binary(caveat()) -> binary().
caveat_to_binary(?TIME_CAVEAT(Timestamp, MaxTtl)) ->
    Expiration = case MaxTtl of
        ?TIME_INFINITY -> <<"infinity">>;
        _ -> integer_to_binary(Timestamp + MaxTtl)
    end,
    <<"time < ", Expiration/binary>>;

caveat_to_binary(?AUTHORIZATION_NONE_CAVEAT) ->
    <<"authorization = none">>;

caveat_to_binary(?SESSION_ID_CAVEAT(SessionId)) ->
    <<"session_id = ", SessionId/binary>>;

caveat_to_binary(?CLUSTER_TYPE_CAVEAT(ServiceType)) ->
    <<"cluster_type = ", (atom_to_binary(ServiceType, utf8))/binary>>;

caveat_to_binary(?SERVICE_ID_CAVEAT(ServiceId)) ->
    <<"service_id = ", ServiceId/binary>>.


-spec build_caveat_verifier(caveat_verifier()) -> binary() | macaroon_verifier:predicate().
build_caveat_verifier(?TIME_CAVEAT(Timestamp, MaxTtl)) ->
    fun
        (<<"time < infinity">>) ->
            case MaxTtl of
                ?TIME_INFINITY -> true;
                _ -> throw(?ERROR_MACAROON_TTL_TO_LONG(MaxTtl))
            end;
        (<<"time < ", Integer/binary>>) ->
            Expiration = binary_to_integer(Integer),
            case Timestamp < Expiration of
                true ->
                    case Expiration - Timestamp =< MaxTtl of
                        true -> true;
                        false -> throw(?ERROR_MACAROON_TTL_TO_LONG(MaxTtl))
                    end;
                false ->
                    throw(?ERROR_MACAROON_EXPIRED)
            end;
        (_) ->
            false
    end;

build_caveat_verifier(?SESSION_ID_VERIFIER(VerifyFun)) ->
    fun(<<"session_id = ", SessionId/binary>>) ->
        VerifyFun(SessionId)
    end;

% The rest of the caveats are exact - 1:1 with the binary form.
build_caveat_verifier(ExactCaveat) ->
    caveat_to_binary(ExactCaveat).


-spec base64_to_62(binary()) -> binary().
base64_to_62(Token) ->
    <<<<(escape_to_base62(C))/binary>> || <<C>> <= Token>>.


-spec base62_to_64(binary()) -> binary().
base62_to_64(Binary) ->
    base62_to_64(Binary, <<>>).

-spec base62_to_64(binary(), binary()) -> binary().
base62_to_64(<<"0", C:1/binary, Rest/binary>>, Result) ->
    base62_to_64(Rest, <<Result/binary, (unescape_from_base62(C))>>);
base62_to_64(<<C:1/binary, Rest/binary>>, Result) ->
    base62_to_64(Rest, <<Result/binary, C/binary>>);
base62_to_64(<<>>, Result) ->
    Result.


-spec escape_to_base62(char()) -> binary().
escape_to_base62($0) -> <<$0, $0>>;
escape_to_base62($_) -> <<$0, $1>>;
escape_to_base62($-) -> <<$0, $2>>;
escape_to_base62($/) -> <<$0, $3>>;
escape_to_base62($+) -> <<$0, $4>>;
escape_to_base62($=) -> <<$0, $5>>;
escape_to_base62(C) -> <<C>>.


-spec unescape_from_base62(binary()) -> char().
unescape_from_base62(<<$0>>) -> $0;
unescape_from_base62(<<$1>>) -> $_;
unescape_from_base62(<<$2>>) -> $-;
unescape_from_base62(<<$3>>) -> $/;
unescape_from_base62(<<$4>>) -> $+;
unescape_from_base62(<<$5>>) -> $=;
unescape_from_base62(_) -> throw(?ERROR_BAD_MACAROON).

