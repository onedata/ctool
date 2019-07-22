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
-module(macaroons).

-include("aai/macaroons.hrl").
-include("api_errors.hrl").

-type location() :: Domain :: binary().
-type nonce() :: binary().
-type secret() :: binary().

-type time_caveat() :: {time, CurrentTimestamp :: non_neg_integer(), MaxTtl :: non_neg_integer() | infinity}.
-type authorization_none_caveat() :: authorization_none.
-type audience_caveat() :: {audience, aai:audience()}.
%% @formatter:off
-type caveat() ::          time_caveat() |
                           authorization_none_caveat() |
                           audience_caveat().

-type caveat_verifier() :: time_caveat() |
                           authorization_none_caveat() |
                           audience_caveat().
%% @formatter:on

-export_type([location/0, nonce/0, secret/0]).
-export_type([caveat/0, caveat_verifier/0]).

-export([create/4, verify/4, add_caveat/2]).
-export([serialize/1, deserialize/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a macaroon.
%% Available caveats are specified as macros in auth/macaroons.hrl.
%% @end
%%--------------------------------------------------------------------
-spec create(location(), nonce(), secret(), [caveat()]) ->
    macaroon:macaroon().
create(Location, Nonce, Secret, Caveats) ->
    Macaroon = macaroon:create(Location, Secret, Nonce),
    lists:foldl(fun(Caveat, MAcc) ->
        add_caveat(MAcc, Caveat)
    end, Macaroon, Caveats).


%%--------------------------------------------------------------------
%% @doc
%% Verifies a macaroon.
%% Available verifiers are specified as macros in auth/macaroons.hrl.
%% Some verifiers use the same macros as caveats.
%% @end
%%--------------------------------------------------------------------
-spec verify(macaroon:macaroon(), secret(),
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
%% Available caveats are specified as macros in auth/macaroons.hrl.
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
-spec serialize(macaroon:macaroon()) -> {ok, binary()} | {error, term()}.
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
-spec deserialize(binary()) -> {ok, macaroon:macaroon()} | {error, term()}.
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

caveat_to_binary(?AUDIENCE_CAVEAT(Audience)) ->
    <<"audience = ", (aai:encode_audience(Audience))/binary>>.


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

