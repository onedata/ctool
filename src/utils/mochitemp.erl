%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @doc Create temporary files and directories. Requires crypto to be started.

-module(mochitemp).
-export([gettempdir/0]).
-export([mkdtemp/0, mkdtemp/3]).
-export([rmtempdir/1]).

-define(SAFE_CHARS, {$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
                     $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
                     $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
                     $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
                     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $_}).
-define(TMP_MAX, 10000).

-include_lib("kernel/include/file.hrl").

rmtempdir(Dir) ->
    case file:del_dir(Dir) of
        {error, eexist} ->
            ok = rmtempdirfiles(Dir),
            ok = file:del_dir(Dir);
        ok ->
            ok
    end.

rmtempdirfiles(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    ok = rmtempdirfiles(Dir, Files).

rmtempdirfiles(_Dir, []) ->
    ok;
rmtempdirfiles(Dir, [Basename | Rest]) ->
    Path = filename:join([Dir, Basename]),
    case filelib:is_dir(Path) of
        true ->
            ok = rmtempdir(Path);
        false ->
            ok = file:delete(Path)
    end,
    rmtempdirfiles(Dir, Rest).

mkdtemp() ->
    mkdtemp("", "tmp", gettempdir()).

mkdtemp(Suffix, Prefix, Dir) ->
    mkdtemp_n(rngpath_fun(Suffix, Prefix, Dir), ?TMP_MAX).



mkdtemp_n(RngPath, 1) ->
    make_dir(RngPath());
mkdtemp_n(RngPath, N) ->
    try make_dir(RngPath())
    catch throw:{error, eexist} ->
            mkdtemp_n(RngPath, N - 1)
    end.

make_dir(Path) ->
    case file:make_dir(Path) of
        ok ->
            ok;
        E={error, eexist} ->
            throw(E)
    end,
    %% Small window for a race condition here because dir is created 777
    ok = file:write_file_info(Path, #file_info{mode=8#0700}),
    Path.

rngpath_fun(Prefix, Suffix, Dir) ->
    fun () ->
            filename:join([Dir, Prefix ++ rngchars(6) ++ Suffix])
    end.

rngchars(0) ->
    "";
rngchars(N) ->
    [rngchar() | rngchars(N - 1)].

rngchar() ->
    rngchar(rand_uniform(0, tuple_size(?SAFE_CHARS))).

rngchar(C) ->
    element(1 + C, ?SAFE_CHARS).

-ifdef(rand_mod_unavailable).
rand_uniform(Start, End) ->
            crypto:rand_uniform(Start, End).
-else.
rand_uniform(Start, End) ->
            Start + rand:uniform(End - Start) - 1.
-endif.

%% @spec gettempdir() -> string()
%% @doc Get a usable temporary directory using the first of these that is a directory:
%%      $TMPDIR, $TMP, $TEMP, "/tmp", "/var/tmp", "/usr/tmp", ".".
gettempdir() ->
    gettempdir(gettempdir_checks(), fun normalize_dir/1).

gettempdir_checks() ->
    [{fun os:getenv/1, ["TMPDIR", "TMP", "TEMP"]},
     {fun gettempdir_identity/1, ["/tmp", "/var/tmp", "/usr/tmp"]},
     {fun gettempdir_cwd/1, [cwd]}].

gettempdir_identity(L) ->
    L.

gettempdir_cwd(cwd) ->
    {ok, L} = file:get_cwd(),
    L.

gettempdir([{_F, []} | RestF], Normalize) ->
    gettempdir(RestF, Normalize);
gettempdir([{F, [L | RestL]} | RestF], Normalize) ->
    case Normalize(F(L)) of
        false ->
            gettempdir([{F, RestL} | RestF], Normalize);
        Dir ->
            Dir
    end.

normalize_dir(False) when False =:= false orelse False =:= "" ->
    %% Erlang doesn't have an unsetenv, wtf.
    false;
normalize_dir(L) ->
    Dir = filename:absname(L),
    case filelib:is_dir(Dir) of
        false ->
            false;
        true ->
            Dir
    end.
