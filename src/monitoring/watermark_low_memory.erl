%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Module responsible for calculating watermark low memory
%%% @end
%%%--------------------------------------------------------------------
-module(watermark_low_memory).
-author("Tomasz Lichon").

-define(ZONE_INFO_FILE, "/proc/zoneinfo").
-define(BUFFER_SIZE, 10485760). %10MB
%% API
-export([calculate/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Return size of memory with low watermark.
%% MemAvailable = MemFree - WatermarkLow
%% @end
%%--------------------------------------------------------------------
-spec calculate() -> KB :: integer().
calculate() ->
    Zoneinfo = get_zoneinfo(),
    count_watermark_low_pages(Zoneinfo) * get_page_size().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Count watermark low pages from zoneinfo data.
%% @end
%%--------------------------------------------------------------------
-spec count_watermark_low_pages(binary()) -> Pages :: integer().
count_watermark_low_pages(Data) ->
    {match, Captured} = re:run(Data, "\\s*low\\s*(\\d+)", [{capture, all_but_first, binary}, global]),
    lists:sum([binary_to_integer(Val) || [Val] <- Captured]).

%%--------------------------------------------------------------------
%% @doc
%% Reads zoneinfo file and returns its data.
%% @end
%%--------------------------------------------------------------------
-spec get_zoneinfo() -> binary().
get_zoneinfo() ->
    {ok, Fd} = file:open(?ZONE_INFO_FILE, [read]),
    {ok, Data} = file:read(Fd, ?BUFFER_SIZE),
    file:close(Fd),
    Data.

%%--------------------------------------------------------------------
%% @doc
%% Get os page size in KB
%% @end
%%--------------------------------------------------------------------
-spec get_page_size() -> integer().
get_page_size() ->
    % code taken from memsup:get_memory_usage
    case os:type() of
        {unix,darwin} -> 4096;
        {unix,OSname} when OSname == freebsd; OSname == dragonfly ->
            list_to_integer(os:cmd("/sbin/sysctl -n vm.stats.vm.v_page_size") -- "\n");
        {unix, linux} ->
            try list_to_integer(os:cmd("getconf PAGESIZE") -- "\n")
            catch
                _:_ ->
                    4096
            end;
        _ -> 4096
    end.