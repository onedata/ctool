%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Module responsible for calculating available memory, using algorithm from:
%%% https://github.com/famzah/linux-memavailable-procfs/blob/master/Linux-MemAvailable.pm
%%% @end
%%%--------------------------------------------------------------------
-module(available_memory).
-author("Tomasz Lichon").

-define(ZONE_INFO_FILE, "/proc/zoneinfo").
-define(MEM_INFO_FILE, "/proc/meminfo").
-define(BUFFER_SIZE, 10485760). %10MB
%% API
-export([calculate/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Return size of available memory.
%% @end
%%--------------------------------------------------------------------
-spec calculate() -> KB :: integer().
calculate() ->
    Zoneinfo = get_zoneinfo(),
    Meminfo = get_meminfo(),
    WMarkLow = count_watermark_low_kb(Zoneinfo),
    PageCache = count_pagecache(Meminfo, WMarkLow),
    SReclaimable = count_slab_reclaimable(Meminfo),
    MemFree = count_free(Meminfo),

    max(MemFree - WMarkLow + PageCache + SReclaimable - min(SReclaimable div 2, WMarkLow), 0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Count watermark low pages from zoneinfo data.
%% @end
%%--------------------------------------------------------------------
-spec count_watermark_low_kb(binary()) -> Pages :: integer().
count_watermark_low_kb(Data) ->
    {match, Captured} = re:run(Data, "\\s*low\\s*(\\d+)", [{capture, all_but_first, binary}, global]),
    lists:sum([binary_to_integer(Val) || [Val] <- Captured]) * get_page_size().

%%--------------------------------------------------------------------
%% @doc
%% Count pagecache from meminfo and watermark_low data
%% @end
%%--------------------------------------------------------------------
-spec count_pagecache(Data :: binary(), WatermarkLowKB :: integer()) -> integer().
count_pagecache(Data, WatermarkLow) ->
    {match, [LruActive]} = re:run(Data, "\\s*Active\\(file\\):\\s*(\\d+)", [{capture, all_but_first, binary}]),
    {match, [LruInactive]} = re:run(Data, "\\s*Inactive\\(file\\):\\s*(\\d+)", [{capture, all_but_first, binary}]),
    PageCache = binary_to_integer(LruActive) + binary_to_integer(LruInactive),
    PageCache - min(PageCache div 2, WatermarkLow).

%%--------------------------------------------------------------------
%% @doc
%% Get slab_reclaimable from meminfo
%% @end
%%--------------------------------------------------------------------
-spec count_slab_reclaimable(Data :: binary()) -> integer().
count_slab_reclaimable(Data) ->
    {match, [SlabReclaimable]} = re:run(Data, "\\s*SReclaimable:\\s*(\\d+)", [{capture, all_but_first, binary}]),
    binary_to_integer(SlabReclaimable).

%%--------------------------------------------------------------------
%% @doc
%% Get mem_free from meminfo
%% @end
%%--------------------------------------------------------------------
-spec count_free(Data :: binary()) -> integer().
count_free(Data) ->
    {match, [MemFree]} = re:run(Data, "\\s*MemFree:\\s*(\\d+)", [{capture, all_but_first, binary}]),
    binary_to_integer(MemFree).

%%--------------------------------------------------------------------
%% @doc
%% Reads meminfo file and returns its data.
%% @end
%%--------------------------------------------------------------------
-spec get_meminfo() -> binary().
get_meminfo() ->
    {ok, Fd} = file:open(?MEM_INFO_FILE, [read]),
    {ok, Data} = file:read(Fd, ?BUFFER_SIZE),
    file:close(Fd),
    Data.

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
    Val = case os:type() of
        {unix, darwin} -> 4096;
        {unix, OSname} when OSname == freebsd; OSname == dragonfly ->
            list_to_integer(os:cmd("/sbin/sysctl -n vm.stats.vm.v_page_size") -- "\n");
        {unix, linux} ->
            try list_to_integer(os:cmd("getconf PAGESIZE") -- "\n")
            catch
                _:_ ->
                    4096
            end;
        _ -> 4096
    end,
    Val div 1024.