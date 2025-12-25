-module(router_duration).

-doc "Duration helper for unified time calculations".
%%
%% This module provides a single helper for calculating durations
%% using monotonic time, ensuring consistency across all code paths
%% (including catch blocks).

-export([duration_ms/2, duration_us/2, duration_seconds/2]).

%% Uses monotonic time for consistency
-spec duration_ms(StartTime :: integer(), EndTime :: integer()) -> float().
duration_ms(StartTime, EndTime) when is_integer(StartTime), is_integer(EndTime) ->
    DurationUs = EndTime - StartTime,
    DurationUs / 1000.0.

%% Uses monotonic time for consistency
-spec duration_us(StartTime :: integer(), EndTime :: integer()) -> integer().
duration_us(StartTime, EndTime) when is_integer(StartTime), is_integer(EndTime) ->
    EndTime - StartTime.

%% Uses monotonic time for consistency
-spec duration_seconds(StartTime :: integer(), EndTime :: integer()) -> float().
duration_seconds(StartTime, EndTime) when is_integer(StartTime), is_integer(EndTime) ->
    DurationUs = EndTime - StartTime,
    DurationUs / 1_000_000.0.

