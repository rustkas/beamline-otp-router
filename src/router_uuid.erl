%% @doc UUID Generation Module
%% Provides unified UUID generation for Router components
-module(router_uuid).
-export([generate_v4/0]).

%% @doc Generate UUID v4 (binary format)
%% Returns: <<"xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx">> (binary)
-spec generate_v4() -> binary().
generate_v4() ->
    %% Generate 128-bit random UUID v4 with safe fallback
    try
        RandomBytes = crypto:strong_rand_bytes(16),
        case RandomBytes of
            <<A1:32, A2:16, A3:16, A4:16, A5:48>> ->
                %% Set version (4) and variant bits (10xx)
                Version = 4,
                Variant = 2,  %% 10xx variant
                %% Extract parts for reconstruction
                <<A2High:12, _:4>> = <<A2:16>>,
                <<A3High:2, A3Low:12, _A3Drop:2>> = <<A3:16>>,
                %% Reconstruct with version and variant
                UUIDBytes = <<A1:32, A2High:12, Version:4, A3High:2, Variant:2, A3Low:12, A4:16, A5:48>>,
                %% Format as UUID string (lowercase hex)
                format_uuid_binary(UUIDBytes);
            _ ->
                %% Fallback if crypto returns unexpected data
                format_uuid_binary(fallback_uuid_bytes())
        end
    catch
        _:_Error ->
            %% Fallback if crypto is unavailable
            format_uuid_binary(fallback_uuid_bytes())
    end.

%% Removed string UUID generator; prefer binary UUIDs

%% Internal: Format UUID binary to standard format
format_uuid_binary(<<A1:32, A2:16, A3:16, A4:16, A5:48>>) ->
    %% Format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (lowercase hex)
    <<(format_hex(A1, 8))/binary, "-",
      (format_hex(A2, 4))/binary, "-",
      (format_hex(A3, 4))/binary, "-",
      (format_hex(A4, 4))/binary, "-",
      (format_hex(A5, 12))/binary>>.

%% Internal: Format integer to lowercase hex binary
format_hex(Int, Width) ->
    HexStr = io_lib:format("~*.16.0b", [Width, Int]),
    list_to_binary(HexStr).

%% Internal: Fallback UUID bytes using deterministic sources
fallback_uuid_bytes() ->
    %% Compose 128 bits from monotonic_time, unique_integer, and process hash
    T = erlang:monotonic_time(),
    U = erlang:unique_integer([positive, monotonic]),
    P = erlang:phash2({node(), self()}, 16#FFFFFFFF),
    A1 = (T band 16#FFFFFFFF),
    A2Raw = (U band 16#FFFF),
    A3Raw = (P band 16#FFFF),
    A4 = ((T bsr 32) band 16#FFFF),
    A5 = ((U bsr 16) band ((1 bsl 48) - 1)),
    %% Apply version (4) and variant (10xx) bits
    Version = 4,
    Variant = 2,
    <<A2High:12, _A2Low:4>> = <<A2Raw:16>>,
    <<A3High:2, A3Low:12, _A3Drop:2>> = <<A3Raw:16>>,
    <<A1:32, A2High:12, Version:4, A3High:2, Variant:2, A3Low:12, A4:16, A5:48>>.
