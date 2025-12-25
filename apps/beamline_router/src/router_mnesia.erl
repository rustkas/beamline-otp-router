-module(router_mnesia).

-doc "Mnesia Tables Initialization".
%% Manages Mnesia table creation and operations
-export([init/0, create_tables/0]).

-include("beamline_router.hrl").

init() ->
    case mnesia:system_info(is_running) of
        yes ->
            create_tables();
        _ ->
            mnesia:create_schema([node()]),
            mnesia:start(),
            create_tables()
    end,
    ok.

create_tables() ->
    %% Policy cache table
    {atomic, ok} = mnesia:create_table(
        policy_cache,
        [{type, set},
         {ram_copies, [node()]},
         {attributes, record_info(fields, policy_cache)},
         {index, []}]),

    %% Sticky sessions table
    {atomic, ok} = mnesia:create_table(
        sticky_sessions,
        [{type, set},
         {ram_copies, [node()]},
         {attributes, record_info(fields, sticky_session)},
         {index, [tenant_id]}]),

    %% Rate counters table (optional for CP1)
    {atomic, ok} = mnesia:create_table(
        rate_counters,
        [{type, set},
         {ram_copies, [node()]},
         {attributes, [key, count, reset_at]},
         {index, []}]),

    %% Idempotency table (optional for CP1)
    {atomic, ok} = mnesia:create_table(
        idempotency,
        [{type, set},
         {ram_copies, [node()]},
         {attributes, [key, result, expires_at]},
         {index, []}]),

    ok.

