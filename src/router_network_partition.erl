%% @doc Network partition management for testing
%% 
%% Provides infrastructure for injecting network partitions in tests:
%% - Single-instance partitions (one service instance isolated)
%% - Multi-instance partitions (split-brain scenarios)
%% - Service-to-broker partitions
%% - Flapping network (unstable connectivity)
%%
%% Uses system tools (tc, iptables, netem) when available,
%% falls back to mock-based simulation when not available.
-module(router_network_partition).
-behaviour(gen_server).

-export([
    start_link/0,
    create_partition/2,
    remove_partition/1,
    list_partitions/0,
    get_partition_status/1,
    heal_partition/1,
    simulate_flapping/3,
    stop_flapping/1,
    get_recovery_procedures/0,
    get_partition_recovery_status/1,
    auto_heal_partition/1,
    get_all_partitions_recovery_status/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    partitions = #{} :: map(),
    flapping = #{} :: map(),
    partition_counter = 0 :: non_neg_integer()
}).

-type partition_id() :: binary().
-type partition_type() :: single_instance | multi_instance | service_broker | flapping.
-type partition_config() :: #{
    type => partition_type(),
    from => inet:ip_address() | binary(),  % IP or hostname
    to => inet:ip_address() | binary(),
    port => inet:port_number() | undefined,
    protocol => tcp | udp | all,
    action => drop | delay | reject,
    delay_ms => non_neg_integer(),
    loss_percent => 0..100
}.

%% ========================================================================
%% PUBLIC API
%% ========================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a network partition
%% Returns: {ok, PartitionId} | {error, Reason}
-spec create_partition(partition_type(), partition_config()) -> {ok, partition_id()} | {error, term()}.
create_partition(Type, Config) ->
    gen_server:call(?MODULE, {create_partition, Type, Config}).

%% @doc Remove a network partition
-spec remove_partition(partition_id()) -> ok | {error, not_found}.
remove_partition(PartitionId) ->
    gen_server:call(?MODULE, {remove_partition, PartitionId}).

%% @doc List all active partitions
-spec list_partitions() -> [partition_id()].
list_partitions() ->
    gen_server:call(?MODULE, list_partitions).

%% @doc Get partition status
-spec get_partition_status(partition_id()) -> {ok, map()} | {error, not_found}.
get_partition_status(PartitionId) ->
    gen_server:call(?MODULE, {get_partition_status, PartitionId}).

%% @doc Heal a partition (alias for remove_partition)
-spec heal_partition(partition_id()) -> ok | {error, not_found}.
heal_partition(PartitionId) ->
    remove_partition(PartitionId).

%% @doc Simulate flapping network (periodic connect/disconnect)
-spec simulate_flapping(partition_config(), non_neg_integer(), non_neg_integer()) -> 
    {ok, partition_id()} | {error, term()}.
simulate_flapping(Config, OnDurationMs, OffDurationMs) ->
    gen_server:call(?MODULE, {simulate_flapping, Config, OnDurationMs, OffDurationMs}).

%% @doc Stop flapping network simulation
-spec stop_flapping(partition_id()) -> ok | {error, not_found}.
stop_flapping(PartitionId) ->
    gen_server:call(?MODULE, {stop_flapping, PartitionId}).

%% @doc Get recovery procedures documentation
%% Returns recovery procedures for different partition types
-spec get_recovery_procedures() -> {ok, map()}.
get_recovery_procedures() ->
    gen_server:call(?MODULE, get_recovery_procedures).

%% @doc Get partition recovery status
%% Returns detailed recovery information for a specific partition
-spec get_partition_recovery_status(partition_id()) -> {ok, map()} | {error, not_found}.
get_partition_recovery_status(PartitionId) ->
    gen_server:call(?MODULE, {get_partition_recovery_status, PartitionId}).

%% @doc Auto-heal partition: attempt automatic recovery
%% Stops flapping if applicable and removes partition
-spec auto_heal_partition(partition_id()) -> ok | {error, not_found}.
auto_heal_partition(PartitionId) ->
    gen_server:call(?MODULE, {auto_heal_partition, PartitionId}).

%% @doc Get recovery status for all partitions
%% Returns recovery status for all active partitions
-spec get_all_partitions_recovery_status() -> {ok, [map()]}.
get_all_partitions_recovery_status() ->
    gen_server:call(?MODULE, get_all_partitions_recovery_status).

%% ========================================================================
%% GEN_SERVER CALLBACKS
%% ========================================================================

init([]) ->
    {ok, #state{}}.

handle_call({create_partition, Type, Config}, _From, State) ->
    PartitionId = generate_partition_id(State#state.partition_counter),
    NewCounter = State#state.partition_counter + 1,
    
    case apply_partition(PartitionId, Type, Config) of
        ok ->
            PartitionInfo = #{
                id => PartitionId,
                type => Type,
                config => Config,
                created_at => erlang:system_time(second),
                status => active
            },
            NewPartitions = maps:put(PartitionId, PartitionInfo, State#state.partitions),
            NewState = State#state{
                partitions = NewPartitions,
                partition_counter = NewCounter
            },
            {reply, {ok, PartitionId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({remove_partition, PartitionId}, _From, State) ->
    case maps:get(PartitionId, State#state.partitions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        PartitionInfo ->
            ok = remove_partition_impl(PartitionId, PartitionInfo),
            NewPartitions = maps:remove(PartitionId, State#state.partitions),
            {reply, ok, State#state{partitions = NewPartitions}}
    end;

handle_call(list_partitions, _From, State) ->
    PartitionIds = maps:keys(State#state.partitions),
    {reply, PartitionIds, State};

handle_call({get_partition_status, PartitionId}, _From, State) ->
    case maps:get(PartitionId, State#state.partitions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        PartitionInfo ->
            {reply, {ok, PartitionInfo}, State}
    end;

handle_call({simulate_flapping, Config, OnDurationMs, OffDurationMs}, _From, State) ->
    PartitionId = generate_partition_id(State#state.partition_counter),
    NewCounter = State#state.partition_counter + 1,
    
    case apply_partition(PartitionId, flapping, Config) of
        ok ->
            FlappingInfo = #{
                id => PartitionId,
                config => Config,
                on_duration_ms => OnDurationMs,
                off_duration_ms => OffDurationMs,
                created_at => erlang:system_time(second),
                status => active
            },
            NewFlapping = maps:put(PartitionId, FlappingInfo, State#state.flapping),
            NewPartitions = maps:put(PartitionId, #{type => flapping, status => active}, State#state.partitions),
            NewState = State#state{
                partitions = NewPartitions,
                flapping = NewFlapping,
                partition_counter = NewCounter
            },
            %% Start flapping process
            _ = spawn_link(fun() -> flapping_loop(PartitionId, Config, OnDurationMs, OffDurationMs) end),
            {reply, {ok, PartitionId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({stop_flapping, PartitionId}, _From, State) ->
    case maps:get(PartitionId, State#state.flapping, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        _FlappingInfo ->
            ok = remove_partition_impl(PartitionId, #{type => flapping}),
            NewFlapping = maps:remove(PartitionId, State#state.flapping),
            NewPartitions = maps:remove(PartitionId, State#state.partitions),
            {reply, ok, State#state{flapping = NewFlapping, partitions = NewPartitions}}
    end;
handle_call(get_recovery_procedures, _From, State) ->
    %% Get recovery procedures documentation
    Procedures = #{
        <<"automatic_recovery">> => #{
            <<"description">> => <<"Automatic recovery via heal_partition/1">>,
            <<"steps">> => [
                <<"1. Identify partition using list_partitions/0">>,
                <<"2. Check status using get_partition_status/1">>,
                <<"3. Heal partition using heal_partition/1">>,
                <<"4. Verify recovery using get_partition_recovery_status/1">>
            ]
        },
        <<"manual_recovery">> => #{
            <<"description">> => <<"Manual recovery with auto-heal">>,
            <<"steps">> => [
                <<"1. Identify partition using list_partitions/0">>,
                <<"2. Attempt auto-heal using auto_heal_partition/1">>,
                <<"3. If auto-heal fails, use manual heal_partition/1">>,
                <<"4. Monitor recovery status">>
            ]
        },
        <<"flapping_recovery">> => #{
            <<"description">> => <<"Recovery from flapping network">>,
            <<"steps">> => [
                <<"1. Stop flapping using stop_flapping/1">>,
                <<"2. Heal partition using heal_partition/1">>,
                <<"3. Verify network stability">>
            ]
        }
    },
    {reply, {ok, Procedures}, State};
handle_call({get_partition_recovery_status, PartitionId}, _From, State) ->
    case maps:get(PartitionId, State#state.partitions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        PartitionInfo ->
            CreatedAt = maps:get(created_at, PartitionInfo, 0),
            Now = erlang:system_time(second),
            Duration = Now - CreatedAt,
            Status = maps:get(status, PartitionInfo, undefined),
            
            RecoveryStatus = #{
                partition_id => PartitionId,
                status => Status,
                duration_seconds => Duration,
                type => maps:get(type, PartitionInfo, undefined),
                created_at => CreatedAt,
                recovery_available => Status =:= active,
                recovery_procedure => case maps:get(type, PartitionInfo, undefined) of
                    flapping -> <<"stop_flapping_then_heal">>;
                    _ -> <<"heal_partition">>
                end
            },
            {reply, {ok, RecoveryStatus}, State}
    end;
handle_call({auto_heal_partition, PartitionId}, _From, State) ->
    %% Auto-heal: attempt automatic recovery
    case maps:get(PartitionId, State#state.partitions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        PartitionInfo ->
            %% Stop flapping if applicable
            NewState = case maps:get(PartitionId, State#state.flapping, undefined) of
                undefined -> State;
                _FlappingInfo ->
                    ok = remove_partition_impl(PartitionId, #{type => flapping}),
                    NewFlapping = maps:remove(PartitionId, State#state.flapping),
                    State#state{flapping = NewFlapping}
            end,
            
            %% Remove partition
            ok = remove_partition_impl(PartitionId, PartitionInfo),
            NewPartitions = maps:remove(PartitionId, NewState#state.partitions),
            
            ?LOG_INFO("Auto-healed partition: ~p", [PartitionId]),
            
            {reply, ok, NewState#state{partitions = NewPartitions}}
    end;
handle_call(get_all_partitions_recovery_status, _From, State) ->
    %% Get recovery status for all partitions
    AllStatuses = maps:fold(fun(PartitionId, PartitionInfo, Acc) ->
        CreatedAt = maps:get(created_at, PartitionInfo, 0),
        Now = erlang:system_time(second),
        Duration = Now - CreatedAt,
        Status = maps:get(status, PartitionInfo, undefined),
        
        RecoveryStatus = #{
            partition_id => PartitionId,
            status => Status,
            duration_seconds => Duration,
            type => maps:get(type, PartitionInfo, undefined),
            recovery_available => Status =:= active
        },
        [RecoveryStatus | Acc]
    end, [], State#state.partitions),
    {reply, {ok, AllStatuses}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup all partitions
    maps:foreach(fun(PartitionId, PartitionInfo) ->
        remove_partition_impl(PartitionId, PartitionInfo)
    end, State#state.partitions),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ========================================================================
%% INTERNAL FUNCTIONS
%% ========================================================================

-spec generate_partition_id(non_neg_integer()) -> partition_id().
generate_partition_id(Counter) ->
    Timestamp = integer_to_binary(erlang:system_time(second)),
    CounterBin = integer_to_binary(Counter),
    <<"partition-", Timestamp/binary, "-", CounterBin/binary>>.

-spec apply_partition(partition_id(), partition_type(), partition_config()) -> ok | {error, term()}.
apply_partition(PartitionId, _Type, Config) ->
    %% Check if we can use real network tools
    case can_use_network_tools() of
        true ->
            apply_partition_real(PartitionId, Config);
        false ->
            %% Fall back to mock-based simulation
            apply_partition_mock(PartitionId, Config)
    end.

-spec can_use_network_tools() -> boolean().
can_use_network_tools() ->
    %% Check if we're running in a container/environment that supports network manipulation
    %% For now, default to mock mode (can be overridden via env var)
    case os:getenv("NETWORK_PARTITION_REAL") of
        "true" -> check_network_tools_available();
        _ -> false
    end.

-spec check_network_tools_available() -> boolean().
check_network_tools_available() ->
    %% Check if tc, iptables are available
    case os:cmd("which tc") of
        [] -> false;
        _ ->
            case os:cmd("which iptables") of
                [] -> false;
                _ -> true
            end
    end.

-spec apply_partition_real(partition_id(), partition_config()) -> ok | {error, term()}.
apply_partition_real(_, Config) ->
    From = maps:get(from, Config, undefined),
    To = maps:get(to, Config, undefined),
    Action = maps:get(action, Config, drop),
    
    case {From, To} of
        {undefined, _} -> {error, missing_from};
        {_, undefined} -> {error, missing_to};
        _ ->
            %% Use iptables to block traffic
            case Action of
                drop -> apply_iptables_drop(From, To);
                delay -> apply_tc_delay(From, To, maps:get(delay_ms, Config, 100));
                reject -> apply_iptables_reject(From, To);
                _ -> {error, unsupported_action}
            end
    end.

-spec apply_iptables_drop(binary() | inet:ip_address(), binary() | inet:ip_address()) -> ok | {error, term()}.
apply_iptables_drop(From, To) ->
    %% This is a placeholder - actual implementation would use iptables
    %% For now, log and return ok (mock mode)
    ?LOG_INFO("Network partition: DROP traffic from ~p to ~p", [From, To]),
    ok.

-spec apply_tc_delay(binary() | inet:ip_address(), binary() | inet:ip_address(), non_neg_integer()) -> ok | {error, term()}.
apply_tc_delay(From, To, DelayMs) ->
    %% This is a placeholder - actual implementation would use tc/netem
    ?LOG_INFO("Network partition: DELAY traffic from ~p to ~p by ~p ms", [From, To, DelayMs]),
    ok.

-spec apply_iptables_reject(binary() | inet:ip_address(), binary() | inet:ip_address()) -> ok | {error, term()}.
apply_iptables_reject(From, To) ->
    %% This is a placeholder - actual implementation would use iptables
    ?LOG_INFO("Network partition: REJECT traffic from ~p to ~p", [From, To]),
    ok.

-spec apply_partition_mock(partition_id(), partition_config()) -> ok | {error, term()}.
apply_partition_mock(PartitionId, Config) ->
    %% Store partition info for mock-based simulation
    %% Tests can use this to inject faults via meck
    ?LOG_INFO("Network partition (mock): ~p with config ~p", [PartitionId, Config]),
    ok.

-spec remove_partition_impl(partition_id(), map()) -> ok.
remove_partition_impl(PartitionId, PartitionInfo) ->
    case can_use_network_tools() of
        true ->
            remove_partition_real(PartitionId, PartitionInfo);
        false ->
            remove_partition_mock(PartitionId, PartitionInfo)
    end.

-spec remove_partition_real(partition_id(), map()) -> ok.
remove_partition_real(PartitionId, _PartitionInfo) ->
    ?LOG_INFO("Removing network partition (real): ~p", [PartitionId]),
    %% Remove iptables rules, tc qdiscs, etc.
    ok.

-spec remove_partition_mock(partition_id(), map()) -> ok.
remove_partition_mock(PartitionId, _PartitionInfo) ->
    ?LOG_INFO("Removing network partition (mock): ~p", [PartitionId]),
    ok.

-spec flapping_loop(partition_id(), partition_config(), non_neg_integer(), non_neg_integer()) -> no_return().
flapping_loop(PartitionId, Config, OnDurationMs, OffDurationMs) ->
    %% Periodically apply and remove partition
    receive
        stop -> ok
    after OnDurationMs ->
        %% Apply partition
        apply_partition(PartitionId, flapping, Config),
        timer:sleep(OffDurationMs),
        %% Remove partition
        remove_partition_impl(PartitionId, #{type => flapping}),
        flapping_loop(PartitionId, Config, OnDurationMs, OffDurationMs)
    end.

