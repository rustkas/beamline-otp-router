-module(router_nats).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([js_setup_consumer/2, js_ack/1, js_nak/2, js_dlq/2]).
-export([
    subscribe_jetstream/5,
    publish/2,
    publish_with_ack/2,
    publish_with_ack/3,
    ack_message/1,
    nak_message/1,
    subscribe/3,
    request/3,
    stop/0
]).
-export([get_connection_status/0, reconnect/0]).
-ifdef(TEST).
-export([simulate_connection_lost/0]).
-endif.
-export([get_connection_health/0, get_jetstream_consumers/0]).
%% Helper functions for testing
-export([error_to_reason/1, extract_stream_from_subject/1]).
%% Helper functions for context extraction
-export([extract_nats_context_from_msgid/1, get_default_nats_context/0]).
%% Exported for testing
-export([get_publish_retry_config/0, classify_error_type/1]).
%% Exported for testing (internal functions)
-export([do_publish_internal/3]).

-ignore_xref([
    {router_nats, start_link, 0},
    {router_nats, reconnect, 0},
    {router_nats, js_setup_consumer, 2},
    {router_nats, js_ack, 1},
    {router_nats, js_nak, 2},
    {router_nats, js_dlq, 2},
    {router_nats, get_connection_status, 0}
]).

-include("beamline_router.hrl").

%% Connection states
-define(CONN_STATE_DISCONNECTED, disconnected).
-define(CONN_STATE_CONNECTING, connecting).
-define(CONN_STATE_CONNECTED, connected).
-define(CONN_STATE_RECONNECTING, reconnecting).

%% Default configuration
-define(DEFAULT_RECONNECT_ATTEMPTS, 10).
-define(DEFAULT_RECONNECT_DELAY_MS, 1000).
-define(DEFAULT_MAX_RECONNECT_DELAY_MS, 30000).

-record(state, {
    connection_state = ?CONN_STATE_DISCONNECTED :: atom(),
    connection_pid = undefined :: pid() | undefined,
    reconnect_attempts = 0 :: non_neg_integer(),
    max_reconnect_attempts = ?DEFAULT_RECONNECT_ATTEMPTS :: non_neg_integer(),
    reconnect_timer = undefined :: reference() | undefined,
    %% Fail-open mode when NATS unavailable
    fail_open_mode = false :: boolean(),
    %% Queue of operations to retry after reconnect
    pending_operations = [] :: list(),
    %% Timestamp of last successful connection
    last_connection_time = undefined :: integer() | undefined,
    %% Timestamp of last connection failure
    last_failure_time = undefined :: integer() | undefined,
    %% Number of successful health checks
    connection_health_checks = 0 :: non_neg_integer(),
    %% Map of ConsumerId => ConsumerState for JetStream subscriptions
    jetstream_consumers = #{} :: map()
}).

%% JetStream consumer state record
-record(jetstream_consumer, {
    consumer_id :: binary(),
    subject :: binary(),
    durable_group :: binary() | undefined,
    ack_policy :: atom(),
    deliver_group :: binary() | undefined,
    mode :: atom(),
    created_at :: integer(),
    last_message_time :: integer() | undefined,
    message_count = 0 :: non_neg_integer()
}).

%% @doc Start link for supervisor
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize gen_server
init([]) ->
    MaxReconnectAttempts = application:get_env(
        beamline_router, nats_reconnect_attempts, ?DEFAULT_RECONNECT_ATTEMPTS
    ),
    FailOpenMode = application:get_env(beamline_router, nats_fail_open_mode, false),

    State = #state{
        connection_state = ?CONN_STATE_DISCONNECTED,
        max_reconnect_attempts = MaxReconnectAttempts,
        fail_open_mode = FailOpenMode
    },

    %% Attempt initial connection
    case attempt_connection() of
        {ok, ConnectionPid} ->
            router_logger:info(<<"NATS connection established">>, #{
                <<"error_code">> => <<"NATS_CONNECTION_ESTABLISHED">>
            }),
            router_metrics:inc(router_nats_connection_established_total),
            %% Monitor connection process
            erlang:monitor(process, ConnectionPid),
            %% Update connection status gauge (1 = connected)
            router_metrics:emit_metric(router_nats_connection_status, #{value => 1}, #{
                state => connected
            }),
            Now = erlang:system_time(millisecond),
            {ok, State#state{
                connection_state = ?CONN_STATE_CONNECTED,
                connection_pid = ConnectionPid,
                last_connection_time = Now,
                connection_health_checks = 0
            }};
        {error, Reason} ->
            router_logger:warn(<<"NATS initial connection failed, will retry">>, #{
                <<"reason">> => sanitize_error(Reason)
            }),
            %% Emit connection failure metric with labels
            ReasonBin = error_to_reason(Reason),
            Cluster = get_nats_cluster(),
            router_metrics:emit_metric(router_nats_connect_failures_total, #{count => 1}, #{
                reason => ReasonBin,
                cluster => Cluster,
                source => <<"initial_connect">>
            }),
            %% Schedule reconnect
            NewState = schedule_reconnect(State),
            {ok, NewState}
    end.

%% @doc Handle calls
handle_call({publish, Subject, Payload}, _From, State) ->
    Result = do_publish(Subject, Payload, State),
    %% Update state if operation was queued
    NewState =
        case Result of
            {error, not_connected} when not State#state.fail_open_mode ->
                queue_operation({publish, Subject, Payload}, State);
            _ ->
                State
        end,
    {reply, Result, NewState};
handle_call({publish_with_ack, Subject, Payload, Headers}, _From, State) ->
    Result = do_publish_with_ack(Subject, Payload, Headers, State),
    %% Update state if operation was queued
    NewState =
        case Result of
            {error, not_connected} when not State#state.fail_open_mode ->
                queue_operation({publish_with_ack, Subject, Payload, Headers}, State);
            _ ->
                State
        end,
    {reply, Result, NewState};
handle_call({ack_message, MsgId}, _From, State) ->
    Result = do_ack_message(MsgId, State),
    {reply, Result, State};
handle_call({nak_message, MsgId}, _From, State) ->
    Result = do_nak_message(MsgId, State),
    {reply, Result, State};
handle_call(
    {subscribe_jetstream, Subject, DurableGroup, AckPolicy, DeliverGroup, Mode}, _From, State
) ->
    Result = do_subscribe_jetstream(Subject, DurableGroup, AckPolicy, DeliverGroup, Mode, State),
    case Result of
        {ok, ConsumerId, NewState} ->
            {reply, {ok, ConsumerId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(get_connection_status, _From, State) ->
    Status = build_connection_status(State),
    {reply, {ok, Status}, State};
%% TEST-ONLY: simulate connection lost via exported function under TEST macro
handle_call({get_connection_health}, _From, State) ->
    Health = check_connection_health(State),
    {reply, Health, State};
handle_call(
    {register_jetstream_consumer, ConsumerId, Subject, DurableGroup, AckPolicy, DeliverGroup, Mode},
    _From,
    State
) ->
    NewState = register_jetstream_consumer(
        ConsumerId, Subject, DurableGroup, AckPolicy, DeliverGroup, Mode, State
    ),
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    %% Stop the gen_server
    {stop, normal, ok, State};
handle_call({unregister_jetstream_consumer, ConsumerId}, _From, State) ->
    NewState = unregister_jetstream_consumer(ConsumerId, State),
    {reply, ok, NewState};
handle_call({get_jetstream_consumers}, _From, State) ->
    Consumers = maps:values(State#state.jetstream_consumers),
    {reply, {ok, Consumers}, State};
handle_call(reconnect, _From, State) ->
    NewState = handle_reconnect(State),
    {reply, {ok, NewState#state.connection_state}, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle casts
handle_cast({connection_lost, Reason}, State) ->
    NewState = handle_connection_lost(Reason, State),
    {noreply, NewState};
handle_cast({connection_restored, Pid}, State) ->
    NewState = handle_connection_restored(Pid, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(reconnect, State) ->
    NewState = handle_reconnect(State),
    {noreply, NewState};
handle_info(health_check, State) ->
    NewState = perform_health_check(State),
    {noreply, NewState};
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) when Pid =:= State#state.connection_pid ->
    %% Connection process died
    NewState = handle_connection_lost(Reason, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate
terminate(_Reason, State) ->
    case State#state.reconnect_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    case State#state.connection_pid of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            %% Close connection if exists
            catch exit(Pid, normal),
            ok
    end,
    ok.

%% ============================================================================
%% Public API Functions
%% ============================================================================

%% @doc Get connection status
get_connection_status() ->
    gen_server:call(?MODULE, get_connection_status).

%% @doc Manually trigger reconnect
reconnect() ->
    gen_server:call(?MODULE, reconnect).

-ifdef(TEST).
simulate_connection_lost() ->
    gen_server:call(?MODULE, reconnect),
    gen_server:cast(?MODULE, {connection_lost, test_injected}),
    ok.
-endif.

%% @doc Stop router_nats
stop() ->
    gen_server:call(?MODULE, stop).

%% @doc Get connection health status
get_connection_health() ->
    gen_server:call(?MODULE, {get_connection_health}).

%% @doc Get list of registered JetStream consumers
get_jetstream_consumers() ->
    gen_server:call(?MODULE, {get_jetstream_consumers}).

%% JetStream functions (legacy API - use ack_message/nak_message/publish_with_ack directly)
js_setup_consumer(Stream, Consumer) -> {ok, {Stream, Consumer}}.

%% @doc Acknowledge JetStream message (legacy API - use ack_message/1 directly)
js_ack(MsgId) ->
    ack_message(MsgId).

%% @doc Negative acknowledge JetStream message (legacy API - use nak_message/1 directly)
js_nak(MsgId, Reason) ->
    %% Note: nak_message/1 doesn't take reason, but we log it via telemetry
    case nak_message(MsgId) of
        ok -> {ok, MsgId};
        Error -> {error, {MsgId, Reason, Error}}
    end.

%% @doc Send message to DLQ (legacy API - use publish_with_ack/3 directly)
%% Stream: DLQ stream name (not used, subject is built from original subject)
%% Payload: Message map to send to DLQ
js_dlq(_Stream, Payload) when is_map(Payload) ->
    %% Extract subject from payload
    Subject = maps:get(subject, Payload, <<"unknown.dlq">>),
    DLQSubject = build_dlq_subject(Subject),
    DLQJson = jsx:encode(Payload),
    publish_with_ack(DLQSubject, DLQJson, #{}).

%% @doc Subscribe to JetStream subject
subscribe_jetstream(Subject, DurableGroup, AckPolicy, DeliverGroup, Mode) ->
    gen_server:call(
        ?MODULE, {subscribe_jetstream, Subject, DurableGroup, AckPolicy, DeliverGroup, Mode}
    ).

%% @doc Publish message to NATS subject
publish(Subject, Payload) ->
    gen_server:call(?MODULE, {publish, Subject, Payload}).

publish_with_ack(Subject, Payload) ->
    publish_with_ack(Subject, Payload, #{}).

%% @doc Publish message with ack
publish_with_ack(Subject, Payload, Headers) ->
    gen_server:call(?MODULE, {publish_with_ack, Subject, Payload, Headers}).

%% @doc Acknowledge message
ack_message(MsgId) ->
    gen_server:call(?MODULE, {ack_message, MsgId}).

%% @doc Negative acknowledge message
nak_message(MsgId) ->
    gen_server:call(?MODULE, {nak_message, MsgId}).

%% @doc Subscribe to NATS subject (stub for CP1)
subscribe(_Subject, _Callback, _Timeout) ->
    ok.

%% @doc Request-reply pattern (stub for CP1)
request(_Subject, _Payload, _Timeout) ->
    {ok, <<"{}">>}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% @doc Attempt NATS connection
%% For now, returns stub connection (actual NATS client implementation would go here)
attempt_connection() ->
    %% Check for fault injection
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            case router_nats_fault_injection:should_fail(connect, undefined) of
                {true, {error, Reason}} ->
                    {error, Reason};
                {true, close_connection} ->
                    {error, connection_closed};
                {true, timeout} ->
                    {error, timeout};
                false ->
                    do_attempt_connection()
            end;
        false ->
            do_attempt_connection()
    end.

%% @doc Internal connection attempt (without fault injection check)
%%
%% ⚠️ STUB IMPLEMENTATION: Returns mock connection process
%% CP3/Release: Implement actual NATS connection using NATS client library
%%
%% Implementation notes:
%% - Requires external NATS client library (e.g., nats.erl, nats_erl, or similar)
%% - Should establish TCP/TLS connection to NATS server
%% - Should handle authentication (username/password, JWT, NKey)
%% - Should support reconnection logic
%% - Should return {ok, ConnectionPid} or {error, Reason}
%%
%% Current behavior:
%% - Always returns stub process (spawn_link dummy process)
%% - Stub process does nothing (waits for messages)
%% - Safe for testing but not production-ready
% do_attempt_connection() ->
%     %% Validate NATS configuration
%     case validate_nats_config() of
%         {ok, Config} ->
%             %% Configuration valid - proceed with connection attempt
%             case application:get_env(beamline_router, nats_url, undefined) of
%                 undefined ->
%                     %% No NATS URL configured - use stub mode (safe for testing)
%                     router_logger:info(<<"NATS connection: stub mode (no URL configured)">>, #{}),
%                     create_stub_connection(Config);
%                 Url ->
%                     %% NATS URL configured but real connection not implemented
%                     %% STUB IMPLEMENTATION: CP3/Release - Implement actual NATS connection
%                     %%
%                     %% Implementation requirements:
%                     %% - Use NATS client library (e.g., nats.erl, nats_erl, or similar)
%                     %% - Establish TCP/TLS connection to NATS server
%                     %% - Handle authentication (username/password, JWT, NKey)
%                     %% - Implement reconnection logic with exponential backoff
%                     %% - Support connection pooling for high throughput
%                     %% - Return {ok, ConnectionPid} on success or {error, Reason} on failure
%                     %%
%                     %% Current stub behavior:
%                     %% - Returns stub process that accepts messages but does nothing
%                     %% - Safe for testing but not production-ready
%                     %% - All NATS operations (publish, subscribe, ack, nak) are stubbed
%                     router_logger:warn(<<"NATS connection: stub mode (real connection not implemented)">>, #{
%                         <<"nats_url">> => sanitize_url_for_logging(Url),
%                         <<"config_validated">> => true
%                     }),
%                     create_stub_connection(Config)
%             end;
%         {error, Reason} ->
%             router_logger:error(<<"NATS configuration validation failed">>, #{
%                 <<"reason">> => Reason
%             }),
%             {error, {config_invalid, Reason}}
%     end.

%% @doc Internal connection attempt (without fault injection check)
do_attempt_connection() ->
    case validate_nats_config() of
        {ok, Config} ->
            case maps:get(url, Config) of
                undefined ->
                    router_logger:info(<<"NATS connection: stub mode (no URL configured)">>, #{}),
                    create_stub_connection(Config);
                Url ->
                    case connect_to_nats(Url, Config) of
                        {ok, ConnectionPid} ->
                            router_logger:info(<<"NATS connection established">>, #{
                                <<"nats_url">> => sanitize_url_for_logging(Url)
                            }),
                            {ok, ConnectionPid};
                        {error, Reason} ->
                            router_logger:error(<<"NATS connection failed">>, #{
                                <<"nats_url">> => sanitize_url_for_logging(Url),
                                <<"reason">> => Reason
                            }),
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, {config_invalid, Reason}}
    end.

%% @doc Connect to NATS server using enats client
connect_to_nats(Url, Config) ->
    AuthConfig = maps:get(auth, Config, #{}),
    TLSConfig = maps:get(tls, Config, #{}),

    %% Build connection options
    Options = #{
        url => Url,
        max_reconnect_attempts => application:get_env(beamline_router, nats_reconnect_attempts, 10),
        reconnect_time_wait => application:get_env(beamline_router, nats_reconnect_delay_ms, 1000),
        max_reconnect_time_wait => application:get_env(beamline_router, nats_max_reconnect_delay_ms, 30000)
    },

    %% Add authentication if configured
    AuthOptions = case maps:get(auth_type, AuthConfig, none) of
        username_password ->
            Options#{
                username => maps:get(username, AuthConfig, ""),
                password => maps:get(password, AuthConfig, "")
            };
        jwt ->
            Options#{jwt => maps:get(jwt, AuthConfig, "")};
        nkey ->
            Options#{nkey => maps:get(nkey, AuthConfig, "")};
        none ->
            Options
    end,

    %% Add TLS if enabled
    FinalOptions = case maps:get(enabled, TLSConfig, false) of
        true ->
            AuthOptions#{
                ssl => true,
                certfile => maps:get(cert_file, TLSConfig, ""),
                keyfile => maps:get(key_file, TLSConfig, "")
            };
        false ->
            AuthOptions
    end,

    %% Attempt connection using nats module (from enats library)
    case nats:start_link(FinalOptions) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Validate NATS configuration
%% Returns: {ok, ConfigMap} | {error, Reason}
-spec validate_nats_config() -> {ok, map()} | {error, term()}.
validate_nats_config() ->
    Url = application:get_env(beamline_router, nats_url, undefined),
    Cluster = application:get_env(beamline_router, nats_cluster, undefined),
    AuthConfig = get_nats_auth_config(),
    TLSConfig = get_nats_tls_config(),

    %% Validate URL format if provided
    case Url of
        undefined ->
            %% No URL - stub mode is valid
            {ok, #{
                url => undefined,
                cluster => Cluster,
                auth => AuthConfig,
                tls => TLSConfig,
                mode => stub
            }};
        UrlBin when is_binary(UrlBin) ->
            case parse_nats_url(UrlBin) of
                {ok, ParsedUrl} ->
                    {ok, #{
                        url => ParsedUrl,
                        cluster => Cluster,
                        auth => AuthConfig,
                        tls => TLSConfig,
                        %% Will be 'real' when actual connection implemented
                        mode => stub
                    }};
                {error, Reason} ->
                    {error, {invalid_url, Reason}}
            end;
        _ ->
            {error, invalid_url_format}
    end.

%% @doc Parse NATS URL (stub-level helper for when real implementation is added)
%% Supports: nats://host:port, nats://user:pass@host:port, tls://host:port
-spec parse_nats_url(binary()) -> {ok, map()} | {error, term()}.
parse_nats_url(UrlBin) when is_binary(UrlBin) ->
    try
        UrlStr = binary_to_list(UrlBin),
        case uri_string:parse(UrlStr) of
            #{scheme := Scheme, host := Host} = Parsed ->
                Port = maps:get(
                    port,
                    Parsed,
                    case Scheme of
                        <<"nats">> -> 4222;
                        <<"tls">> -> 4222;
                        _ -> 4222
                    end
                ),
                UserInfo = maps:get(userinfo, Parsed, undefined),
                {ok, #{
                    scheme => Scheme,
                    host => Host,
                    port => Port,
                    userinfo => UserInfo,
                    full_url => UrlBin
                }};
            _ ->
                {error, invalid_url_format}
        end
    catch
        _:_ ->
            {error, url_parse_failed}
    end.

%% @doc Get NATS authentication configuration
%% Returns authentication config map (stub-level helper)
-spec get_nats_auth_config() -> map().
get_nats_auth_config() ->
    Username = application:get_env(beamline_router, nats_username, undefined),
    Password = application:get_env(beamline_router, nats_password, undefined),
    JWT = application:get_env(beamline_router, nats_jwt, undefined),
    NKey = application:get_env(beamline_router, nats_nkey, undefined),

    #{
        username => Username,
        %% Never log password
        password => undefined,
        has_password => Password =/= undefined,
        jwt => JWT,
        nkey => NKey,
        auth_type => determine_auth_type(Username, Password, JWT, NKey)
    }.

%% @doc Determine authentication type from configuration
-spec determine_auth_type(term(), term(), term(), term()) -> atom().
determine_auth_type(_Username, _Password, JWT, _NKey) when JWT =/= undefined ->
    jwt;
determine_auth_type(_Username, _Password, _JWT, NKey) when NKey =/= undefined ->
    nkey;
determine_auth_type(Username, Password, _JWT, _NKey) when
    Username =/= undefined, Password =/= undefined
->
    username_password;
determine_auth_type(_Username, _Password, _JWT, _NKey) ->
    none.

%% @doc Get NATS TLS configuration
%% Returns TLS config map (stub-level helper)
-spec get_nats_tls_config() -> map().
get_nats_tls_config() ->
    TLSEnabled = application:get_env(beamline_router, nats_tls_enabled, false),
    TLSCertFile = application:get_env(beamline_router, nats_tls_cert_file, undefined),
    TLSKeyFile = application:get_env(beamline_router, nats_tls_key_file, undefined),
    TLSCACertFile = application:get_env(beamline_router, nats_tls_ca_cert_file, undefined),
    TLSVerify = application:get_env(beamline_router, nats_tls_verify, true),

    #{
        enabled => TLSEnabled,
        cert_file => TLSCertFile,
        key_file => TLSKeyFile,
        ca_cert_file => TLSCACertFile,
        verify => TLSVerify
    }.

%% @doc Sanitize URL for logging (remove credentials)
-spec sanitize_url_for_logging(binary() | list()) -> binary().
sanitize_url_for_logging(Url) when is_binary(Url) ->
    case binary:split(Url, <<"@">>) of
        [_UserInfo, Rest] ->
            <<"***@", Rest/binary>>;
        _ ->
            Url
    end;
sanitize_url_for_logging(Url) when is_list(Url) ->
    sanitize_url_for_logging(list_to_binary(Url));
sanitize_url_for_logging(_) ->
    <<"***">>.

%% @doc Create stub connection process with configuration
-spec create_stub_connection(map()) -> {ok, pid()}.
create_stub_connection(Config) ->
    StubPid = spawn_link(fun() -> 
        %% Mark this process as a stub connection
        erlang:put(stub_connection, true),
        stub_connection_loop(#{
            created_at => erlang:system_time(millisecond),
            message_count => 0,
            last_activity => erlang:system_time(millisecond),
            config => Config,
            connection_id => generate_connection_id()
        })
    end),
    {ok, StubPid}.

%% @doc Generate unique connection ID for stub connection
-spec generate_connection_id() -> binary().
generate_connection_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    Id =
        <<"stub-", (integer_to_binary(Timestamp))/binary, "-", (integer_to_binary(Random))/binary>>,
    Id.

%% @doc Stub connection process loop with state tracking
%% Simulates connection process behavior for testing
stub_connection_loop(State) ->
    receive
        {health_check, From} ->
            %% Respond to health checks
            HealthStatus = check_stub_connection_health(State),
            From ! {health_check_response, HealthStatus},
            stub_connection_loop(State);
        {get_state, From} ->
            From ! {state_response, State},
            stub_connection_loop(State);
        {get_config, From} ->
            Config = maps:get(config, State, #{}),
            From ! {config_response, Config},
            stub_connection_loop(State);
        {publish, Subject, Payload} ->
            %% Simulate publish operation
            NewState = maps:update_with(message_count, fun(C) -> C + 1 end, 1, State),
            NewState2 = maps:put(last_activity, erlang:system_time(millisecond), NewState),
            %% Track published messages (for testing/debugging)
            track_stub_message(publish, Subject, Payload, NewState2),
            stub_connection_loop(NewState2);
        {subscribe, Subject, _Options} ->
            %% Simulate subscribe operation
            NewState = maps:put(last_activity, erlang:system_time(millisecond), State),
            Subscriptions = maps:get(subscriptions, State, []),
            NewSubscriptions = [Subject | Subscriptions],
            NewState2 = maps:put(subscriptions, NewSubscriptions, NewState),
            stub_connection_loop(NewState2);
        {unsubscribe, Subject} ->
            %% Simulate unsubscribe operation
            NewState = maps:put(last_activity, erlang:system_time(millisecond), State),
            Subscriptions = maps:get(subscriptions, State, []),
            NewSubscriptions = lists:delete(Subject, Subscriptions),
            NewState2 = maps:put(subscriptions, NewSubscriptions, NewState),
            stub_connection_loop(NewState2);
        stop ->
            ok;
        _ ->
            %% Handle any other messages
            NewState = maps:put(last_activity, erlang:system_time(millisecond), State),
            stub_connection_loop(NewState)
    end.

%% @doc Check stub connection health
-spec check_stub_connection_health(map()) -> atom().
check_stub_connection_health(State) ->
    Now = erlang:system_time(millisecond),
    LastActivity = maps:get(last_activity, State, Now),
    Timeout = application:get_env(beamline_router, nats_stub_connection_timeout_ms, 60000),
    case (Now - LastActivity) > Timeout of
        true ->
            stale;
        false ->
            healthy
    end.

%% @doc Track stub message for testing/debugging
-spec track_stub_message(atom(), binary(), term(), map()) -> ok.
track_stub_message(Op, Subject, _Payload, State) ->
    %% Store message in ETS table for testing (if table exists)
    case ets:whereis(stub_nats_messages) of
        undefined ->
            ok;
        _Tid ->
            MessageId = generate_message_id(),
            ets:insert(
                stub_nats_messages, {MessageId, Op, Subject, erlang:system_time(millisecond), State}
            ),
            ok
    end.

%% @doc Generate unique message ID for stub messages
-spec generate_message_id() -> binary().
generate_message_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    <<"stub-msg-", (integer_to_binary(Timestamp))/binary, "-", (integer_to_binary(Random))/binary>>.

%% @doc Handle connection lost
handle_connection_lost(Reason, State) ->
    router_logger:error(<<"NATS connection lost">>, #{
        <<"reason">> => sanitize_error(Reason),
        <<"reconnect_attempts">> => State#state.reconnect_attempts,
        <<"error_code">> => <<"NATS_CONNECTION_LOST">>,
        <<"error_tag">> => <<"nats_connection_failure">>
    }),
    router_metrics:inc(router_nats_connection_lost_total),
    %% Update connection status gauge (0 = disconnected)
    router_metrics:emit_metric(router_nats_connection_status, #{value => 0}, #{
        state => disconnected
    }),

    %% Cancel any existing reconnect timer
    case State#state.reconnect_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    Now = erlang:system_time(millisecond),
    NewState = State#state{
        connection_state = ?CONN_STATE_DISCONNECTED,
        connection_pid = undefined,
        last_failure_time = Now
    },

    %% Schedule reconnect if attempts remaining
    case State#state.reconnect_attempts < State#state.max_reconnect_attempts of
        true ->
            schedule_reconnect(NewState);
        false ->
            router_logger:error(<<"NATS reconnection exhausted, switching to fail-open mode">>, #{
                <<"max_attempts">> => State#state.max_reconnect_attempts
            }),
            router_metrics:inc(router_nats_reconnection_exhausted_total),
            NewState#state{
                fail_open_mode = true,
                connection_state = ?CONN_STATE_DISCONNECTED
            }
    end.

%% @doc Handle connection restored
handle_connection_restored(Pid, State) ->
    router_logger:info(<<"NATS connection restored">>, #{
        <<"reconnect_attempts">> => State#state.reconnect_attempts,
        <<"error_code">> => <<"NATS_CONNECTION_RESTORED">>,
        <<"error_tag">> => <<"nats_connection_recovered">>
    }),
    router_metrics:inc(router_nats_connection_restored_total),
    %% Update connection status gauge (1 = connected)
    router_metrics:emit_metric(router_nats_connection_status, #{value => 1}, #{
        state => connected
    }),

    %% Cancel reconnect timer
    case State#state.reconnect_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    %% Monitor new connection
    erlang:monitor(process, Pid),

    Now = erlang:system_time(millisecond),
    FailOpenConfig = application:get_env(beamline_router, nats_fail_open_mode, false),
    NewState = State#state{
        connection_state = ?CONN_STATE_CONNECTED,
        connection_pid = Pid,
        reconnect_attempts = 0,
        fail_open_mode = FailOpenConfig,
        reconnect_timer = undefined,
        last_connection_time = Now,
        connection_health_checks = 0
    },

    %% Retry pending operations
    RetryState = retry_pending_operations(NewState),

    %% Resubscribe to JetStream consumers
    resubscribe_jetstream_consumers(RetryState).

%% @doc Handle reconnect attempt
handle_reconnect(State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            %% Already connected
            State;
        _ ->
            router_logger:info(<<"Attempting NATS reconnection">>, #{
                <<"attempt">> => State#state.reconnect_attempts + 1,
                <<"max_attempts">> => State#state.max_reconnect_attempts,
                <<"error_code">> => <<"NATS_RECONNECT_ATTEMPT">>,
                <<"error_tag">> => <<"nats_reconnecting">>
            }),
            router_metrics:inc(router_nats_reconnect_attempts_total),
            %% Update connection status gauge (0.5 = reconnecting)
            router_metrics:emit_metric(router_nats_connection_status, #{value => 0.5}, #{
                state => reconnecting
            }),

            NewState = State#state{
                connection_state = ?CONN_STATE_RECONNECTING,
                reconnect_attempts = State#state.reconnect_attempts + 1
            },

            case attempt_connection() of
                {ok, ConnectionPid} ->
                    erlang:monitor(process, ConnectionPid),
                    handle_connection_restored(ConnectionPid, NewState);
                {error, Reason} ->
                    router_logger:warn(<<"NATS reconnection attempt failed">>, #{
                        <<"attempt">> => NewState#state.reconnect_attempts,
                        <<"reason">> => sanitize_error(Reason)
                    }),
                    %% Emit reconnect failure metric with labels
                    ReasonBin = error_to_reason(Reason),
                    Cluster = get_nats_cluster(),
                    router_metrics:emit_metric(
                        router_nats_reconnect_failures_total, #{count => 1}, #{
                            reason => ReasonBin,
                            cluster => Cluster,
                            attempt => NewState#state.reconnect_attempts
                        }
                    ),
                    schedule_reconnect(NewState)
            end
    end.

%% @doc Schedule reconnect with exponential backoff
schedule_reconnect(State) ->
    Delay = calculate_reconnect_delay(State#state.reconnect_attempts),
    Timer = erlang:send_after(Delay, self(), reconnect),
    State#state{
        reconnect_timer = Timer,
        connection_state = ?CONN_STATE_RECONNECTING
    }.

%% @doc Calculate reconnect delay with exponential backoff
calculate_reconnect_delay(Attempt) ->
    BaseDelay = application:get_env(
        beamline_router, nats_reconnect_delay_ms, ?DEFAULT_RECONNECT_DELAY_MS
    ),
    MaxDelay = application:get_env(
        beamline_router, nats_max_reconnect_delay_ms, ?DEFAULT_MAX_RECONNECT_DELAY_MS
    ),
    Delay = min(BaseDelay * trunc(math:pow(2, Attempt)), MaxDelay),
    Delay.

%% @doc Do publish (with fail-open support, queueing, and retry logic)
do_publish(Subject, Payload, State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            %% Connected - attempt publish with retry logic
            RetryConfig = get_publish_retry_config(),
            PublishFun = fun(S, P) -> do_publish_internal(S, P, State#state.connection_pid) end,
            MetricsFun = fun(Attempt, MaxAttempts) ->
                %% Emit publish attempt metric with retry count
                Status =
                    case Attempt =:= MaxAttempts of
                        true -> <<"error">>;
                        false -> <<"success">>
                    end,
                router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
                    status => Status,
                    retry_count => integer_to_binary(Attempt - 1)
                })
            end,

            %% Use retry logic if configured, otherwise use simple publish
            case maps:get(<<"enabled">>, RetryConfig, false) of
                true ->
                    case
                        router_nats_publish_retry:publish_with_retry(
                            Subject, Payload, PublishFun, MetricsFun, State, RetryConfig
                        )
                    of
                        {ok, _Attempts} ->
                            router_metrics:inc(router_nats_publish_total),
                            ok;
                        {error, Reason} ->
                            router_logger:warn(<<"NATS publish failed after retries">>, #{
                                <<"subject">> => Subject,
                                <<"reason">> => sanitize_error(Reason),
                                <<"error_code">> => <<"NATS_PUBLISH_ERROR">>
                            }),
                            %% Emit publish failure metric with labels
                            ReasonBin = error_to_reason(Reason),
                            Stream = extract_stream_from_subject(Subject),
                            router_metrics:emit_metric(
                                router_nats_publish_failures_total, #{count => 1}, #{
                                    reason => ReasonBin,
                                    subject => Subject,
                                    stream => Stream,
                                    source => <<"publish">>
                                }
                            ),
                            %% Emit error type metric
                            ErrorType = classify_error_type(Reason),
                            router_metrics:emit_metric(
                                router_nats_publish_errors_total, #{count => 1}, #{
                                    error_type => ErrorType
                                }
                            ),
                            %% In fail-open mode, return ok even on error
                            case State#state.fail_open_mode of
                                true -> ok;
                                false -> {error, Reason}
                            end
                    end;
                false ->
                    %% Simple publish without retry (backward compatible)
                    PublishStartTime = erlang:system_time(microsecond),
                    case do_publish_internal(Subject, Payload, State#state.connection_pid) of
                        ok ->
                            PublishLatency =
                                (erlang:system_time(microsecond) - PublishStartTime) / 1000.0,
                            router_metrics:inc(router_nats_publish_total),
                            router_metrics:emit_metric(
                                router_nats_publish_attempts_total, #{count => 1}, #{
                                    status => <<"success">>,
                                    retry_count => <<"0">>
                                }
                            ),
                            router_metrics:emit_metric(
                                router_nats_publish_latency_seconds,
                                #{value => PublishLatency / 1000.0},
                                #{}
                            ),
                            ok;
                        {error, Reason} ->
                            PublishLatency =
                                (erlang:system_time(microsecond) - PublishStartTime) / 1000.0,
                            router_logger:warn(<<"NATS publish failed">>, #{
                                <<"subject">> => Subject,
                                <<"reason">> => sanitize_error(Reason),
                                <<"error_code">> => <<"NATS_PUBLISH_ERROR">>
                            }),
                            router_metrics:emit_metric(
                                router_nats_publish_attempts_total, #{count => 1}, #{
                                    status => <<"error">>,
                                    retry_count => <<"0">>
                                }
                            ),
                            router_metrics:emit_metric(
                                router_nats_publish_latency_seconds,
                                #{value => PublishLatency / 1000.0},
                                #{}
                            ),
                            %% Emit publish failure metric with labels
                            ReasonBin = error_to_reason(Reason),
                            Stream = extract_stream_from_subject(Subject),
                            router_metrics:emit_metric(
                                router_nats_publish_failures_total, #{count => 1}, #{
                                    reason => ReasonBin,
                                    subject => Subject,
                                    stream => Stream,
                                    source => <<"publish">>
                                }
                            ),
                            ErrorType = classify_error_type(Reason),
                            router_metrics:emit_metric(
                                router_nats_publish_errors_total, #{count => 1}, #{
                                    error_type => ErrorType
                                }
                            ),
                            %% In fail-open mode, return ok even on error
                            case State#state.fail_open_mode of
                                true ->
                                    ok;
                                false ->
                                    %% Return error (caller will handle queueing if needed)
                                    {error, Reason}
                            end
                    end
            end;
        _ ->
            %% Not connected - queue operation for retry after reconnect
            router_logger:warn(<<"NATS publish queued: not connected">>, #{
                <<"subject">> => Subject,
                <<"connection_state">> => State#state.connection_state,
                <<"error_code">> => <<"NATS_PUBLISH_QUEUED">>
            }),
            %% Emit publish failure metric with labels
            Stream = extract_stream_from_subject(Subject),
            router_metrics:emit_metric(router_nats_publish_failures_total, #{count => 1}, #{
                reason => <<"not_connected">>,
                subject => Subject,
                stream => Stream,
                source => <<"publish">>
            }),
            case State#state.fail_open_mode of
                true ->
                    %% Fail-open: return ok, don't queue
                    ok;
                false ->
                    %% Queue for retry after reconnect (caller will update state)
                    {error, not_connected}
            end
    end.

% %% @doc Do publish internal (stub implementation with fault injection)
% do_publish_internal(Subject, Payload, ConnectionPid) ->
%     %% Check for fault injection
%     case code:is_loaded(router_nats_fault_injection) of
%         {file, _} ->
%             case
%                 router_nats_fault_injection:should_fail(publish, #{
%                     subject => Subject, payload => Payload
%                 })
%             of
%                 {true, {error, Reason}} ->
%                     {error, Reason};
%                 {true, timeout} ->
%                     %% Simulate timeout by sleeping longer than operation timeout
%                     timer:sleep(10000),
%                     {error, timeout};
%                 {true, close_connection} when is_pid(ConnectionPid) ->
%                     %% Close connection
%                     exit(ConnectionPid, normal),
%                     {error, connection_closed};
%                 {true, {delay, Ms}} ->
%                     timer:sleep(Ms),
%                     ok;
%                 false ->
%                     ok
%             end;
%         false ->
%             ok
%     end.

%% @doc Do publish internal (real NATS implementation)
do_publish_internal(Subject, Payload, ConnectionPid) ->
    %% Check if connection is a stub process
    case is_pid(ConnectionPid) andalso is_process_alive(ConnectionPid) of
        true ->
            case process_info(ConnectionPid, dictionary) of
                {dictionary, Dict} ->
                    case lists:keyfind(stub_connection, 1, Dict) of
                        {stub_connection, true} ->
                            %% Stub mode - return success
                            ok;
                        _ ->
                            %% Real NATS connection
                            case nats:pub(ConnectionPid, Subject, Payload) of
                                ok -> ok;
                                {error, Reason} -> {error, Reason}
                            end
                    end;
                undefined ->
                    %% No dictionary info, try real NATS
                    case nats:pub(ConnectionPid, Subject, Payload) of
                        ok -> ok;
                        {error, Reason} -> {error, Reason}
                    end
            end;
        false ->
            {error, invalid_connection}
    end.

%% @doc Do publish with ack (with fail-open support and queueing)
do_publish_with_ack(Subject, Payload, Headers, State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            case
                do_publish_with_ack_internal(Subject, Payload, Headers, State#state.connection_pid)
            of
                {ok, MsgId} ->
                    router_metrics:inc(router_nats_publish_with_ack_total),
                    {ok, MsgId};
                {error, Reason} ->
                    router_logger:warn(<<"NATS publish_with_ack failed">>, #{
                        <<"subject">> => Subject,
                        <<"reason">> => sanitize_error(Reason),
                        <<"error_code">> => <<"NATS_PUBLISH_WITH_ACK_ERROR">>
                    }),
                    %% Emit publish_with_ack failure metric with labels
                    ReasonBin = error_to_reason(Reason),
                    Stream = extract_stream_from_subject(Subject),
                    router_metrics:emit_metric(
                        router_nats_publish_with_ack_failures_total, #{count => 1}, #{
                            reason => ReasonBin,
                            subject => Subject,
                            stream => Stream,
                            source => <<"publish_with_ack">>
                        }
                    ),
                    case State#state.fail_open_mode of
                        %% Fail-open: return stub ID
                        true ->
                            {ok, <<"stub-msg-id">>};
                        false ->
                            %% Return error (caller will handle queueing if needed)
                            {error, Reason}
                    end
            end;
        _ ->
            router_logger:warn(<<"NATS publish_with_ack queued: not connected">>, #{
                <<"subject">> => Subject,
                <<"error_code">> => <<"NATS_PUBLISH_WITH_ACK_QUEUED">>
            }),
            %% Emit publish_with_ack failure metric with labels
            Stream = extract_stream_from_subject(Subject),
            router_metrics:emit_metric(
                router_nats_publish_with_ack_failures_total, #{count => 1}, #{
                    reason => <<"not_connected">>,
                    subject => Subject,
                    stream => Stream,
                    source => <<"publish_with_ack">>
                }
            ),
            case State#state.fail_open_mode of
                true ->
                    {ok, <<"stub-msg-id">>};
                false ->
                    %% Queue for retry after reconnect (return error, caller will update state)
                    {error, not_connected}
            end
    end.

%% @doc Do publish with ack internal (stub with fault injection)
% do_publish_with_ack_internal(Subject, Payload, _Headers, ConnectionPid) ->
%     %% Check for fault injection
%     case code:is_loaded(router_nats_fault_injection) of
%         {file, _} ->
%             case
%                 router_nats_fault_injection:should_fail(publish_with_ack, #{
%                     subject => Subject, payload => Payload
%                 })
%             of
%                 {true, {error, Reason}} ->
%                     {error, Reason};
%                 {true, timeout} ->
%                     timer:sleep(10000),
%                     {error, timeout};
%                 {true, close_connection} when is_pid(ConnectionPid) ->
%                     exit(ConnectionPid, normal),
%                     {error, connection_closed};
%                 {true, {delay, Ms}} ->
%                     timer:sleep(Ms),
%                     {ok, <<"stub-msg-id">>};
%                 false ->
%                     {ok, <<"stub-msg-id">>}
%             end;
%         false ->
%             {ok, <<"stub-msg-id">>}
%     end.

%% @doc Do publish with ack internal (real NATS implementation)
do_publish_with_ack_internal(Subject, Payload, Headers, ConnectionPid) ->
    %% Check if connection is a stub process
    case is_pid(ConnectionPid) andalso is_process_alive(ConnectionPid) of
        true ->
            case process_info(ConnectionPid, dictionary) of
                {dictionary, Dict} ->
                    case lists:keyfind(stub_connection, 1, Dict) of
                        {stub_connection, true} ->
                            %% Stub mode - return stub message ID
                            {ok, generate_message_id()};
                        _ ->
                            %% Real NATS connection
                            NATSHeaders = case Headers of
                                undefined -> #{};
                                _ -> Headers
                            end,
                            case nats:pub(ConnectionPid, Subject, Payload, #{headers => NATSHeaders}) of
                                ok -> {ok, generate_message_id()};
                                {error, Reason} -> {error, Reason}
                            end
                    end;
                undefined ->
                    %% No dictionary info, try real NATS
                    NATSHeaders = case Headers of
                        undefined -> #{};
                        _ -> Headers
                    end,
                    case nats:pub(ConnectionPid, Subject, Payload, #{headers => NATSHeaders}) of
                        ok -> {ok, generate_message_id()};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        false ->
            {error, invalid_connection}
    end.

%% @doc Do ack message
do_ack_message(MsgId, State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            case do_ack_message_internal(MsgId, State#state.connection_pid) of
                ok ->
                    router_metrics:inc(router_nats_ack_total),
                    ok;
                {error, Reason} ->
                    router_logger:warn(<<"NATS ack failed">>, #{
                        <<"msg_id">> => MsgId,
                        <<"reason">> => sanitize_error(Reason)
                    }),
                    %% Emit ACK failure metric with labels
                    ReasonBin = error_to_reason(Reason),
                    %% Extract subject/stream/consumer from MsgId context
                    Context = extract_nats_context_from_msgid(MsgId),
                    router_metrics:emit_metric(router_nats_ack_failures_total, #{count => 1}, #{
                        reason => ReasonBin,
                        subject => maps:get(<<"subject">>, Context, <<"unknown">>),
                        stream => maps:get(<<"stream">>, Context, <<"unknown">>),
                        consumer => maps:get(<<"consumer">>, Context, <<"unknown">>)
                    }),
                    {error, Reason}
            end;
        _ ->
            router_logger:warn(<<"NATS ack failed: not connected">>, #{
                <<"msg_id">> => MsgId
            }),
            %% Emit ACK failure metric with labels
            Context = extract_nats_context_from_msgid(MsgId),
            router_metrics:emit_metric(router_nats_ack_failures_total, #{count => 1}, #{
                reason => <<"not_connected">>,
                subject => maps:get(subject, Context, <<"unknown">>),
                stream => maps:get(stream, Context, <<"unknown">>),
                consumer => maps:get(consumer, Context, <<"unknown">>)
            }),
            {error, not_connected}
    end.

%% @doc Do ack message internal (stub with fault injection)
do_ack_message_internal(MsgId, ConnectionPid) ->
    %% Check for fault injection
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            case router_nats_fault_injection:should_fail(ack, #{msg_id => MsgId}) of
                {true, {error, Reason}} ->
                    {error, Reason};
                {true, timeout} ->
                    timer:sleep(10000),
                    {error, timeout};
                {true, close_connection} when is_pid(ConnectionPid) ->
                    exit(ConnectionPid, normal),
                    {error, connection_closed};
                {true, {delay, Ms}} ->
                    timer:sleep(Ms),
                    ok;
                false ->
                    ok
            end;
        false ->
            ok
    end.

%% @doc Do nak message
do_nak_message(MsgId, State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            case do_nak_message_internal(MsgId, State#state.connection_pid) of
                ok ->
                    router_metrics:inc(router_nats_nak_total),
                    ok;
                {error, Reason} ->
                    router_logger:warn(<<"NATS nak failed">>, #{
                        <<"msg_id">> => MsgId,
                        <<"reason">> => sanitize_error(Reason)
                    }),
                    %% Emit NAK failure metric with labels
                    ReasonBin = error_to_reason(Reason),
                    Context = extract_nats_context_from_msgid(MsgId),
                    router_metrics:emit_metric(router_nats_nak_failures_total, #{count => 1}, #{
                        reason => ReasonBin,
                        subject => maps:get(subject, Context, <<"unknown">>),
                        stream => maps:get(stream, Context, <<"unknown">>)
                    }),
                    {error, Reason}
            end;
        _ ->
            router_logger:warn(<<"NATS nak failed: not connected">>, #{
                <<"msg_id">> => MsgId
            }),
            %% Emit NAK failure metric with labels
            Context = extract_nats_context_from_msgid(MsgId),
            router_metrics:emit_metric(router_nats_nak_failures_total, #{count => 1}, #{
                reason => <<"not_connected">>,
                subject => maps:get(subject, Context, <<"unknown">>),
                stream => maps:get(stream, Context, <<"unknown">>)
            }),
            {error, not_connected}
    end.

%% @doc Do nak message internal (stub)
do_nak_message_internal(MsgId, _ConnectionPid) ->
    %% Check fault injection for NAK
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            case router_nats_fault_injection:should_fail(nak, #{msg_id => MsgId}) of
                {true, {error, Reason}} ->
                    {error, Reason};
                {true, timeout} ->
                    timer:sleep(10000),
                    {error, timeout};
                {true, close_connection} ->
                    {error, connection_closed};
                {true, {delay, Ms}} ->
                    timer:sleep(Ms),
                    ok;
                false ->
                    ok
            end;
        false ->
            %% ⚠️ STUB IMPLEMENTATION: NAK not implemented for real NATS
            %% CP3/Release: Implement actual NATS NAK via NATS client library
            %%
            %% Implementation requirements:
            %% - Requires actual NATS connection (not stub)
            %% - Send NAK message to JetStream consumer for message redelivery
            %% - Support delay parameter for redelivery backoff (exponential backoff)
            %% - Handle errors gracefully (connection lost, consumer not found, etc.)
            %% - Emit telemetry events for NAK operations (router_nats_nak_total)
            %% - Log NAK operations with context (msg_id, subject, delay_ms)
            %%
            %% Current stub behavior:
            %% - Returns ok (no-op) when not in fault injection mode
            %% - Safe for testing but messages won't be NAK'd in production
            %% - Messages will be redelivered by JetStream after AckWait timeout
            router_logger:debug(<<"NATS NAK: stub mode (not implemented)">>, #{
                <<"msg_id">> => MsgId
            }),
            ok
    end.

%% @doc Do subscribe jetstream
do_subscribe_jetstream(Subject, DurableGroup, AckPolicy, DeliverGroup, Mode, State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            case
                do_subscribe_jetstream_internal(
                    Subject, DurableGroup, AckPolicy, DeliverGroup, Mode, State#state.connection_pid
                )
            of
                {ok, ConsumerId} ->
                    router_logger:info(<<"JetStream subscription created">>, #{
                        <<"subject">> => Subject,
                        <<"durable_group">> => DurableGroup,
                        <<"consumer_id">> => ConsumerId
                    }),
                    router_metrics:inc(router_nats_subscribe_total),
                    %% Register consumer in state
                    NewState = register_jetstream_consumer(
                        ConsumerId, Subject, DurableGroup, AckPolicy, DeliverGroup, Mode, State
                    ),
                    {ok, ConsumerId, NewState};
                {error, Reason} ->
                    router_logger:error(<<"JetStream subscription failed">>, #{
                        <<"subject">> => Subject,
                        <<"reason">> => sanitize_error(Reason)
                    }),
                    %% Emit subscribe failure metric with labels
                    ReasonBin = error_to_reason(Reason),
                    Stream = extract_stream_from_subject(Subject),
                    router_metrics:emit_metric(
                        router_nats_subscribe_failures_total, #{count => 1}, #{
                            reason => ReasonBin,
                            subject => Subject,
                            stream => Stream,
                            consumer => DurableGroup
                        }
                    ),
                    {error, Reason}
            end;
        _ ->
            router_logger:error(<<"JetStream subscription failed: not connected">>, #{
                <<"subject">> => Subject
            }),
            %% Emit subscribe failure metric with labels
            Stream = extract_stream_from_subject(Subject),
            router_metrics:emit_metric(router_nats_subscribe_failures_total, #{count => 1}, #{
                reason => <<"not_connected">>,
                subject => Subject,
                stream => Stream,
                consumer => DurableGroup
            }),
            {error, not_connected}
    end.

%% @doc Do subscribe jetstream internal (stub with fault injection)
do_subscribe_jetstream_internal(
    Subject, DurableGroup, _AckPolicy, _DeliverGroup, _Mode, ConnectionPid
) ->
    %% Check for fault injection
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            case
                router_nats_fault_injection:should_fail(subscribe, #{
                    subject => Subject, durable => DurableGroup
                })
            of
                {true, {error, Reason}} ->
                    {error, Reason};
                {true, timeout} ->
                    timer:sleep(10000),
                    {error, timeout};
                {true, close_connection} when is_pid(ConnectionPid) ->
                    exit(ConnectionPid, normal),
                    {error, connection_closed};
                {true, {delay, Ms}} ->
                    timer:sleep(Ms),
                    {ok, <<"dummy-consumer">>};
                false ->
                    {ok, <<"dummy-consumer">>}
            end;
        false ->
            {ok, <<"dummy-consumer">>}
    end.

%% @doc Queue operation for retry after reconnect
%% Operations are stored with timestamp for timeout handling
queue_operation(Operation, State) ->
    Pending = State#state.pending_operations,
    %% Limit queue size to prevent memory issues
    MaxPending = application:get_env(beamline_router, nats_max_pending_operations, 1000),
    OperationTimeout = application:get_env(
        beamline_router, nats_pending_operation_timeout_ms, 60000
    ),
    Now = erlang:system_time(millisecond),

    %% Remove expired operations
    ValidPending = lists:filter(
        fun(Op) ->
            case Op of
                {_OpType, _Args, Timestamp} when is_integer(Timestamp) ->
                    (Now - Timestamp) < OperationTimeout;
                _ ->
                    %% Legacy format without timestamp
                    true
            end
        end,
        Pending
    ),

    case length(ValidPending) >= MaxPending of
        true ->
            router_logger:warn(<<"NATS pending operations queue full, dropping oldest">>, #{
                <<"max_pending">> => MaxPending,
                <<"error_code">> => <<"NATS_QUEUE_FULL">>
            }),
            router_metrics:inc(router_nats_pending_operations_dropped_total),
            %% Drop oldest (last in list) and add new with timestamp
            Dropped = lists:droplast(ValidPending),
            NewPending = [{Operation, Now} | Dropped];
        false ->
            %% Add new operation with timestamp
            NewPending = [{Operation, Now} | ValidPending]
    end,
    router_metrics:emit_metric(
        router_nats_pending_operations_count, #{value => length(NewPending)}, #{}
    ),
    State#state{pending_operations = NewPending}.

%% @doc Retry pending operations after reconnect
retry_pending_operations(State) ->
    PendingCount = length(State#state.pending_operations),
    case PendingCount > 0 of
        true ->
            router_logger:info(<<"Retrying pending NATS operations after reconnect">>, #{
                <<"count">> => PendingCount,
                <<"error_code">> => <<"NATS_RETRY_PENDING">>
            }),
            router_metrics:emit_metric(
                router_nats_pending_operations_retry, #{count => PendingCount}, #{}
            ),

            %% Retry operations in reverse order (FIFO - oldest first)
            %% Extract operations from {Operation, Timestamp} tuples
            Operations = lists:map(
                fun(Op) ->
                    case Op of
                        {Operation, _Timestamp} -> Operation;
                        %% Legacy format
                        Operation -> Operation
                    end
                end,
                State#state.pending_operations
            ),
            ReversedOps = lists:reverse(Operations),
            {Successful, Failed, NewStateAfter} = lists:foldl(
                fun(Operation, {Succ, Fail, AccState}) ->
                    case Operation of
                        {publish, Subject, Payload} ->
                            case do_publish(Subject, Payload, AccState) of
                                ok -> {[Operation | Succ], Fail, AccState};
                                _ -> {Succ, [Operation | Fail], AccState}
                            end;
                        {publish_with_ack, Subject, Payload, Headers} ->
                            case do_publish_with_ack(Subject, Payload, Headers, AccState) of
                                {ok, _} -> {[Operation | Succ], Fail, AccState};
                                _ -> {Succ, [Operation | Fail], AccState}
                            end;
                        _ ->
                            {Succ, [Operation | Fail], AccState}
                    end
                end,
                {[], [], State},
                ReversedOps
            ),

            SuccessCount = length(Successful),
            FailCount = length(Failed),

            router_logger:info(<<"Pending operations retry completed">>, #{
                <<"total">> => PendingCount,
                <<"successful">> => SuccessCount,
                <<"failed">> => FailCount,
                <<"error_code">> => <<"NATS_RETRY_COMPLETE">>
            }),

            router_metrics:emit_metric(
                router_nats_pending_operations_retry_success, #{count => SuccessCount}, #{}
            ),
            router_metrics:emit_metric(
                router_nats_pending_operations_retry_failed, #{count => FailCount}, #{}
            ),

            %% Clear pending operations (even if some failed - they won't be retried again)
            NewStateAfter#state{pending_operations = []};
        false ->
            State
    end.

%% @doc Sanitize error for logging
sanitize_error(Error) ->
    case is_binary(Error) of
        true ->
            Error;
        false ->
            ErrorBin = iolist_to_binary(io_lib:format("~p", [Error])),
            %% Check for secrets
            case re:run(ErrorBin, "(?i)(api[_-]?key|secret|token|password)", [{capture, none}]) of
                match -> <<"[REDACTED]">>;
                nomatch -> ErrorBin
            end
    end.

%% Internal: Build DLQ subject (same logic as router_jetstream)
build_dlq_subject(Subject) ->
    case application:get_env(beamline_router, dlq_subject_pattern, undefined) of
        undefined ->
            <<Subject/binary, ".dlq">>;
        Pattern when is_binary(Pattern) ->
            Pattern;
        Pattern when is_list(Pattern) ->
            list_to_binary(Pattern);
        _ ->
            <<Subject/binary, ".dlq">>
    end.

%% @doc Convert error to reason label (binary)
%% Normalizes various error types to standardized reason values
error_to_reason(Error) when is_binary(Error) ->
    Error;
error_to_reason(timeout) ->
    <<"timeout">>;
error_to_reason(connection_closed) ->
    <<"connection_closed">>;
error_to_reason(connection_refused) ->
    <<"connection_refused">>;
error_to_reason(auth_failed) ->
    <<"auth_failed">>;
error_to_reason(not_connected) ->
    <<"not_connected">>;
error_to_reason(no_route) ->
    <<"no_route">>;
error_to_reason(stream_not_found) ->
    <<"stream_not_found">>;
error_to_reason(consumer_not_found) ->
    <<"consumer_not_found">>;
error_to_reason(authorization) ->
    <<"authorization">>;
error_to_reason(Error) when is_atom(Error) ->
    atom_to_binary(Error, utf8);
error_to_reason(Error) ->
    %% Fallback: convert to binary
    iolist_to_binary(io_lib:format("~p", [Error])).

%% @doc Get publish retry configuration from application environment
get_publish_retry_config() ->
    case application:get_env(beamline_router, publish_retry_enabled, false) of
        true ->
            DefaultConfig = router_nats_publish_retry:get_default_config(),
            maps:merge(DefaultConfig, #{
                <<"enabled">> => true,
                <<"max_attempts">> => application:get_env(
                    beamline_router, publish_retry_max_attempts, 3
                ),
                <<"backoff_strategy">> => application:get_env(
                    beamline_router, publish_retry_backoff_strategy, exponential
                ),
                <<"backoff_base_ms">> => application:get_env(
                    beamline_router, publish_retry_backoff_base_ms, 100
                ),
                <<"backoff_max_ms">> => application:get_env(
                    beamline_router, publish_retry_backoff_max_ms, 5000
                ),
                <<"jitter_percent">> => application:get_env(
                    beamline_router, publish_retry_jitter_percent, 20
                ),
                <<"timeout_per_attempt_ms">> => application:get_env(
                    beamline_router, publish_retry_timeout_per_attempt_ms, 2000
                ),
                <<"total_deadline_ms">> => application:get_env(
                    beamline_router, publish_retry_total_deadline_ms, 10000
                )
            });
        false ->
            #{<<"enabled">> => false}
    end.

%% @doc Classify error type for metrics
classify_error_type(Reason) ->
    case Reason of
        timeout ->
            <<"timeout">>;
        connection_refused ->
            <<"connection">>;
        connection_closed ->
            <<"connection">>;
        nats_unavailable ->
            <<"connection">>;
        not_connected ->
            <<"connection">>;
        {error, timeout} ->
            <<"timeout">>;
        {error, connection_refused} ->
            <<"connection">>;
        {error, connection_closed} ->
            <<"connection">>;
        {error, nats_unavailable} ->
            <<"connection">>;
        {error, not_connected} ->
            <<"connection">>;
        _ ->
            ReasonBin = error_to_reason(Reason),
            case binary:match(ReasonBin, [<<"nack">>, <<"NAK">>]) of
                nomatch ->
                    case
                        binary:match(ReasonBin, [<<"5">>, <<"server_error">>, <<"unavailable">>])
                    of
                        nomatch -> <<"unknown">>;
                        _ -> <<"broker_error">>
                    end;
                _ ->
                    <<"nack">>
            end
    end.

%% @doc Extract NATS context (subject, stream, consumer) from MsgId
%% Attempts to extract context from ETS table or returns defaults
-spec extract_nats_context_from_msgid(binary() | undefined) -> map().
extract_nats_context_from_msgid(undefined) ->
    get_default_nats_context();
extract_nats_context_from_msgid(MsgId) when is_binary(MsgId) ->
    %% Try to get context from ETS table (if msg_context_table exists)
    case ets:whereis(msg_context_table) of
        undefined ->
            %% Table doesn't exist, return defaults
            get_default_nats_context();
        _Tid ->
            %% Table exists, try to lookup context
            case ets:lookup(msg_context_table, MsgId) of
                [] ->
                    %% No context found, return defaults
                    get_default_nats_context();
                [{MsgId, Context}] when is_map(Context) ->
                    %% Context found, extract subject/stream/consumer
                    #{
                        subject => maps:get(subject, Context, <<"unknown">>),
                        stream => maps:get(stream, Context, <<"unknown">>),
                        consumer => maps:get(consumer, Context, <<"unknown">>)
                    };
                _ ->
                    %% Invalid context format, return defaults
                    get_default_nats_context()
            end
    end.

%% @doc Get default NATS context when extraction fails
-spec get_default_nats_context() -> map().
get_default_nats_context() ->
    #{
        subject => <<"unknown">>,
        stream => <<"unknown">>,
        consumer => <<"unknown">>
    }.

%% @doc Extract stream name from subject
%% Attempts to infer stream name from subject pattern
%% For JetStream subjects like "beamline.router.v1.decide", stream might be "router-stream"
extract_stream_from_subject(Subject) when is_binary(Subject) ->
    %% Try to extract stream from subject pattern
    %% Common patterns:
    %% - "beamline.router.v1.decide" -> "router-stream"
    %% - "caf.exec.result.v1" -> "caf-stream"
    case binary:split(Subject, <<".">>, [global]) of
        [<<"beamline">>, StreamName, _, _] ->
            <<StreamName/binary, "-stream">>;
        [StreamName, <<"exec">>, _, _] ->
            <<StreamName/binary, "-stream">>;
        _ ->
            %% Fallback: use first part of subject
            case binary:split(Subject, <<".">>) of
                [FirstPart, _] -> FirstPart;
                [FirstPart] -> FirstPart;
                _ -> <<"unknown">>
            end
    end;
extract_stream_from_subject(_) ->
    <<"unknown">>.

%% @doc Get NATS cluster name from configuration
%% Returns cluster name from application environment or default value
%% @returns binary() - Cluster name (default: <<"default">>)
get_nats_cluster() ->
    case application:get_env(beamline_router, nats_cluster, undefined) of
        undefined -> <<"default">>;
        Cluster when is_binary(Cluster) -> Cluster;
        Cluster when is_list(Cluster) -> list_to_binary(Cluster);
        Cluster when is_atom(Cluster) -> atom_to_binary(Cluster, utf8);
        _ -> <<"default">>
    end.

%% @doc Build comprehensive connection status map
build_connection_status(State) ->
    #{
        state => State#state.connection_state,
        reconnect_attempts => State#state.reconnect_attempts,
        max_reconnect_attempts => State#state.max_reconnect_attempts,
        fail_open_mode => State#state.fail_open_mode,
        pending_operations_count => length(State#state.pending_operations),
        last_connection_time => State#state.last_connection_time,
        last_failure_time => State#state.last_failure_time,
        connection_health_checks => State#state.connection_health_checks,
        jetstream_consumers_count => maps:size(State#state.jetstream_consumers)
    }.

%% @doc Check connection health
check_connection_health(State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            case State#state.connection_pid of
                undefined ->
                    {error, no_connection_pid};
                Pid when is_pid(Pid) ->
                    case is_process_alive(Pid) of
                        true ->
                            {ok, healthy};
                        false ->
                            {error, connection_process_dead}
                    end
            end;
        ?CONN_STATE_DISCONNECTED ->
            {error, disconnected};
        ?CONN_STATE_RECONNECTING ->
            {error, reconnecting};
        ?CONN_STATE_CONNECTING ->
            {error, connecting};
        _ ->
            {error, unknown_state}
    end.

%% @doc Perform periodic health check
perform_health_check(State) ->
    case State#state.connection_state of
        ?CONN_STATE_CONNECTED ->
            case check_connection_health(State) of
                {ok, healthy} ->
                    NewHealthChecks = State#state.connection_health_checks + 1,
                    router_metrics:emit_metric(router_nats_health_check_total, #{count => 1}, #{
                        status => <<"healthy">>
                    }),
                    State#state{connection_health_checks = NewHealthChecks};
                {error, Reason} ->
                    router_logger:warn(<<"NATS connection health check failed">>, #{
                        <<"reason">> => Reason
                    }),
                    router_metrics:emit_metric(router_nats_health_check_total, #{count => 1}, #{
                        status => <<"unhealthy">>
                    }),
                    %% Trigger reconnection if health check fails
                    handle_connection_lost(health_check_failed, State)
            end;
        _ ->
            %% Not connected, skip health check
            State
    end.

%% @doc Register JetStream consumer in state
register_jetstream_consumer(
    ConsumerId, Subject, DurableGroup, AckPolicy, DeliverGroup, Mode, State
) ->
    Now = erlang:system_time(millisecond),
    Consumer = #jetstream_consumer{
        consumer_id = ConsumerId,
        subject = Subject,
        durable_group = DurableGroup,
        ack_policy = AckPolicy,
        deliver_group = DeliverGroup,
        mode = Mode,
        created_at = Now,
        last_message_time = undefined,
        message_count = 0
    },
    Consumers = State#state.jetstream_consumers,
    NewConsumers = maps:put(ConsumerId, Consumer, Consumers),
    State#state{jetstream_consumers = NewConsumers}.

%% @doc Unregister JetStream consumer from state
unregister_jetstream_consumer(ConsumerId, State) ->
    Consumers = State#state.jetstream_consumers,
    NewConsumers = maps:remove(ConsumerId, Consumers),
    State#state{jetstream_consumers = NewConsumers}.

%% @doc Resubscribe to all JetStream consumers after reconnection
resubscribe_jetstream_consumers(State) ->
    Consumers = maps:values(State#state.jetstream_consumers),
    lists:foldl(
        fun(Consumer, AccState) ->
            #jetstream_consumer{
                consumer_id = ConsumerId,
                subject = Subject,
                durable_group = DurableGroup,
                ack_policy = AckPolicy,
                deliver_group = DeliverGroup,
                mode = Mode
            } = Consumer,
            %% Attempt to resubscribe
            case
                do_subscribe_jetstream(
                    Subject, DurableGroup, AckPolicy, DeliverGroup, Mode, AccState
                )
            of
                {ok, NewConsumerId, NewState} ->
                    router_logger:info(<<"JetStream consumer resubscribed after reconnection">>, #{
                        <<"old_consumer_id">> => ConsumerId,
                        <<"new_consumer_id">> => NewConsumerId,
                        <<"subject">> => Subject
                    }),
                    NewState;
                {error, Reason} ->
                    router_logger:warn(<<"JetStream consumer resubscription failed">>, #{
                        <<"consumer_id">> => ConsumerId,
                        <<"subject">> => Subject,
                        <<"reason">> => sanitize_error(Reason)
                    }),
                    AccState
            end
        end,
        State,
        Consumers
    ).
