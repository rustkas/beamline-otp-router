-module(router_jetstream_consumer_manager).

-doc "JetStream Consumer Manager".
%%
%% Manages JetStream consumers for decide, results, and acks.
%% Handles subscription lifecycle and message processing.
%%
%% @module router_jetstream_consumer_manager

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([subscribe_decide/1, subscribe_results/1, subscribe_acks/1]).

-include("beamline_router.hrl").

-record(state, {
    decide_consumer = undefined :: pid() | undefined,
    results_consumer = undefined :: pid() | undefined,
    acks_consumer = undefined :: pid() | undefined
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Setup JetStream consumers if enabled
    State = case application:get_env(beamline_router, jetstream_enabled, false) of
        true ->
            setup_consumers(#state{});
        false ->
            #state{}
    end,
    {ok, State}.

subscribe_decide(Opts) ->
    gen_server:call(?MODULE, {subscribe_decide, Opts}).

subscribe_results(Opts) ->
    gen_server:call(?MODULE, {subscribe_results, Opts}).

subscribe_acks(Opts) ->
    gen_server:call(?MODULE, {subscribe_acks, Opts}).

handle_call({subscribe_decide, Opts}, _From, State) ->
    case subscribe_to_subject(decide, Opts, State) of
        {ok, ConsumerPid, NewState} ->
            {reply, {ok, ConsumerPid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({subscribe_results, Opts}, _From, State) ->
    case subscribe_to_subject(results, Opts, State) of
        {ok, ConsumerPid, NewState} ->
            {reply, {ok, ConsumerPid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({subscribe_acks, Opts}, _From, State) ->
    case subscribe_to_subject(acks, Opts, State) of
        {ok, ConsumerPid, NewState} ->
            {reply, {ok, ConsumerPid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

setup_consumers(State) ->
    %% Subscribe to decide subject
    DecideOpts = get_consumer_config(decide),
    ResultsOpts = get_consumer_config(results),
    AcksOpts = get_consumer_config(acks),

    case setup_consumer(decide, DecideOpts, State) of
        {ok, DecidePid, State1} ->
            case setup_consumer(results, ResultsOpts, State1) of
                {ok, ResultsPid, State2} ->
                    case setup_consumer(acks, AcksOpts, State2) of
                        {ok, AcksPid, FinalState} ->
                            router_logger:info(~"JetStream consumers setup completed", #{
                                decide_consumer => DecidePid,
                                results_consumer => ResultsPid,
                                acks_consumer => AcksPid
                            }),
                            FinalState;
                        {error, Reason} ->
                            router_logger:error(~"Failed to setup acks consumer", #{
                                reason => Reason
                            }),
                            State2
                    end;
                {error, Reason} ->
                    router_logger:error(~"Failed to setup results consumer", #{
                        reason => Reason
                    }),
                    State1
            end;
        {error, Reason} ->
            router_logger:error(~"Failed to setup decide consumer", #{
                reason => Reason
            }),
            State
    end.

setup_consumer(Type, Opts, State) ->
    Subject = maps:get(subject, Opts),
    DurableGroup = maps:get(durable_group, Opts),
    AckPolicy = maps:get(ack_policy, Opts),
    DeliverGroup = maps:get(deliver_group, Opts),
    Mode = maps:get(mode, Opts),

    case router_nats:subscribe_jetstream(Subject, DurableGroup, AckPolicy, DeliverGroup, Mode) of
        {ok, ConsumerId} ->
            %% Store consumer PID in state
            NewState = case Type of
                decide -> State#state{decide_consumer = ConsumerId};
                results -> State#state{results_consumer = ConsumerId};
                acks -> State#state{acks_consumer = ConsumerId}
            end,
            {ok, ConsumerId, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

subscribe_to_subject(Type, Opts, State) ->
    Subject = maps:get(subject, Opts),
    DurableGroup = maps:get(durable_group, Opts),
    AckPolicy = maps:get(ack_policy, Opts),
    DeliverGroup = maps:get(deliver_group, Opts),
    Mode = maps:get(mode, Opts),

    case router_nats:subscribe_jetstream(Subject, DurableGroup, AckPolicy, DeliverGroup, Mode) of
        {ok, ConsumerId} ->
            %% Update state with new consumer
            NewState = case Type of
                decide -> State#state{decide_consumer = ConsumerId};
                results -> State#state{results_consumer = ConsumerId};
                acks -> State#state{acks_consumer = ConsumerId}
            end,
            {ok, ConsumerId, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

get_consumer_config(Type) ->
    BaseConfig = #{
        ack_policy => explicit,
        deliver_group => undefined,
        mode => pull_mode
    },

    case Type of
        decide ->
            Subject = application:get_env(beamline_router, decide_subject, ~"beamline.router.v1.decide"),
            DurableGroup = application:get_env(beamline_router, nats_js_durable_group_decide, ~"router-decide-consumer"),
            BaseConfig#{
                subject => Subject,
                durable_group => DurableGroup
            };
        results ->
            Subject = application:get_env(beamline_router, results_subject, ~"beamline.router.v1.results"),
            DurableGroup = application:get_env(beamline_router, nats_js_durable_group_results, ~"router-results"),
            DeliverGroup = application:get_env(beamline_router, nats_js_deliver_group_results, ~"router-results-group"),
            BaseConfig#{
                subject => Subject,
                durable_group => DurableGroup,
                deliver_group => DeliverGroup
            };
        acks ->
            Subject = application:get_env(beamline_router, ack_subject, ~"beamline.router.v1.acks"),
            DurableGroup = application:get_env(beamline_router, nats_js_durable_group_acks, ~"router-acks"),
            DeliverGroup = application:get_env(beamline_router, nats_js_deliver_group_acks, ~"router-acks-group"),
            BaseConfig#{
                subject => Subject,
                durable_group => DurableGroup,
                deliver_group => DeliverGroup
            }
    end.
