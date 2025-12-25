%%%-------------------------------------------------------------------
%%% @doc Mock Router for Gateway Testing
%%%
%%% Lightweight, deterministic mock that responds to NATS requests
%%% from c-gateway for testing purposes.
%%%
%%% Features:
%%% - NATS request-reply support
%%% - Configurable scenarios
%%% - Deterministic responses
%%% - Fast startup (< 1s)
%%% - CI-friendly
%%% @end
%%%-------------------------------------------------------------------
-module(router_mock).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([stop/0]).
-export([add_scenario/2]).
-export([reset/0]).
-export([stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(NATS_SUBJECT, <<"beamline.router.v1.decide">>).

-record(state, {
    nats_conn :: pid() | undefined,
    scenarios = [] :: list(),
    stats = #{} :: map()
}).

-record(scenario, {
    name :: binary(),
    match :: map(),
    response :: map() | {error, term()},
    delay_ms = 0 :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Add a scenario for testing
add_scenario(Name, ScenarioSpec) ->
    gen_server:call(?SERVER, {add_scenario, Name, ScenarioSpec}).

%% @doc Reset scenarios and stats
reset() ->
    gen_server:call(?SERVER, reset).

%% @doc Get statistics
stats() ->
    gen_server:call(?SERVER, stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    process_flag(trap_exit, true),
    
    % Load configuration
    ScenariosFile = proplists:get_value(scenarios_file, Opts, "test/mock_scenarios.json"),
    Scenarios = load_scenarios(ScenariosFile),
    
    % Connect to NATS
    NatsUrl = proplists:get_value(nats_url, Opts, "nats://localhost:4222"),
    {ok, Conn} = connect_nats(NatsUrl),
    
    % Subscribe to router subject
    ok = gnat:sub(Conn, self(), ?NATS_SUBJECT),
    
    logger:info("Mock Router started with ~p scenarios", [length(Scenarios)]),
    
    {ok, #state{
        nats_conn = Conn,
        scenarios = Scenarios,
        stats = #{requests => 0, responses => 0, errors => 0}
    }}.

handle_call({add_scenario, Name, Spec}, _From, State) ->
    Scenario = spec_to_scenario(Name, Spec),
    NewScenarios = [Scenario | State#state.scenarios],
    {reply, ok, State#state{scenarios = NewScenarios}};

handle_call(reset, _From, State) ->
    {reply, ok, State#state{
        scenarios = [],
        stats = #{requests => 0, responses => 0, errors => 0}
    }};

handle_call(stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({msg, _Topic, _ReplyTo = undefined, _Msg}, State) ->
    % Ignore messages without reply-to
    {noreply, State};

handle_info({msg, Topic, ReplyTo, Msg}, State) ->
    % Handle NATS request
    Stats = State#state.stats,
    NewStats = Stats#{requests => maps:get(requests, Stats, 0) + 1},
    
    % Process request
    case process_request(Msg, State#state.scenarios) of
        {ok, Response, DelayMs} ->
            % Simulate latency if configured
            maybe_delay(DelayMs),
            
            % Send response
            ok = gnat:pub(State#state.nats_conn, ReplyTo, jsx:encode(Response)),
            
            FinalStats = NewStats#{responses => maps:get(responses, NewStats, 0) + 1},
            {noreply, State#state{stats = FinalStats}};
        
        {error, Reason} ->
            % Send error response
            ErrorResponse = #{
                <<"error">> => atom_to_binary(Reason, utf8),
                <<"message">> => <<"Mock router error">>
            },
            ok = gnat:pub(State#state.nats_conn, ReplyTo, jsx:encode(ErrorResponse)),
            
            FinalStats = NewStats#{errors => maps:get(errors, NewStats, 0) + 1},
            {noreply, State#state{stats = FinalStats}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.nats_conn of
        undefined -> ok;
        Conn -> gnat:close(Conn)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_nats(Url) ->
    case gnat:start_link(#{url => Url}) of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} ->
            logger:error("Failed to connect to NATS: ~p", [Reason]),
            {error, Reason}
    end.

load_scenarios(FilePath) ->
    case filelib:is_file(FilePath) of
        true ->
            {ok, Content} = file:read_file(FilePath),
            Config = jsx:decode(Content, [return_maps]),
            ScenarioSpecs = maps:get(<<"scenarios">>, Config, []),
            [json_to_scenario(S) || S <- ScenarioSpecs];
        false ->
            logger:warning("Scenarios file not found: ~s, using defaults", [FilePath]),
            default_scenarios()
    end.

json_to_scenario(Json) ->
    #scenario{
        name = maps:get(<<"name">>, Json),
        match = maps:get(<<"match">>, Json, #{}),
        response = case maps:get(<<"error">>, Json, undefined) of
            undefined -> maps:get(<<"response">>, Json);
            Error -> {error, binary_to_atom(Error, utf8)}
        end,
        delay_ms = maps:get(<<"delay_ms">>, Json, 0)
    }.

spec_to_scenario(Name, Spec) when is_map(Spec) ->
    #scenario{
        name = Name,
        match = maps:get(match, Spec, #{}),
        response = maps:get(response, Spec),
        delay_ms = maps:get(delay_ms, Spec, 0)
    }.

default_scenarios() ->
    [
        #scenario{
            name = <<"default_success">>,
            match = #{},
            response = #{
                <<"decision_id">> => <<"mock-dec-001">>,
                <<"provider_id">> => <<"openai">>,
                <<"reason">> => <<"mock">>,
                <<"priority">> => 1,
                <<"expected_cost">> => 0.0001
            }
        }
    ].

process_request(MsgBin, Scenarios) ->
    try
        Request = jsx:decode(MsgBin, [return_maps]),
        
        % Find matching scenario
        case find_matching_scenario(Request, Scenarios) of
            {ok, Scenario} ->
                case Scenario#scenario.response of
                    {error, Reason} ->
                        {error, Reason};
                    Response ->
                        % Enrich response with request data
                        EnrichedResponse = enrich_response(Response, Request),
                        {ok, EnrichedResponse, Scenario#scenario.delay_ms}
                end;
            not_found ->
                % Use default response
                DefaultResponse = #{
                    <<"decision_id">> => generate_decision_id(),
                    <<"provider_id">> => <<"mock-provider">>,
                    <<"reason">> => <<"no_matching_scenario">>,
                    <<"priority">> => 1,
                    <<"expected_cost">> => 0.0
                },
                {ok, DefaultResponse, 0}
        end
    catch
        error:CatchReason ->
            logger:error("Failed to process request: ~p", [CatchReason]),
            {error, invalid_request}
    end.

find_matching_scenario(_Request, []) ->
    not_found;
find_matching_scenario(Request, [Scenario | Rest]) ->
    case matches(Request, Scenario#scenario.match) of
        true -> {ok, Scenario};
        false -> find_matching_scenario(Request, Rest)
    end.

matches(_Request, Match) when map_size(Match) == 0 ->
    % Empty match means match all
    false;  % But we want to try other scenarios first
matches(Request, Match) ->
    maps:fold(fun(K, V, Acc) ->
        Acc andalso (maps:get(K, Request, undefined) == V)
    end, true, Match).

enrich_response(Response, Request) ->
    % Add trace_id if present in request
    case maps:get(<<"trace_id">>, Request, undefined) of
        undefined -> Response;
        TraceId -> Response#{<<"trace_id">> => TraceId}
    end.

generate_decision_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("mock-dec-~p-~p", [Timestamp, Random])).

maybe_delay(0) -> ok;
maybe_delay(Ms) when Ms > 0 -> timer:sleep(Ms).
