%% @doc NATS Admin Subscriber
%% Subscribes to admin NATS subjects and handles request-reply
%% Subjects:
%%   - beamline.router.v1.admin.get_extension_health
%%   - beamline.router.v1.admin.get_circuit_breaker_states
%%   - beamline.router.v1.admin.dry_run_pipeline
%%   - beamline.router.v1.admin.get_pipeline_complexity
-module(router_admin_nats_subscriber).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("beamline_router.hrl").

-define(SUBJECTS, [
    <<"beamline.router.v1.admin.get_extension_health">>,
    <<"beamline.router.v1.admin.get_circuit_breaker_states">>,
    <<"beamline.router.v1.admin.dry_run_pipeline">>,
    <<"beamline.router.v1.admin.get_pipeline_complexity">>
]).

-record(state, {
    connection :: pid() | undefined
}).

%% @doc Start admin NATS subscriber
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize subscriber
-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    %% Subscribe to all admin subjects
    Timeout = application:get_env(beamline_router, nats_timeout_ms, 5000),
    
    case subscribe_to_admin_subjects(Timeout) of
        ok ->
            {ok, #state{connection = undefined}};
        Error ->
            router_logger:error(<<"Admin NATS subscription failed">>, #{
                <<"error">> => Error
            }),
            {stop, Error}
    end.

%% @doc Handle calls
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handle casts
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
-spec handle_info({nats_message, binary(), binary(), binary() | undefined, map(), binary() | undefined} | term(), #state{}) -> {noreply, #state{}}.
handle_info({nats_message, Subject, Payload, ReplyTo, _Headers, _MsgId}, State) ->
    handle_admin_request(Subject, Payload, ReplyTo),
    {noreply, State};
%% Backward compatibility: handle old format without ReplyTo
handle_info({nats_message, Subject, Payload}, State) ->
    handle_admin_request(Subject, Payload, undefined),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Internal: Subscribe to admin subjects
-spec subscribe_to_admin_subjects(integer()) -> ok | {error, term()}.
subscribe_to_admin_subjects(Timeout) ->
    lists:foldl(fun(Subject, Acc) ->
        case Acc of
            ok ->
                case router_nats:subscribe(Subject, fun handle_admin_nats_message/2, Timeout) of
                    ok -> ok;
                    Error -> Error
                end;
            Error -> Error
        end
    end, ok, ?SUBJECTS).

%% Internal: Handle NATS message (callback for router_nats:subscribe)
%% Note: This callback doesn't receive ReplyTo, so we use gen_server message passing instead
-spec handle_admin_nats_message(binary(), binary()) -> ok.
handle_admin_nats_message(Subject, Payload) ->
    %% Forward to gen_server to handle with ReplyTo support
    ?MODULE ! {nats_message, Subject, Payload},
    ok.

%% Internal: Handle admin request
-spec handle_admin_request(binary(), binary(), binary() | undefined) -> ok.
handle_admin_request(Subject, Payload, ReplyTo) ->
    try
        Response = case Subject of
            <<"beamline.router.v1.admin.get_extension_health">> ->
                router_admin_nats:handle_get_extension_health(Payload);
            <<"beamline.router.v1.admin.get_circuit_breaker_states">> ->
                router_admin_nats:handle_get_circuit_breaker_states(Payload);
            <<"beamline.router.v1.admin.dry_run_pipeline">> ->
                router_admin_nats:handle_dry_run_pipeline(Payload);
            <<"beamline.router.v1.admin.get_pipeline_complexity">> ->
                router_admin_nats:handle_get_pipeline_complexity(Payload);
            _ ->
                {error, <<"Unknown admin subject">>}
        end,
        
        case Response of
            {ok, ResponseJson} ->
                %% Publish response to reply subject (if provided)
                case ReplyTo of
                    undefined ->
                        %% No reply subject - this is a one-way message
                        ok;
                    ReplySubjectVal when is_binary(ReplySubjectVal) ->
                        router_nats:publish(ReplySubjectVal, ResponseJson)
                end;
            {error, ErrorJson} ->
                %% Publish error response
                case ReplyTo of
                    undefined -> ok;
                    ReplySubjectVal when is_binary(ReplySubjectVal) ->
                        router_nats:publish(ReplySubjectVal, ErrorJson)
                end
        end
    catch
        ErrorClass:ErrorReason:ErrorStacktrace ->
            router_logger:error(<<"Error handling admin request">>, #{
                <<"subject">> => Subject,
                <<"error">> => ErrorClass,
                <<"reason">> => ErrorReason,
                <<"stacktrace">> => ErrorStacktrace
            }),
            %% Send error response if reply subject available
            case ReplyTo of
                undefined -> ok;
                ReplyToVal when is_binary(ReplyToVal) ->
                    ErrorResponse = jsx:encode(#{
                        error => #{
                            code => <<"INTERNAL_ERROR">>,
                            message => <<"Internal server error">>
                        }
                    }),
                    router_nats:publish(ReplyToVal, ErrorResponse);
                _ -> ok
            end
    end.

