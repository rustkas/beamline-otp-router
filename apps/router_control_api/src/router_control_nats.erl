%% Router control API NATS handler (request-reply).
-module(router_control_nats).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("beamline_router.hrl").

 -define(COMMAND_SUFFIXES, [
    ~"project.open",
    ~"project.close",
    ~"index.start",
    ~"index.status",
    ~"index.cancel",
    ~"task.submit",
    ~"task.status",
    ~"task.cancel",
    ~"chat.start",
    ~"chat.send",
    ~"chat.cancel",
    ~"events.subscribe"
]).

-record(state, {
    connection :: pid() | undefined
}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    Timeout = application:get_env(beamline_router, nats_timeout_ms, 5000),
    case subscribe_to_subjects(Timeout) of
        ok ->
            {ok, #state{connection = undefined}};
        Error ->
            router_logger:error(~"Control NATS subscription failed", #{
                ~"error" => Error
            }),
            {stop, Error}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info({nats_message, binary(), binary(), binary() | undefined, map(), binary() | undefined} | term(), #state{}) -> {noreply, #state{}}.
handle_info({nats_message, Subject, Payload, ReplyTo, Headers, _MsgId}, State) ->
    handle_control_request(Subject, Payload, ReplyTo, Headers),
    {noreply, State};
%% Backward compatibility: handle old format without ReplyTo
handle_info({nats_message, Subject, Payload}, State) ->
    handle_control_request(Subject, Payload, undefined, #{}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

-spec subscribe_to_subjects(integer()) -> ok | {error, term()}.
subscribe_to_subjects(Timeout) ->
    Subjects = command_subject_variants(),
    lists:foldl(fun(Subject, Acc) ->
        case Acc of
            ok ->
                case router_nats:subscribe(Subject, fun(_Subj, _Payload) -> ok end, Timeout) of
                    ok -> ok;
                    Error -> Error
                end;
            Error -> Error
        end
    end, ok, Subjects).

subject_variants(Suffix) ->
    Prefixes = subject_prefixes(),
    lists:map(fun(Prefix) -> subject_from(Prefix, Suffix) end, Prefixes).

subject_prefixes() ->
    Base = [router_control_config:control_subject_prefix()],
    case router_control_config:control_alias_enabled() of
        true ->
            Base ++ [router_control_config:control_alias_prefix()];
        false ->
            Base
    end.

command_subject_variants() ->
    lists:usort(lists:foldl(fun(Suffix, Acc) ->
        Acc ++ subject_variants(Suffix)
    end, [], ?COMMAND_SUFFIXES)).

subject_from(Prefix, Suffix) ->
    <<Prefix/binary, ".", Suffix/binary>>.

-spec handle_control_request(binary(), binary(), binary() | undefined, map()) -> ok.
handle_control_request(Subject, Payload, ReplyTo, Headers) ->
    ResponseMap = case router_control_protocol:decode_request(Payload) of
        {ok, Request} ->
            router_control_protocol:handle_request(Subject, Request, Headers);
        {error, _} ->
            router_control_protocol:handle_request(Subject, #{}, Headers)
    end,
    ReplyJson = jsx:encode(ResponseMap),
    case ReplyTo of
        undefined -> ok;
        ReplySubject when is_binary(ReplySubject) ->
            router_nats:publish(ReplySubject, ReplyJson)
    end.
