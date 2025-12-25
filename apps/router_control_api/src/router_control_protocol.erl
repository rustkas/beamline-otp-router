%% Control API protocol validation and response helpers.
-module(router_control_protocol).

-export([
    decode_request/1,
    handle_request/2,
    handle_request/3,
    validate_request/2,
    validate_request/3,
    is_known_subject/1,
    normalize_subject/1,
    canonical_event_inbox/1
]).

-include("beamline_router.hrl").

-define(VERSION, ~"1").
-define(CONTROL_SUBJECT_PREFIX, <<"beamline.router.control.v1">>).
-define(CONTROL_SUBJECT(Suffix), <<?CONTROL_SUBJECT_PREFIX/binary, ".", Suffix/binary>>).

-define(SUBJECT_PROJECT_OPEN, <<"beamline.router.control.v1.project.open">>).
-define(SUBJECT_PROJECT_CLOSE, <<"beamline.router.control.v1.project.close">>).
-define(SUBJECT_INDEX_START, <<"beamline.router.control.v1.index.start">>).
-define(SUBJECT_INDEX_STATUS, <<"beamline.router.control.v1.index.status">>).
-define(SUBJECT_INDEX_CANCEL, <<"beamline.router.control.v1.index.cancel">>).
-define(SUBJECT_TASK_SUBMIT, <<"beamline.router.control.v1.task.submit">>).
-define(SUBJECT_TASK_STATUS, <<"beamline.router.control.v1.task.status">>).
-define(SUBJECT_TASK_CANCEL, <<"beamline.router.control.v1.task.cancel">>).
-define(SUBJECT_CHAT_START, <<"beamline.router.control.v1.chat.start">>).
-define(SUBJECT_CHAT_SEND, <<"beamline.router.control.v1.chat.send">>).
-define(SUBJECT_CHAT_CANCEL, <<"beamline.router.control.v1.chat.cancel">>).
-define(SUBJECT_EVENTS_SUBSCRIBE, <<"beamline.router.control.v1.events.subscribe">>).
-define(EVENTS_INBOX_PREFIX, <<"beamline.router.control.v1.events.inbox.">>).

%% ------------------------------------------------------------------
%% Public API
%% ------------------------------------------------------------------

-spec decode_request(binary()) -> {ok, map()} | {error, term()}.
decode_request(Payload) ->
    try
        case jsx:decode(Payload, [return_maps]) of
            Map when is_map(Map) -> {ok, Map};
            _ -> {error, invalid_json}
        end
    catch
        _:_ -> {error, invalid_json}
    end.

-spec handle_request(binary(), map()) -> map().
handle_request(Subject, Request) ->
    handle_request(Subject, Request, #{}).

-spec handle_request(binary(), map(), map()) -> map().
handle_request(Subject, Request, Headers) ->
    Canon = normalize_subject(Subject),
    case validate_request(Canon, Request, Headers) of
        {ok, Context} ->
            handle_valid_request(Canon, Request, Context);
        {error, Code, Message, Context} ->
            build_error_response(Context, Code, Message)
    end.

-spec validate_request(binary(), map()) -> {ok, map()} | {error, binary(), binary(), map()}.
validate_request(Subject, Request) ->
    validate_request(Subject, Request, #{}).

-spec validate_request(binary(), map(), map()) -> {ok, map()} | {error, binary(), binary(), map()}.
validate_request(Subject, Request, Headers) ->
    case validate_common(Request, Headers) of
        {ok, Context} ->
            case validate_subject_fields(Subject, Request) of
                ok -> {ok, Context};
                {error, Code, Message} -> {error, Code, Message, Context}
            end;
        {error, Code, Message, Context} ->
            {error, Code, Message, Context}
    end.

%% ------------------------------------------------------------------
%% Validation
%% ------------------------------------------------------------------

-spec validate_common(map(), map()) -> {ok, map()} | {error, binary(), binary(), map()}.
validate_common(Request, Headers) ->
    Version = maps:get(~"version", Request, undefined),
    TenantId0 = maps:get(~"tenant_id", Request, undefined),
    RequestId = maps:get(~"request_id", Request, undefined),
    TraceId = maps:get(~"trace_id", Request, undefined),
    IdemKey = maps:get(~"idempotency_key", Request, undefined),
    TenantId = case TenantId0 of
        undefined -> router_control_config:control_tenant_id();
        _ -> TenantId0
    end,
    Context = build_context(maps:put(~"tenant_id", TenantId, Request)),
    case Version of
        undefined ->
            {error, ~"invalid_request", ~"Missing version", Context};
        ?VERSION ->
            Missing = missing_required_fields([
                {~"tenant_id", TenantId},
                {~"request_id", RequestId},
                {~"trace_id", TraceId},
                {~"idempotency_key", IdemKey}
            ]),
            case Missing of
                [] ->
                    case validate_token(Request, Headers) of
                        ok -> {ok, Context};
                        {error, Code, Message} -> {error, Code, Message, Context}
                    end;
                _ ->
                    {error, ~"invalid_request",
                     missing_fields_message(Missing), Context}
            end;
        _ ->
            {error, ~"invalid_request", ~"Unsupported version", Context}
    end.

-spec validate_subject_fields(binary(), map()) -> ok | {error, binary(), binary()}.
validate_subject_fields(Subject, Request) ->
    case is_known_subject(Subject) of
        true ->
            Required = required_fields(Subject),
            case Required of
                [] -> ok;
                _ ->
                    Missing = missing_required_fields(
                        lists:map(fun(Key) -> {Key, nested_get(Request, Key)} end, Required)
                    ),
                    case Missing of
                        [] -> ok;
                        _ -> {error, ~"invalid_request", missing_fields_message(Missing)}
                    end
            end;
        false ->
            {error, ~"invalid_subject", ~"Unknown subject"}
    end.

-spec is_known_subject(binary()) -> boolean().
is_known_subject(?SUBJECT_PROJECT_OPEN) -> true;
is_known_subject(?SUBJECT_PROJECT_CLOSE) -> true;
is_known_subject(?SUBJECT_INDEX_START) -> true;
is_known_subject(?SUBJECT_INDEX_STATUS) -> true;
is_known_subject(?SUBJECT_INDEX_CANCEL) -> true;
is_known_subject(?SUBJECT_TASK_SUBMIT) -> true;
is_known_subject(?SUBJECT_TASK_STATUS) -> true;
is_known_subject(?SUBJECT_TASK_CANCEL) -> true;
is_known_subject(?SUBJECT_CHAT_START) -> true;
is_known_subject(?SUBJECT_CHAT_SEND) -> true;
is_known_subject(?SUBJECT_CHAT_CANCEL) -> true;
is_known_subject(?SUBJECT_EVENTS_SUBSCRIBE) -> true;
is_known_subject(_) -> false.

-spec validate_token(map(), map()) -> ok | {error, binary(), binary()}.
validate_token(Request, Headers) ->
    case router_control_config:control_token() of
        undefined -> ok;
        ExpectedToken ->
            Token = case get_header_token(Headers) of
                undefined -> maps:get(token_field(), Request, undefined);
                HeaderToken -> HeaderToken
            end,
            case Token of
                ExpectedToken -> ok;
                _ -> {error, ~"unauthorized", ~"Invalid control token"}
            end
    end.

%% ------------------------------------------------------------------
%% Subject-specific handling
%% ------------------------------------------------------------------

-spec handle_valid_request(binary(), map(), map()) -> map().
handle_valid_request(Subject, _Request, Context) ->
    EventsSubject = ?SUBJECT_EVENTS_SUBSCRIBE,
    case Subject of
        EventsSubject ->
            SubscriptionId = router_uuid:generate_v4(),
            Inbox = canonical_event_inbox(SubscriptionId),
            build_success_response(Context, #{
                ~"subscription_id" => SubscriptionId,
                ~"inbox_subject" => Inbox
            });
        _ ->
            build_error_response(Context, ~"not_implemented", ~"Control command not implemented")
    end.

%% ------------------------------------------------------------------
%% Responses
%% ------------------------------------------------------------------

-spec build_success_response(map(), map()) -> map().
build_success_response(Context, Result) ->
    #{
        ~"ok" => true,
        ~"result" => Result,
        ~"context" => Context
    }.

-spec build_error_response(map(), binary(), binary()) -> map().
build_error_response(Context, Code, Message) ->
    #{
        ~"ok" => false,
        ~"error" => #{
            ~"code" => Code,
            ~"message" => Message
        },
        ~"context" => Context
    }.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

-spec build_context(map()) -> map().
build_context(Request) ->
    Base = #{
        ~"tenant_id" => maps:get(~"tenant_id", Request, undefined),
        ~"request_id" => maps:get(~"request_id", Request, undefined),
        ~"trace_id" => maps:get(~"trace_id", Request, undefined),
        ~"idempotency_key" => maps:get(~"idempotency_key", Request, undefined),
        ~"run_id" => maps:get(~"run_id", Request, undefined),
        ~"step_id" => maps:get(~"step_id", Request, undefined)
    },
    maps:filter(fun(_Key, Value) -> Value =/= undefined end, Base).

-spec missing_required_fields([{binary(), term()}]) -> [binary()].
missing_required_fields(Pairs) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        case is_missing(Value) of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], Pairs).

-spec is_missing(term()) -> boolean().
is_missing(undefined) -> true;
is_missing(<<>>) -> true;
is_missing(_) -> false.

-spec missing_fields_message([binary()]) -> binary().
missing_fields_message(Keys) ->
    Joined = string:join([binary_to_list(K) || K <- lists:reverse(Keys)], ", "),
    list_to_binary("Missing required fields: " ++ Joined).

-spec required_fields(binary()) -> [binary()].
required_fields(?SUBJECT_PROJECT_OPEN) ->
    [~"project_root"];
required_fields(?SUBJECT_PROJECT_CLOSE) ->
    [~"project_id"];
required_fields(?SUBJECT_INDEX_START) ->
    [~"project_id"];
required_fields(?SUBJECT_INDEX_STATUS) ->
    [~"index_id"];
required_fields(?SUBJECT_INDEX_CANCEL) ->
    [~"index_id"];
required_fields(?SUBJECT_TASK_SUBMIT) ->
    [~"job", ~"job.type"];
required_fields(?SUBJECT_TASK_STATUS) ->
    [~"task_id"];
required_fields(?SUBJECT_TASK_CANCEL) ->
    [~"task_id"];
required_fields(?SUBJECT_CHAT_START) ->
    [~"project_id"];
required_fields(?SUBJECT_CHAT_SEND) ->
    [~"session_id", ~"message"];
required_fields(?SUBJECT_CHAT_CANCEL) ->
    [~"session_id"];
required_fields(?SUBJECT_EVENTS_SUBSCRIBE) ->
    [];
required_fields(_) ->
    [].

%% ------------------------------------------------------------------
%% Subject normalization
%% ------------------------------------------------------------------

normalize_subject(Subject) when is_binary(Subject) ->
    Prefix = router_control_config:control_subject_prefix(),
    Alias = router_control_config:control_alias_prefix(),
    case binary:match(Subject, Prefix) of
        {0, _Length} -> Subject;
        nomatch ->
            case router_control_config:control_alias_enabled() of
                true ->
                    case binary:match(Subject, Alias) of
                        {0, AliasLen} ->
                            Body = binary:part(Subject, AliasLen, byte_size(Subject) - AliasLen),
                            <<Prefix/binary, Body/binary>>;
                        nomatch -> Subject
                    end;
                false ->
                    Subject
            end
    end.

canonical_event_inbox(SubscriptionId) ->
    <<?EVENTS_INBOX_PREFIX/binary, SubscriptionId/binary>>.

%% Supports nested keys in form <<"job.type">>
-spec nested_get(map(), binary()) -> term().
nested_get(Request, Key) ->
    case binary:split(Key, <<".">>, [global]) of
        [Single] ->
            maps:get(Single, Request, undefined);
        [First, Second] ->
            case maps:get(First, Request, undefined) of
                Map when is_map(Map) -> maps:get(Second, Map, undefined);
                _ -> undefined
            end;
        _ ->
            undefined
    end.

-spec get_header_token(map()) -> binary() | undefined.
get_header_token(Headers) ->
    case get_bin(Headers, token_field(), undefined) of
        undefined ->
            case get_bin(Headers, <<"authorization">>, undefined) of
                undefined -> undefined;
                Value -> parse_bearer(Value)
            end;
        Token ->
            Token
    end.

-spec parse_bearer(binary()) -> binary().
parse_bearer(Value) ->
    case binary:split(Value, <<" ">>, [global]) of
        [<<"Bearer">>, Token] -> Token;
        _ -> Value
    end.

-spec token_field() -> binary().
token_field() ->
    <<$i, $d, $e, $_, $t, $o, $k, $e, $n>>.

-spec get_bin(map(), binary(), term()) -> term().
get_bin(M, K, Default) ->
    case maps:get(K, M, undefined) of
        V when is_binary(V) -> V;
        V when is_list(V) -> list_to_binary(V);
        _ -> Default
    end.
