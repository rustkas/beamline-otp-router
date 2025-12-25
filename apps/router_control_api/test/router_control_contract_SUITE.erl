-module(router_control_contract_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, groups_for_level/1]).
-export([test_missing_common_fields/1,
         test_invalid_version/1,
         test_unknown_subject_rejected/1,
         test_events_subscribe_inbox/1,
         test_alias_events_subscribe_inbox/1,
         test_token_required/1,
         test_token_header_accepted/1,
         test_token_header_ide_token_accepted/1,
         test_token_header_precedence/1,
         test_task_submit_requires_job_type/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(_Level) ->
    Base = [{group, core}],
    case os:getenv("ROUTER_CONTROL_SUBJECT_ALIAS") of
        "true" -> Base ++ [{group, alias}];
        "1" -> Base ++ [{group, alias}];
        _ -> Base
    end.

groups() ->
    [
        {core, [], [
            test_missing_common_fields,
            test_invalid_version,
            test_unknown_subject_rejected,
            test_events_subscribe_inbox,
            test_token_required,
            test_token_header_accepted,
            test_token_header_ide_token_accepted,
            test_token_header_precedence,
            test_task_submit_requires_job_type
        ]},
        {alias, [], [
            test_alias_events_subscribe_inbox
        ]}
    ].

test_missing_common_fields(_Config) ->
    Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.project.open">>,
        #{}
    ),
    assert_equal(false, maps:get(~"ok", Response)),
    Error = maps:get(~"error", Response),
    assert_equal(~"invalid_request", maps:get(~"code", Error)).

test_invalid_version(_Config) ->
    Request0 = base_request(),
    Request = maps:merge(Request0, #{~"version" => ~"2"}),
    Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.project.open">>,
        Request#{~"project_root" => ~"/tmp"}
    ),
    assert_equal(false, maps:get(~"ok", Response)),
    Error = maps:get(~"error", Response),
    assert_equal(~"invalid_request", maps:get(~"code", Error)).

test_unknown_subject_rejected(_Config) ->
    Request = base_request(),
    Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.unknown">>,
        Request
    ),
    assert_equal(false, maps:get(~"ok", Response)),
    Error = maps:get(~"error", Response),
    assert_equal(~"invalid_subject", maps:get(~"code", Error)).

test_events_subscribe_inbox(_Config) ->
    Request = base_request(),
    Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.events.subscribe">>,
        Request
    ),
    assert_equal(true, maps:get(~"ok", Response)),
    Result = maps:get(~"result", Response),
    SubscriptionId = maps:get(~"subscription_id", Result),
    Inbox = maps:get(~"inbox_subject", Result),
    Prefix = <<"beamline.router.control.v1.events.inbox.">>,
    assert_prefix(Prefix, Inbox),
    case binary:split(Inbox, Prefix) of
        [<<>>, Suffix] ->
            assert_equal(SubscriptionId, Suffix);
        _ ->
            ct:fail({assert_subscription_suffix, Inbox})
    end,
    assert_binary(SubscriptionId).

test_alias_events_subscribe_inbox(_Config) ->
    Request = base_request(),
    Response = router_control_protocol:handle_request(
        <<"beamline.router.ide.v1.events.subscribe">>,
        Request
    ),
    assert_equal(true, maps:get(~"ok", Response)),
    Result = maps:get(~"result", Response),
    SubscriptionId = maps:get(~"subscription_id", Result),
    Inbox = maps:get(~"inbox_subject", Result),
    Prefix = <<"beamline.router.control.v1.events.inbox.">>,
    assert_prefix(Prefix, Inbox),
    case binary:split(Inbox, Prefix) of
        [<<>>, Suffix] ->
            assert_equal(SubscriptionId, Suffix);
        _ ->
            ct:fail({assert_subscription_suffix, Inbox})
    end,
    assert_binary(SubscriptionId).

test_token_required(_Config) ->
    with_env([{"ROUTER_CONTROL_TOKEN", "secret"}], fun() ->
        Request = base_request(),
        Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.project.open">>,
            Request#{~"project_root" => ~"/tmp"}
        ),
        assert_equal(false, maps:get(~"ok", Response)),
        Error = maps:get(~"error", Response),
        assert_equal(~"unauthorized", maps:get(~"code", Error))
    end).

test_token_header_accepted(_Config) ->
    with_env([{"ROUTER_CONTROL_TOKEN", "secret"}], fun() ->
        Request = base_request(),
        Headers = #{<<"authorization">> => <<"Bearer secret">>},
        Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.project.open">>,
            Request#{~"project_root" => ~"/tmp"},
            Headers
        ),
        assert_equal(false, maps:get(~"ok", Response)),
        Error = maps:get(~"error", Response),
        assert_equal(~"not_implemented", maps:get(~"code", Error))
    end).

test_token_header_ide_token_accepted(_Config) ->
    with_env([{"ROUTER_CONTROL_TOKEN", "secret"}], fun() ->
        Request = base_request(),
        Headers = #{<<"ide_token">> => <<"secret">>},
        Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.project.open">>,
            Request#{~"project_root" => ~"/tmp"},
            Headers
        ),
        assert_equal(false, maps:get(~"ok", Response)),
        Error = maps:get(~"error", Response),
        assert_equal(~"not_implemented", maps:get(~"code", Error))
    end).

test_token_header_precedence(_Config) ->
    with_env([{"ROUTER_CONTROL_TOKEN", "secret"}], fun() ->
        Request = base_request(),
        Headers = #{<<"ide_token">> => <<"secret">>},
        Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.project.open">>,
            Request#{~"project_root" => ~"/tmp", ~"ide_token" => ~"wrong"},
            Headers
        ),
        assert_equal(false, maps:get(~"ok", Response)),
        Error = maps:get(~"error", Response),
        assert_equal(~"not_implemented", maps:get(~"code", Error))
    end).

test_task_submit_requires_job_type(_Config) ->
    Request0 = base_request(),
    Request = maps:merge(Request0, #{~"job" => #{}}),
    Response = router_control_protocol:handle_request(
        <<"beamline.router.control.v1.task.submit">>,
        Request
    ),
    assert_equal(false, maps:get(~"ok", Response)),
    Error = maps:get(~"error", Response),
    assert_equal(~"invalid_request", maps:get(~"code", Error)).

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

base_request() ->
    #{
        ~"version" => ~"1",
        ~"tenant_id" => ~"local",
        ~"request_id" => ~"req-1",
        ~"trace_id" => ~"tr-1",
        ~"idempotency_key" => ~"idem-1"
    }.

with_env(EnvVars, Fun) ->
    Prev = [{Key, os:getenv(Key)} || {Key, _} <- EnvVars],
    lists:foreach(fun({Key, Value}) -> _ = os:putenv(Key, Value) end, EnvVars),
    try
        Fun()
    after
        lists:foreach(fun({Key, Value}) -> restore_env(Key, Value) end, Prev)
    end.

restore_env(Key, false) ->
    _ = os:unsetenv(Key),
    ok;
restore_env(Key, Value) ->
    _ = os:putenv(Key, Value),
    ok.

assert_equal(Expected, Actual) ->
    case Actual =:= Expected of
        true -> ok;
        false -> ct:fail({assert_equal, Expected, Actual})
    end.

assert_prefix(Prefix, Value) ->
    case binary:split(Value, Prefix) of
        [<<>>, _] -> ok;
        _ -> ct:fail({assert_prefix, Prefix, Value})
    end.

assert_binary(Value) ->
    case is_binary(Value) of
        true -> ok;
        false -> ct:fail({assert_binary, Value})
    end.
