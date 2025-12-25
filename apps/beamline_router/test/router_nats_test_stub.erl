%% @doc Test stub for router_nats module
-module(router_nats_test_stub).

-export([
    publish/2,
    request/3,
    ack_message/1,
    publish_with_ack/2,
    publish_with_ack/3
]).

%% Stub implementations
publish(_, _) -> ok.
request(_, _, _) -> {ok, <<"+OK">>}.
ack_message(_) -> ok.
publish_with_ack(_, _) -> {ok, <<"msg-id">>}.
publish_with_ack(_, _, _) -> {ok, <<"msg-id">>}.
