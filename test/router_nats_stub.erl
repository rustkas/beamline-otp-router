%% @doc Stub implementation for router_nats in tests
-module(router_nats_stub).
-export([request/3, ack_message/1, publish_with_ack/3]).

request(_, _, _) -> {ok, <<"+OK">>}.
ack_message(_) -> ok.
publish_with_ack(_, _, _) -> {ok, <<"dlq-msg">>}.
