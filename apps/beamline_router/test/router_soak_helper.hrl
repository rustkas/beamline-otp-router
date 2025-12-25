%% Shared record definitions for router_soak_helper and soak suites.

-record(memory_snapshot, {
    timestamp :: integer(),
    total :: integer(),
    processes :: integer(),
    system :: integer(),
    atom :: integer(),
    binary :: integer(),
    code :: integer(),
    ets :: integer()
}).

-record(ets_snapshot, {
    timestamp :: integer(),
    tables :: [{atom(), integer(), integer()}]  % {TableName, Size, Memory}
}).

-record(process_snapshot, {
    timestamp :: integer(),
    count :: integer(),
    supervisors :: integer(),
    message_queue_total :: integer()
}).
