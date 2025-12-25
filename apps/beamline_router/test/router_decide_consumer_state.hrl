-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}
}).
