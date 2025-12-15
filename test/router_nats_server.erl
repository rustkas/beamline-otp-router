%% @doc Helper to manage a real NATS server instance during tests
-module(router_nats_server).

-export([ensure_running/0, stop/0, url/0, port/0,
         configure_env/0, ensure_router_process/0, stop_router_process/0, wait_for_connection/1,
         normalize_command_output/1]).

-define(DEFAULT_PORT, 4222).
-define(STATE_KEY, router_nats_server_state).

ensure_running() ->
    case persistent_term:get(?STATE_KEY, undefined) of
        undefined ->
            start_server();
        {Pid, Port, Count} when Count >= 1 ->
            persistent_term:put(?STATE_KEY, {Pid, Port, Count + 1}),
            {ok, Pid}
    end.

stop() ->
    case persistent_term:get(?STATE_KEY, undefined) of
        undefined -> ok;
        {Pid, _Port, Count} when Count > 1 ->
            persistent_term:put(?STATE_KEY, {Pid, _Port, Count - 1}),
            ok;
        {Pid, _Port, 1} ->
            kill_pid(Pid),
            persistent_term:erase(?STATE_KEY),
            ok
    end.

url() ->
    io_lib:format("nats://127.0.0.1:~p", [port()]).

port() ->
    case persistent_term:get(?STATE_KEY, undefined) of
        undefined -> ?DEFAULT_PORT;
        {_, Port, _} -> Port
    end.

configure_env() ->
    ok = application:set_env(beamline_router, nats_mode, real),
    ok = application:set_env(beamline_router, nats_url, url()),
    ok.

ensure_router_process() ->
    case whereis(router_nats) of
        undefined ->
            {ok, Pid} = router_nats:start_link(),
            wait_for_connection(10000),
            {ok, Pid};
        Pid when is_pid(Pid) ->
            wait_for_connection(10000),
            {ok, Pid}
    end.

stop_router_process() ->
    catch router_nats:stop(),
    ok.

wait_for_connection(Timeout) ->
    catch router_test_nats_helpers:wait_for_connection(Timeout),
    ok.

%%% Internal helpers

start_server() ->
    Port = ?DEFAULT_PORT,
    case find_binary() of
        {error, Reason} ->
            {error, Reason};
        {ok, Binary} ->
            Root = project_root(),
            LogPath = log_path(Root),
            Command = start_command(Root, Binary, Port, LogPath),
            case os:cmd(Command) of
                Output ->
                    case string:trim(Output) of
                        "" ->
                            {error, no_pid};
                        PidText ->
                            case catch list_to_integer(string:trim(PidText)) of
                                Pid when is_integer(Pid) ->
                                    persistent_term:put(?STATE_KEY, {Pid, Port, 1}),
                                    {ok, Pid};
                                _ ->
                                    {error, invalid_pid, Output}
                            end
                    end
            end
    end.

kill_pid(Pid) when is_integer(Pid) ->
    _ = os:cmd(io_lib:format("kill -TERM ~p >/dev/null 2>&1", [Pid])),
    ok.

find_binary() ->
    case os:getenv("NATS_SERVER_BIN") of
        false ->
            case which("nats-server") of
                {ok, Path} -> {ok, Path};
                error ->
                    case bundled_binary() of
                        {ok, Bin} -> {ok, Bin};
                        Error -> Error
                    end
            end;
        Bin ->
            {ok, Bin}
    end.

which(Command) ->
    Output = os:cmd(io_lib:format("command -v ~s", [Command])),
    Trimmed = normalize_command_output(Output),
    case Trimmed of
        "" -> error;
        Path -> {ok, Path}
    end.

bundled_binary() ->
    Root = project_root(),
    Bin = filename:join([Root, "nats-server-v2.10.7-linux-amd64", "nats-server"]),
    case filelib:is_file(Bin) of
        true -> {ok, Bin};
        false -> {error, nats_binary_missing}
    end.

start_command(Root, Binary, Port, LogPath) ->
    CommandTemplate = "cd ~s && ~s -p ~p >~s 2>&1 & echo $!",
    lists:flatten(io_lib:format(CommandTemplate, [
        shell_quote(Root),
        shell_quote(Binary),
        Port,
        shell_quote(LogPath)
    ])).

log_path(Root) ->
    Path = filename:join(Root, "tmp/router_nats_server.log"),
    _ = filelib:ensure_dir(filename:dirname(Path)),
    Path.

project_root() ->
    Beam = code:which(?MODULE),
    TestDir = filename:dirname(Beam),
    filename:dirname(TestDir).

shell_quote(Path) when is_binary(Path) ->
    shell_quote(binary_to_list(Path));
shell_quote(Path) when is_list(Path) ->
    Escaped = string:replace(Path, "'", "'\"'\"'", all),
    lists:concat(["'", Escaped, "'"]).

normalize_command_output(Output) when is_binary(Output) ->
    normalize_command_output(binary_to_list(Output));
normalize_command_output(Output) when is_list(Output) ->
    trim_whitespace(Output);
normalize_command_output(_) ->
    [].

trim_whitespace(Chars) ->
    trim_trailing(trim_leading(Chars)).

trim_leading(Chars) ->
    lists:dropwhile(fun is_whitespace/1, Chars).

trim_trailing(Chars) ->
    lists:reverse(
        lists:dropwhile(fun is_whitespace/1, lists:reverse(Chars))
    ).

is_whitespace(C) ->
    lists:member(C, [ $\s, $\t, $\n, $\r, $\f, $\v ]).
