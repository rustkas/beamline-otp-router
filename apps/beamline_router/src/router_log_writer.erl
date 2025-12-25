-module(router_log_writer).
-behaviour(gen_server).

-export([start_link/0, log/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    file :: file:io_device() | undefined,
    current_date :: string() | undefined,
    log_dir :: string()
}).

-doc "Start the log writer".
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(Json) ->
    gen_server:cast(?MODULE, {log, Json}).

init([]) ->
    LogDir = case application:get_env(beamline_router, log_dir, undefined) of
        undefined -> ".windsurf/reports";
        Dir -> Dir
    end,
    %% Ensure dir exists
    ok = filelib:ensure_dir(filename:join(LogDir, "dummy")),
    
    %% Open file
    {Date, File} = open_log_file(LogDir),
    {ok, #state{file = File, current_date = Date, log_dir = LogDir}}.

handle_cast({log, Json}, State = #state{file = File, current_date = CurrentDate, log_dir = LogDir}) ->
    NewDate = date_string(),
    NewState = case NewDate =:= CurrentDate of
        true ->
            write_to_file(File, Json),
            State;
        false ->
            %% Rotate file if date changed
            _ = file:close(File),
            {NewDate2, NewFile} = open_log_file(LogDir),
            write_to_file(NewFile, Json),
            State#state{file = NewFile, current_date = NewDate2}
    end,
    {noreply, NewState};
    
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{file = File}) ->
    if File =/= undefined -> file:close(File); true -> ok end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal helpers

date_string() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day])).

open_log_file(LogDir) ->
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    %% Open in raw mode for performance, append mode
    {ok, File} = file:open(LogFile, [append, raw, binary]),
    {Date, File}.

write_to_file(File, Json) ->
    file:write(File, [Json, ~"\n"]).
