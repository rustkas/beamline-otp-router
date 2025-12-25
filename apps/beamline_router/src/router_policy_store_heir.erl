-module(router_policy_store_heir).

-doc "Heir process for router_policy_store ETS tables".
%% Receives ETS tables when owner process crashes and transfers them back on request
-behaviour(gen_server).

-export([start_link/0, claim/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    tabs = #{}  %% Map: TabId => #{from => Pid, gift => Data}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Called by router_policy_store when it restarts and needs to claim tables
claim(NewOwnerPid) when is_pid(NewOwnerPid) ->
    gen_server:call(?MODULE, {claim, NewOwnerPid}).

init([]) ->
    {ok, #state{}}.

handle_call({claim, NewOwnerPid}, _From, State = #state{tabs = Tabs}) ->
    %% Transfer all known tables to new owner
    Transferred = lists:foldl(
        fun({Tab, _Meta}, Acc) ->
            case ets:info(Tab, owner) of
                OwnerPid when OwnerPid =:= self() ->
                    %% We own this table, transfer it
                    case catch ets:give_away(Tab, NewOwnerPid, none) of
                        true ->
                            [Tab | Acc];
                        GiveAwayError ->
                            %% Log error but continue with other tables
                            case erlang:function_exported(router_logger, warn, 2) of
                                true ->
                                    ErrorBin = case GiveAwayError of
                                        ErrorAtom when is_atom(ErrorAtom) ->
                                            erlang:atom_to_binary(ErrorAtom, utf8);
                                        _ ->
                                            erlang:term_to_binary(GiveAwayError)
                                    end,
                                    router_logger:warn("Failed to transfer ETS table", #{
                                        ~"table" => erlang:ref_to_list(Tab),
                                        ~"error" => ErrorBin
                                    });
                                false ->
                                    ok
                            end,
                            Acc
                    end;
                _ ->
                    %% Table already transferred or we don't own it
                    Acc
            end
        end,
        [],
        maps:to_list(Tabs)
    ),
    
    %% Remove transferred tables from state
    NewTabs = maps:without(Transferred, Tabs),
    
    {reply, ok, State#state{tabs = NewTabs}};

handle_call(_Req, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', Tab, FromPid, GiftData}, State = #state{tabs = Tabs}) ->
    %% Store table information
    NewTabs = Tabs#{Tab => #{from => FromPid, gift => GiftData, received_at => erlang:monotonic_time(second)}},
    
    %% Emit telemetry event for table transfer to heir
    TableNameInfo = ets:info(Tab, name),
    TableName = case TableNameInfo of
        undefined -> ~"unnamed";
        NameAtom when is_atom(NameAtom) -> erlang:atom_to_binary(NameAtom, utf8);
        _ -> ~"unknown"
    end,
    telemetry:execute([router_policy_store, transferred_to_heir], #{}, #{
        table => Tab,
        table_name => TableName,
        from => FromPid
    }),
    
    %% Log transfer for observability
    case erlang:function_exported(router_logger, info, 2) of
        true ->
            router_logger:info("Received ETS table transfer", #{
                ~"table" => erlang:ref_to_list(Tab),
                ~"from_pid" => erlang:pid_to_list(FromPid),
                ~"table_name" => case ets:info(Tab, name) of
                    undefined -> ~"unnamed";
                    Name -> erlang:atom_to_binary(Name, utf8)
                end
            });
        false ->
            ok
    end,
    
    {noreply, State#state{tabs = NewTabs}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

