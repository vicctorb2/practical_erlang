-module(main).

-export([parse/1]).


-record(state, {
    statistic = #{},
    errors = #{}
}).

parse(Files) ->
    Workers = lists:foldl(
    fun(File, Acc) ->
        Pid = spawn(worker, parser, [File, self()]),
        Ref = erlang:monitor(process, Pid),
        Worker = {Pid, Ref},
        Acc#{Worker => File}
    end,
    #{},
    Files),
    InitialState = #state{},
    loop(Workers, InitialState).


loop(Workers, CurrentState) ->
    CurrStatistic = CurrentState#state.statistic,
    CurrErrors = CurrentState#state.errors,
    case map_size(Workers) =< 0 of
        true -> {CurrStatistic, CurrErrors};
        false -> 
            receive
                {result, Data} -> 
                    NewState = aggr(CurrentState, Data),
                    loop(Workers, NewState);
                {'DOWN', Ref, process, Pid, Reason} ->
                    Worker = {Pid, Ref},
                    NewWorkers = maps:remove(Worker, Workers),
                    case Reason of
                        normal -> loop(NewWorkers, CurrentState);
                        _ ->
                            File = maps:get(Worker, Workers),
                            NewState = CurrentState#state{errors = maps:put(File, Reason, CurrErrors)},
                            loop(NewWorkers, NewState)
                    end
            after
                1000 -> {error, no_reply}
            end
    end.


aggr(CurrentState, Data) ->
    % io:format("Starting aggregation~n CurrentState: ~p~n Incoming Data: ~p~n", [CurrentState, Data]),
    CurrStatistic = CurrentState#state.statistic,
    NewStatistic = maps:fold(
        fun (Name, Count, Acc) ->
            case maps:find(Name, CurrStatistic) of
                {ok, OldCount} -> Acc#{Name => OldCount + Count};
                error -> Acc#{Name => Count}
            end
        end,
    CurrStatistic,
    Data
    ),
    % io:format("After aggregation:~p~n", [NewStatistic]),
    CurrentState#state{statistic = NewStatistic}.