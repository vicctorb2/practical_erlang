-module(sup_1).

-export([start_link/0, init/1]).

%% TODO

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{strategy => one_for_one,
                                    intensity => 10,
                                    period => 1000},
    Worker1 =  #{id => first_worker,
                    start => {worker, start_link, [workerId1]},
                    restart => permanent,
                    shutdown => 2000,
                    type => worker,
                    modules => [worker]
                },
    Worker2 =  #{id => second_worker,
                    start => {worker, start_link, [workerId2]},
                    restart => permanent,
                    shutdown => 2000,
                    type => worker,
                    modules => [worker]
                },

    ChildSpecification = [Worker1, Worker2],
    {ok, {SupervisorSpecification, ChildSpecification}}.
