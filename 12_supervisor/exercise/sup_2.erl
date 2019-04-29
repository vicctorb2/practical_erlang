-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

%% TODO
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


add_worker(WorkerId) ->
    supervisor:start_child(?MODULE, new_worker(WorkerId)).


remove_worker(WorkerId) ->
    supervisor:terminate_child(?MODULE, WorkerId),
    supervisor:delete_child(?MODULE, WorkerId).


init(_DoesntMatter) ->
    SupervisorSpecification = #{strategy => one_for_one,
                                intensity => 10,
                                period => 1000},
    Worker3 = new_worker(third_worker),
    Worker4 = new_worker(fourth_worker),
    ChildSpecification = [Worker3, Worker4],
    {ok, {SupervisorSpecification, ChildSpecification}}.


new_worker(WorkerId) ->
    #{id => WorkerId,
      start => {worker, start_link, [WorkerId]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]
     }.