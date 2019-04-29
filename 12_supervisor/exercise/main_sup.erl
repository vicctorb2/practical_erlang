-module(main_sup).

-export([start_link/0, init/1]).

%% TODO

start_link() ->
    supervisor:start_link(?MODULE, []).


init(_Args) ->
    SupervisorSpecification = #{strategy => one_for_one,
                                intensity => 10,
                                period => 1000},
    Sup1 =  #{id => sup_1,
                start => {sup_1, start_link, []},
                restart => permanent,
                shutdown => 2000,
                type => supervisor,
                modules => [sup_1]
            },
    Sup2 =  #{id => sup_2,
                start => {sup_2, start_link, []},
                restart => permanent,
                shutdown => 2000,
                type => supervisor,
                modules => [sup_2]
            },

    ChildSpecifications = [Sup1, Sup2],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
