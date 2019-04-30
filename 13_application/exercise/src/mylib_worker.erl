-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0,get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state,{
    version,
    modules = [],
    min_val,
    max_val,
    query_timeout,
    connection_timeout
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, ApplicationVersion} = application:get_key(mylib, vsn),
    {ok, ApplicationModules} = application:get_key(mylib, modules),
    {ok, MinVal} = application:get_env(mylib, min_val),
    {ok, MaxVal} = application:get_env(mylib, max_val),
    {ok, ConnectionTimeout} = application:get_env(mylib, connection_timeout),
    {ok, QueryTimeout} = application:get_env(mylib, query_timeout),
    State = #state{
        version = ApplicationVersion,
        modules = ApplicationModules,
        min_val = MinVal,
        max_val = MaxVal,
        query_timeout = QueryTimeout,
        connection_timeout = ConnectionTimeout
    },
    % State = #state{},
    {ok, State}.

get_version() ->
    gen_server:call(?MODULE, get_version).
        
        
get_modules() ->
    gen_server:call(?MODULE, get_modules).
        
        
get_min_val() ->
    gen_server:call(?MODULE, get_min_val).
        
        
get_connection_timeout() ->
    gen_server:call(?MODULE, get_connection_timeout).
        
        
all_apps() ->
    Applications = application:which_applications(),
    lists:foldl(fun({Application, Description, Version}, Acc) ->
                        Acc#{Application => #{description => Description, version => Version}}
                end,
                #{},
                Applications).

handle_call(get_modules, _From, #state{modules = Modules} = State) ->
    {reply, Modules, State};
        
handle_call(get_connection_timeout, _From, #state{connection_timeout = ConnectionTimeout} = State) ->
    {reply, ConnectionTimeout, State};

handle_call(get_min_val, _From, #state{min_val = MinVal} = State) ->
    {reply, MinVal, State};
            
handle_call(get_version, _From, #state{version = Version} = State) ->
    {reply, Version, State};

handle_call(_Any, _From, _State) ->
    {reply, reply, _State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Any, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
