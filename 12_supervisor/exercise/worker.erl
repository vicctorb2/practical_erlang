-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% TODO

start_link(Id)->
    gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
    {ok, Id}.

ping(Pid) ->
    gen_server:call(Pid, ping).

handle_cast(_Message, State) ->
    {noreply, State}.

handle_call(ping, _From, State) ->
    Reply = {State, self()},
    {reply, Reply, State}.


handle_info(_Request, State) ->
    {noreply, State}.
        
        
terminate(_Reason, _State) ->
    ok.
        
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.