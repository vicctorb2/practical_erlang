-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
    messages = []
}).


add_message(Pid, Author, Message) ->
    gen_server:cast(Pid, {add_message, {Author, Message}}),
    ok.


get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

start_link()->
    gen_server:start_link(?MODULE, [], []).


handle_cast({add_message, {Author, Message}}, #state{messages  = CurrMessages} = State) ->
    NewState = State#state{messages = [{Author, Message} | CurrMessages]},
    {noreply, NewState}.

handle_call(get_messages, _From, #state{messages = CurrMessages} = State) ->
    {reply, lists:reverse(CurrMessages), State}.

init([]) ->
    {ok, #state{}}.

handle_info(_Request, State) ->
    {noreply, State}.
        
        
terminate(_Reason, _State) ->
    ok.
        
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
        
