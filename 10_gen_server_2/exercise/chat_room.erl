-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1,add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    users = #{},
    messages = []
}).

start_link()->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

add_user(RoomPid, Username, UserPid) ->
    gen_server:cast(RoomPid, {add_user, Username, UserPid}),
    ok.

remove_user(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove_user, UserPid}).

get_users(RoomPid) ->
    gen_server:call(RoomPid, {get_users}).

add_message(RoomPid, Author, Message) ->
    gen_server:cast(RoomPid, {add_message, Author, Message}),
    ok.

get_history(RoomPid) ->
    gen_server:call(RoomPid, {get_history}).


handle_call({remove_user, UserPid}, _From, #state{users = CurrUsers} = State) ->
    case maps:find(UserPid, CurrUsers) of
        {ok, _User} -> 
            NewUsers = maps:remove(UserPid, CurrUsers),
            NewState = State#state{users = NewUsers},
            Reply = ok;
        error ->
            Reply = {error, user_not_found},
            NewState = State
    end,
    {reply, Reply, NewState};

handle_call({get_users}, _From, #state{users = CurrUsers} = State) ->
    Reply = [{Username, UserPid} || {UserPid, Username} <- maps:to_list(CurrUsers)],
    {reply, Reply, State};

handle_call({get_history}, _From, #state{messages = CurrMessages} = State) ->
    Reply = lists:reverse(CurrMessages),
    {reply, Reply, State}.
        

handle_cast({add_user, Username, UserPid}, #state{users = CurrUsers, messages  = CurrMessages} = State) ->
    NewUsers = maps:put(UserPid, Username, CurrUsers),
    NewState = State#state{users = NewUsers, messages = CurrMessages},
    {noreply, NewState};

handle_cast({add_message, Author, Message}, #state{users = CurrUsers, messages  = CurrMessages} = State) ->
    UserPidsList = maps:keys(CurrUsers),
    lists:foreach(
        fun(UserPid) ->
            chat_user:add_message(UserPid, Author, Message)
            end,
        UserPidsList
    ),
    NewState = State#state{messages = [{Author, Message} | CurrMessages]},
    {noreply, NewState}.

handle_info(_Request, State) ->
    {noreply, State}.
                
                
terminate(_Reason, _State) ->
    ok.
                
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.