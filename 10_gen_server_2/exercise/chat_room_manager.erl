-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    rooms = #{}
}).

start_link()->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
     {ok, #state{}}.

create_room(RoomName) ->
    {ok, NewRoomPid} = chat_room:start_link(),
    NewRoom = {RoomName, NewRoomPid},
    gen_server:cast(?MODULE, {create_room, NewRoom}),
    NewRoom.

get_rooms() ->
    gen_server:call(?MODULE,{get_rooms}).

add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(?MODULE, {add_user, RoomPid, UserName, UserPid}).

get_users(RoomPid) ->
    gen_server:call(?MODULE, {get_users, RoomPid}).

remove_user(RoomPid, UserPid) ->
    gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).

send_message(RoomPid, Author, Message) ->
    gen_server:call(?MODULE, {send_message, RoomPid, Author, Message}).

get_history(RoomPid) ->
    gen_server:call(?MODULE, {get_history, RoomPid}).

handle_cast({create_room, {RoomName,RoomPid}}, #state{rooms = CurrRooms} = State) ->
    NewRoomsMap = maps:put(RoomPid, RoomName, CurrRooms),
    NewState = State#state{rooms = NewRoomsMap},
    {noreply, NewState}.

handle_call({get_rooms}, _From, #state{rooms = CurrRooms} = State) ->
    Reply = [{RoomName, RoomPid} || {RoomPid, RoomName} <- maps:to_list(CurrRooms)],
    {reply, Reply, State};

handle_call({remove_user, RoomPid, UserPid}, _From, #state{rooms = CurrRooms} = State) ->
    case maps:find(RoomPid, CurrRooms) of
        {ok, _} -> 
            Reply = chat_room:remove_user(RoomPid, UserPid);
        error -> 
            Reply = {error, room_not_found}
    end,
    {reply, Reply, State};

handle_call({get_users, RoomPid}, _From, #state{rooms = CurrRooms} = State) ->
    case maps:find(RoomPid, CurrRooms) of
        {ok, _} -> 
            Users = chat_room:get_users(RoomPid),
            Reply = {ok, Users};
        error -> 
            Reply = {error, room_not_found}
    end,
    {reply, Reply, State};

handle_call({add_user, RoomPid, UserName, UserPid}, _From, #state{rooms = CurrRooms} = State) ->
    case maps:find(RoomPid, CurrRooms) of
        {ok, _} ->
            chat_room:add_user(RoomPid, UserName, UserPid),
            Reply = ok;
        error -> 
            Reply = {error, room_not_found}
    end,
    {reply, Reply, State};

handle_call({send_message, RoomPid, Author, Message}, _From, #state{rooms = CurrRooms} = State) ->
    case maps:find(RoomPid, CurrRooms) of
        {ok, _} ->
            Reply = chat_room:add_message(RoomPid, Author, Message);
        error -> 
            Reply = {error, room_not_found}
    end,
    {reply, Reply, State};

handle_call({get_history, RoomPid}, _From, #state{rooms = CurrRooms} = State) ->
    case maps:find(RoomPid, CurrRooms) of
        {ok, _} ->
            Messages = chat_room:get_history(RoomPid),
            Reply = {ok, Messages};
        error -> 
            Reply = {error, room_not_found}
    end,
    {reply, Reply, State}.

handle_info(_Request, State) ->
    {noreply, State}.
                                       
terminate(_Reason, _State) ->
     ok.
                        
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.